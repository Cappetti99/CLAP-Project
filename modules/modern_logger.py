"""
Modern Logging System for CLAP Project
Single Responsibility: Structured logging with performance tracking and error analysis
"""

import os
import json
import logging
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, Optional, Any, List
from dataclasses import dataclass, asdict
from enum import Enum


class LogLevel(Enum):
    """Logging levels for CLAP operations"""
    DEBUG = "DEBUG"
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    CRITICAL = "CRITICAL"


@dataclass
class ExecutionMetrics:
    """Metrics for code execution tracking"""
    language: str
    task_name: str
    execution_time: float
    memory_usage: Optional[float] = None
    success: bool = True
    error_type: Optional[str] = None
    timestamp: str = ""
    
    def __post_init__(self):
        if not self.timestamp:
            self.timestamp = datetime.now().isoformat()


@dataclass
class ErrorInfo:
    """Structured error information"""
    language: str
    task_name: str
    error_type: str
    error_message: str
    file_path: str
    code_snippet: str = ""
    timestamp: str = ""
    suggested_fix: Optional[str] = None
    
    def __post_init__(self):
        if not self.timestamp:
            self.timestamp = datetime.now().isoformat()


class ModernLogger:
    """
    Single Responsibility: Centralized logging and metrics collection for CLAP
    Provides structured logging, error tracking, and performance metrics
    """
    
    def __init__(self, log_dir: str = "results/logs", session_id: Optional[str] = None):
        self.log_dir = Path(log_dir)
        self.log_dir.mkdir(parents=True, exist_ok=True)
        
        self.session_id = session_id or datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Initialize logging files
        self.execution_log = self.log_dir / f"execution_{self.session_id}.log"
        self.error_log = self.log_dir / f"errors_{self.session_id}.json"
        self.metrics_log = self.log_dir / f"metrics_{self.session_id}.json"
        
        # Setup Python logger
        self.logger = self._setup_logger()
        
        # Runtime tracking
        self.session_start = time.time()
        self.execution_metrics: List[ExecutionMetrics] = []
        self.error_records: List[ErrorInfo] = []
        
    def _setup_logger(self) -> logging.Logger:
        """Setup structured Python logger"""
        logger = logging.getLogger(f"swam_{self.session_id}")
        logger.setLevel(logging.INFO)
        
        # Clear existing handlers
        logger.handlers.clear()
        
        # File handler
        file_handler = logging.FileHandler(self.execution_log)
        file_handler.setLevel(logging.INFO)
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.INFO)
        
        # Formatter
        formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)
        
        logger.addHandler(file_handler)
        logger.addHandler(console_handler)
        
        return logger
    
    def info(self, message: str, **kwargs):
        """Log info message with optional context"""
        self.logger.info(message, extra=kwargs)
    
    def warning(self, message: str, **kwargs):
        """Log warning message with optional context"""
        self.logger.warning(message, extra=kwargs)
    
    def error(self, message: str, **kwargs):
        """Log error message with optional context"""
        self.logger.error(message, extra=kwargs)
    
    def debug(self, message: str, **kwargs):
        """Log debug message with optional context"""
        self.logger.debug(message, extra=kwargs)
    
    def log_execution_start(self, language: str, task_name: str) -> str:
        """Start tracking execution of a task"""
        execution_id = f"{language}_{task_name}_{int(time.time())}"
        self.info(f"🚀 Starting execution: {language} - {task_name}", 
                  execution_id=execution_id)
        return execution_id
    
    def log_execution_end(self, language: str, task_name: str, 
                         execution_time: float, success: bool = True,
                         error_type: Optional[str] = None):
        """End tracking execution and record metrics"""
        metrics = ExecutionMetrics(
            language=language,
            task_name=task_name,
            execution_time=execution_time,
            success=success,
            error_type=error_type
        )
        
        self.execution_metrics.append(metrics)
        
        status = "✅ SUCCESS" if success else "❌ FAILED"
        self.info(f"{status}: {language} - {task_name} ({execution_time:.3f}s)",
                  execution_time=execution_time, success=success)
        
        # Save metrics periodically
        if len(self.execution_metrics) % 10 == 0:
            self._save_metrics()
    
    def log_error(self, language: str, task_name: str, error_type: str,
                  error_message: str, file_path: str = "", 
                  code_snippet: str = "", suggested_fix: Optional[str] = None):
        """Log structured error information"""
        error_info = ErrorInfo(
            language=language,
            task_name=task_name,
            error_type=error_type,
            error_message=error_message,
            file_path=file_path,
            code_snippet=code_snippet[:200] if code_snippet else "",
            suggested_fix=suggested_fix
        )
        
        self.error_records.append(error_info)
        
        self.error(f" {error_type} in {language}: {error_message}",
                   language=language, task_name=task_name, error_type=error_type)
        
        # Save errors immediately
        self._save_errors()
    
    def log_dependency_issue(self, language: str, missing_deps: List[str],
                           suggested_commands: List[str]):
        """Log dependency-related issues with suggestions"""
        deps_str = ", ".join(missing_deps)
        commands_str = "; ".join(suggested_commands)
        
        self.warning(f" Missing dependencies for {language}: {deps_str}")
        self.info(f" Suggested fixes: {commands_str}")
    
    def log_performance_warning(self, language: str, task_name: str,
                               execution_time: float, threshold: float = 30.0):
        """Log performance warnings for slow executions"""
        if execution_time > threshold:
            self.warning(f" Slow execution: {language} - {task_name} "
                        f"took {execution_time:.3f}s (threshold: {threshold}s)")
    
    def get_session_summary(self) -> Dict[str, Any]:
        """Get comprehensive session summary"""
        total_executions = len(self.execution_metrics)
        successful_executions = sum(1 for m in self.execution_metrics if m.success)
        failed_executions = total_executions - successful_executions
        
        total_errors = len(self.error_records)
        error_types = {}
        for error in self.error_records:
            error_types[error.error_type] = error_types.get(error.error_type, 0) + 1
        
        languages_used = set(m.language for m in self.execution_metrics)
        
        session_duration = time.time() - self.session_start
        
        summary = {
            "session_id": self.session_id,
            "session_duration": session_duration,
            "total_executions": total_executions,
            "successful_executions": successful_executions,
            "failed_executions": failed_executions,
            "success_rate": successful_executions / total_executions if total_executions > 0 else 0,
            "total_errors": total_errors,
            "error_types": error_types,
            "languages_used": list(languages_used),
            "avg_execution_time": sum(m.execution_time for m in self.execution_metrics) / total_executions if total_executions > 0 else 0
        }
        
        return summary
    
    def _save_metrics(self):
        """Save execution metrics to file"""
        try:
            metrics_data = [asdict(m) for m in self.execution_metrics]
            with open(self.metrics_log, 'w') as f:
                json.dump(metrics_data, f, indent=2)
        except Exception as e:
            self.error(f"Failed to save metrics: {e}")
    
    def _save_errors(self):
        """Save error records to file"""
        try:
            error_data = [asdict(e) for e in self.error_records]
            with open(self.error_log, 'w') as f:
                json.dump(error_data, f, indent=2)
        except Exception as e:
            print(f"Failed to save errors: {e}")
    
    def finalize_session(self):
        """Finalize logging session and save all data"""
        self._save_metrics()
        self._save_errors()
        
        summary = self.get_session_summary()
        
        # Save session summary
        summary_file = self.log_dir / f"session_summary_{self.session_id}.json"
        with open(summary_file, 'w') as f:
            json.dump(summary, f, indent=2)
        
        self.info(f" Session {self.session_id} completed: "
                 f"{summary['successful_executions']}/{summary['total_executions']} "
                 f"successful ({summary['success_rate']:.1%})")
        
        return summary


# Global logger instance - can be used across modules
_global_logger: Optional[ModernLogger] = None

def get_logger(log_dir: str = "results/logs", session_id: Optional[str] = None) -> ModernLogger:
    """Get or create global logger instance"""
    global _global_logger
    if _global_logger is None:
        _global_logger = ModernLogger(log_dir, session_id)
    return _global_logger

def setup_logger(log_dir: str = "results/logs", session_id: Optional[str] = None) -> ModernLogger:
    """Setup and return new logger instance"""
    return ModernLogger(log_dir, session_id)