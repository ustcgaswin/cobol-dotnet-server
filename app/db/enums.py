from enum import Enum

class SourceFileType(str, Enum):
    """Supported source file types."""
    
    COBOL = "cobol"
    COPYBOOK = "copybook"
    JCL = "jcl"
    REXX = "rexx"
    CATPROC = "catproc"
    PROC = "proc"
    PLI = "pli"
    PLI_COPYBOOK = "pli_copybook"
    DCLGEN = "dclgen" 