"""Database enums."""

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
    PLI_COPYBOOK = "inc"
    DCLGEN = "dclgen" 
    CA7  = "ca7"
    PARMLIB = "ctl"
    ASSEMBLY = "assembly"
