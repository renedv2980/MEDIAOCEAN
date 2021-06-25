*          DATA SET ACWRI00    AT LEVEL 012 AS OF 02/02/11                      
*PHASE T61400A                                                                  
*INCLUDE ACCIO                                                                  
*INCLUDE ACPTACNV                                                               
*INCLUDE CONVMOS                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T61400 - ACCPAK WRITER CONTROLLER'                              
T61400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WKEND-WKST,T61400,R7,R6,RR=R2,CLEAR=YES                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=H'2000'         GRABBING 2 1000 BYTE I/O AREAS               
         LA    R9,16(R9)           NEED SPACE FOR 2 8BYTE LABELS                
         ST    R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         ST    R2,RELOC                                                         
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         USING T614FFD,RA                                                       
         SPACE 1                                                                
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT ACTION FOR GENCON               
         OI    CONSERVH+1,X'01'     SERVICE FIELD IS ALWAYS MODIFIED            
         OI    CONSERVH+6,X'80'                                                 
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         SPACE 1                                                                
         GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON                                
*        IF OFF-LINE NEED TO FREE DPG BUFFER                                    
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    XIT                                                              
*                                                                               
         ICM   R3,15,DRBFEND       GET SIZE OF GETMAIN AREA                     
         S     R3,DRBFSTRT         SAVED BUFFER START                           
*                                                                               
         LA    R4,DRBFSTRT         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  FREE CORE                                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                      THEN WE'RE THROUGH                           
         EJECT                                                                  
*              INITIALIZE SYSTEM ADDRESSES                                      
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   WRIAUTH,TWAAUTH                                                  
         MVC   USERID,TWAORIG                                                   
         MVC   AGYALPHA,TWAAGY                                                  
         MVC   AGENCY,TWAAGY                                                    
         MVI   FILTIDNO,2          PROGRAM FILTER FIELD ID 2                    
         MVI   TWANSAVE,0          OUTSMART GENCON - DON'T RESTORE              
         SPACE 1                                                                
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         MVC   ATIOB+1(3),1(R1)    P1 1-3 HAS A(TIOB)                           
         LM    R3,R4,12(R1)        A(TIA) A(COMFACS)                            
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         SPACE 1                                                                
SYS2     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS2                                                          
         MVC   ACCIO,=V(ACCIO)                                                  
         SPACE 1                                                                
         SR    R3,R3               SET UP COMMON ENTRIES                        
         LA    R4,WRICOMM                                                       
         LA    R5,NWRICOMM                                                      
         SPACE 1                                                                
SYS4     MVC   0(4,R4),ACCGEN      ALL GO TO ACCGEN                             
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         EJECT                                                                  
*              OTHER INITIALIZATION                                             
         SPACE 3                                                                
*                                  SEED SYSD WITH DUMP COMMENTS                 
         SPACE 1                                                                
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPFCIL,=C'*FACILS*'                                            
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         MVC   DUMPACIO,=C'*ACCIOD*'                                            
         MVC   DUMPDATE,=C'**DATE**'                                            
*                                                                               
         LA    R1,BUFF             MAKE BUFFER AREAS ADDRESSABLE                
         ST    R1,ADATES                                                        
         LA    R1,960(R1)                                                       
         MVC   0(8,R1),=C'*ACCFLT*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,AACCFILT                                                      
*                                                                               
         LA    R1,2048(R1)                                                      
         MVC   0(8,R1),=C'**DPG***'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADPGPROG                                                      
*                                                                               
         ST    R1,DRSTBUF                                                       
*                                                                               
         LA    R1,3000(R1)                                                      
         ST    R1,DRENDBUF                                                      
*                                                                               
         MVC   0(8,R1),=C'*DRONIO*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,DRACCIO                                                       
*                                                                               
*        IF OFF-LINE NEED TO REASSIGN DPG BUFFER TO GETMAIN                     
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    SYS6                                                             
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(20*1024)             20K BUFFER                              
*                                                                               
         LA    R4,DRBFSTRT         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,DRBFSTRT      BUFFER START ADDRESS                         
         MVC   0(8,R1),=C'***DPG**'   FLAG AREA                                 
         LA    RF,8(R1)                                                         
         ST    RF,DRSTBUF          BUFFER START ADDRESS                         
         ST    RF,ADPGPROG                                                      
*                                                                               
         LA    R1,0(R3,R1)         BUFFER END ADDRESS                           
         ST    R1,DRENDBUF                                                      
         ST    R1,DRBFEND                                                       
*                                                                               
SYS6     L     RF,=V(DUMMY)        END OF SYSTEM BASE                           
         A     RF,RELOC                                                         
         ST    RF,SYSDUMMY                                                      
         MVI   SYSTEM,C'A'         ACCOUNT                                      
         MVI   MAXIOS,2            USES 2 I/O AREAS                             
         MVC   SIZEIO,=F'1000'     EACH I/O IS 1000 BYTES                       
         MVC   GETUSER,WRIUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'42'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'49'                                                  
         MVC   SYSDIR,=C'ACCOUNT '                                              
         MVC   SYSFIL,=C'ACCOUNT '                                              
         MVC   REQFILE,=C'ACCREQ '                                              
         MVI   USEIO,C'Y'          ONLY ACCPAK DOES THIS                        
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   LWORK,=AL4(WKEND-WKST) WE TOOK XXXXX BYTES IN NMOD               
         MVC   RCPROG(2),=C'AC'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9061400'    PRESET FOR SYSTEM CALLOVS               
         LA    RF,RECACTS            RECORD/ACTION DIRECTORY                    
         ST    RF,ARECACT            RECORD/ACTION DIRECTORY                    
         LA    R1,BUFF                                                          
         ST    R1,ASTARTSV                                                      
         SPACE 1                                                                
SYSINTX  XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    X'303982'                                                        
CORES    EQU   (*-CORETAB)                                                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              TABLES OF RECORDS ACTIONS AND COMBINATIONS                       
         SPACE 3                                                                
RECACTS  DS    0D                                                               
         SPACE 1                                                                
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE 1                                                                
         DC    X'04',C'WRITER  ',AL1(01),X'0000'                                
         DC    X'04',C'TFC     ',AL1(02),X'0000'                                
         DC    X'04',C'BROWSE  ',AL1(03),X'0000'                                
         DC    X'04',C'PAYROLL ',AL1(04),X'0000'                                
         DC    X'04',C'BOX     ',AL1(05),X'0000'                                
         DC    X'04',C'INVOICE ',AL1(06),X'0000'                                
         DC    X'04',C'TEC     ',AL1(14),X'0000'                                
         SPACE 3                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 3                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
         SPACE 1                                                                
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,12),X'F101000138',C'RWRW'  WRITER   REPORT          
         DC    X'03',AL1(02,12),X'F202000238',C'FCRW'  TFC      REPORT          
         DC    X'03',AL1(03,12),X'F303000338',C'FCRW'  BROWSE   REPORT          
         DC    X'03',AL1(04,12),X'F204000438',C'FCRW'  PAYROLL  REPORT          
         DC    X'03',AL1(05,12),X'F205000538',C'FCRW'  BOX      REPORT          
         DC    X'03',AL1(06,12),X'F206000638',C'FCRW'  INVOICE  REPORT          
         DC    X'03',AL1(14,12),X'F20E000E38',C'FCRW'  TEC      REPORT          
         DC    X'FF'                                                            
         SPACE 3                                                                
*              PHASE USED UP BY SYSTEM SO FAR                                   
         SPACE 1                                                                
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     Y  Y  Y  Y  Y  Y  Y                       Y                     
*        FX        Y  Y                                      Y                  
         EJECT                                                                  
*              DSECT TO DEFINE WORKING STORAGE                                  
WKLND    DSECT                                                                  
WKST     DS    CL(SPOOLEND-SPOOLD)                                              
         DS    CL((GENDEND+16)-GEND)                                            
         DS    CL(SYSEND-WRISYS)                                                
WKEND    EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE ACWRIWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACWRIFFD                                                                       
*DDGENTWA                                                                       
*ACGENBOTH                                                                      
*CTGENFILE                                                                      
*FAFACTS                                                                        
*FATIOB                                                                         
*DDCOMFACS                                                                      
*DRGLOBAL                                                                       
*DRDICFILE                                                                      
*DDBIGBOX                                                                       
*DDWIDED                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
*********INCLUDE DRDICFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACWRI00   02/02/11'                                      
         END                                                                    
