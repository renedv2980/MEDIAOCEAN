*          DATA SET ACLDXFQA   AT LEVEL 006 AS OF 05/26/15                      
*PHASE ACLDFQAA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE XSORT                                                                  
*INCLUDE HEXIN                                                                  
ACLDXFQA TITLE 'Dump or load ACCT - Companies in a list only'                   
         PRINT NOGEN                                                            
***********************************************************************         
*   Requires UPSI switches to be set                                  *         
***********************************************************************         
*   DUMP                                                              *         
*     UPSI = X'80' - Dump MQ and MA from ACCT to tape                 *         
*     UPSI = X'20' - DROP AGENCIES IN PARAM= AND KEEP THE REST.       *         
***********************************************************************         
*   LOAD - concatednated load with normal dump tape                   *         
*     UPSI = X'08' - Load MQ and MA back to ACCT                      *         
***********************************************************************         
         USING WORKC,RC            RC=A(WORK AREA)                              
         USING DPRINT,RA           RA=A(PRINT AREA)                             
         USING COMFACSD,COMFACS                                                 
ACLDXFQA CSECT                                                                  
         NMOD1 0,**AFQA**                                                       
         L     RC,AWORKC                                                        
         ST    R1,APARM                                                         
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,PACPRINT                                                      
                                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
EXIT     XIT1                                                                   
                                                                                
AWORKC   DC    A(WORKC)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         USING LDDEFND,RF                                                       
         USING ISDTF,R2                                                         
DMXINIT  L     RF,PALDEFN                                                       
         L     R2,LDDDTFIS                                                      
         MVI   ACCRECS,NO                                                       
         MVI   DUMP,NO             Both                                         
         MVI   LOAD,NO                                                          
         ZAP   PKZERO,=P'0'                                                     
         ZAP   COUNT1,PKZERO                                                    
         ZAP   COUNT2,PKZERO                                                    
         MVC   CDATAMGR,LDATAMGR                                                
         MVC   CDATCON,LDATCON                                                  
         MVC   CADDAY,LADDAY                                                    
         MVC   CXSORT,LADDAY                                                    
         MVC   CHEXOUT,LHEXOUT                                                  
         MVC   CHELLO,LHELLO                                                    
         MVC   CSCANNER,LSCANNER                                                
         MVC   CXSORT,=V(XSORT)                                                 
         MVC   CGETPROF,=V(GETPROF)                                             
         L     RE,LUPSIVAL         A(UPSI)                                      
         MVC   UPSI,0(RE)                                                       
         DROP  RF                                                               
                                                                                
         L     R4,PAPARMC          Parm Card                                    
         GOTOR CSCANNER,DMCB,(C'C',(R4)),('MAXCPYS',ABLOCK),0                   
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING SCANBLKD,R3                                                      
         STC   R0,#OFCPYS          Number of hex company codes                  
         L     R3,ABLOCK                                                        
         LA    R6,CPYLIST                                                       
DMXINT10 GOTOR VHEXIN,DMCB,SC1STFLD,(R6),2,0                                    
         LA    R3,SCBLKLQ(,R3)                                                  
         AHI   R6,1                                                             
         BRCT  R0,DMXINT10                                                      
         DROP  R3                                                               
                                                                                
         MVI   LOAD,NO                                                          
         MVI   DUMP,YES            Dump                                         
         TM    UPSI,UPSIDUMP                                                    
         BO    DMINTXIT                                                         
         MVI   LOAD,YES            Load                                         
         MVI   DUMP,NO             Dump                                         
         TM    UPSI,UPSILOAD                                                    
         BO    DMINTXIT                                                         
         DC    H'00'                                                            
                                                                                
DMINTXIT J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECORD                                                    *         
***********************************************************************         
         USING ACCRECD,R2                                                       
DMXREC   DS    0H                                                               
         SR    R6,R6                                                            
         TM    PLIST+4,X'10'                                                    
         BO    DMREC010                                                         
         L     R2,PADAREC                                                       
         B     DMREC020                                                         
                                                                                
DMREC010 L     R2,PADSISP                                                       
         CLI   0(R2),PLDKTYPQ      X'18'                                        
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
DMREC020 GOTOR VRECTYPE,DMCB,(C'D',ACCRECD)                                     
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
         EJECT ,                                                                
***********************************************************************         
* Dump                                                                          
***********************************************************************         
         LA    R3,CPYLIST                                                       
         LLC   R0,#OFCPYS                                                       
         CLI   DUMP,YES                                                         
         BNE   LOAD100             No must be load                              
         TM    UPSI,UPSIDROP       Are we dropping agencies in param?           
         BO    DUMP120                                                          
*                                                                               
DUMP110  CLC   COMPANY,0(R3)       Check list of hex companies                  
         BE    DMXKEEP                                                          
         AHI   R3,1                                                             
         BCT   R0,DUMP110                                                       
         B     DMXPURGE            Not in list so purge                         
         SPACE 2                                                                
*                                                                               
DUMP120  CLC   COMPANY,0(R3)       Check list of hex companies                  
         BE    DMXPURGE                                                         
         AHI   R3,1                                                             
         BCT   R0,DUMP120                                                       
         B     DMXKEEP             Not in list so keep.                         
         EJECT ,                                                                
***********************************************************************         
* Load                                                                          
***********************************************************************         
LOAD100  CLI   LOAD,YES                                                         
         BE    *+6                                                              
         DC    H'00'               Missing UPSI switches                        
                                                                                
         CLI   RECTYPE,ACRTTRLA    Trailer ?                                    
         BNE   *+8                 No                                           
         MVI   RESUME,YES                                                       
         CLI   RESUME,YES          Drop until resume=YES                        
         BE    DMXKEEP                                                          
LOAD110  CLC   COMPANY,0(R3)       Match on list of companies                   
         BER   RE                  Drop 1st tape, keep from 2nd tape            
         AHI   R3,1                                                             
         BCT   R0,LOAD110                                                       
         BR    RF                  Keep 1st tape, drop from 2nd tape            
         EJECT                                                                  
***********************************************************************         
* END OF INPUT FILE                                                   *         
***********************************************************************         
DMXEOF   DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
                                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         J     EXIT                                                             
                                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         J     EXIT                                                             
                                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         J     EXIT                                                             
                                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         J     EXIT                                                             
                                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         J     EXIT                                                             
         EJECT ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT ,                                                                
***********************************************************************         
* Table                                                                         
***********************************************************************         
BLOCK    DS    (MAXCPYS)CL(SCBLKLQ)                                             
         EJECT ,                                                                
***********************************************************************         
* Working storage area within code                                              
***********************************************************************         
WORKC    CSECT                                                                  
                                                                                
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
EOR      EQU   0                                                                
MAXMTHD  EQU   9                                                                
MAXYEARS EQU   4                                                                
MAXGL    EQU   8                                                                
MAXMEDS  EQU   36                                                               
K        EQU   1024                                                             
MAXDUMP  DC    P'50'                                                            
MAXCPYS  EQU   20                                                               
                                                                                
VRECTYPE DC    V(ACRECTYP)                                                      
VPRNTBL  DC    V(PRNTBL)                                                        
VPRINT   DC    V(PRINT)                                                         
VHEXIN   DC    V(HEXIN)                                                         
ABLOCK   DC    A(BLOCK)                                                         
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMGETR   DC    C'GETREC  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
                                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
WORK     DS    XL256                                                            
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
PKZERO   DS    PL1                                                              
                                                                                
APARM    DS    A                                                                
                                                                                
AFSTREC  DS    0A                                                               
ACPYREC  DS    A                                                                
ALDGREC  DS    A                                                                
ALVAREC  DS    A                                                                
ALVBREC  DS    A                                                                
ALVCREC  DS    A                                                                
ALVDREC  DS    A                                                                
#RECS    EQU   ((*-AFSTREC)/L'AFSTREC)                                          
                                                                                
COMFACS  DS    24A                                                              
                                                                                
PLIST    DS    0X                                                               
PADAREC  DS    A                                                                
PATAPEO  DS    A                                                                
PAPARMC  DS    A                                                                
PALDEFN  DS    A                                                                
PAPRINT  DS    A                                                                
PACPRINT DS    A                                                                
         DS    2A                                                               
PADSISP  DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
                                                                                
SVRE     DS    A                                                                
SVRF     DS    A                                                                
                                                                                
COUNT1   DS    PL3                                                              
COUNT2   DS    PL3                                                              
                                                                                
RECTYPE  DS    X                   EXTRACTED RECORD TYPE                        
COMPANY  DS    X                                                                
DROP     DS    X                   Company to drop in load 1st round            
KEEP     DS    X                   Company to keep in dump                      
#OFCPYS  DS    X                   Max 20                                       
                                                                                
DUMP     DS    C                   Yes/No dumping file                          
LOAD     DS    C                   Yes/No loading file                          
RESUME   DS    C                   Yes/No - resume keeping all records          
ACCRECS  DS    C                   Yes/No - Account records processed           
                                                                                
UPSI     DS    XL1                                                              
UPSIDUMP EQU   X'80'               .   Dump list of companies                   
UPSILOAD EQU   X'40'               .   Load list of companies                   
UPSIDROP EQU   X'20'               .   DROP AGENCY IN PARAM= AND KEEP           
*                                  .   the rest.                                
                                                                                
CPYLIST  DC    21XL2'00'                                                        
                                                                                
TAPECNT  DC    PL8'0'                                                           
                                                                                
TAPEHDR  DS    0H                  ** OUTPUT RECORD BUILT HERE **               
TAPELEN  DS    H                                                                
         DC    H'0'                                                             
TAPEREC  DS    XL4000                                                           
                                                                                
         EJECT ,                                                                
***********************************************************************         
* ++INCLUDES                                                                    
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
*  ACGLPOSTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGLPOSTD                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACLDXFQA  05/26/15'                                      
         END                                                                    
