*          DATA SET DDCCDSP9   AT LEVEL 254 AS OF 07/17/07                      
*PHASE CCDSP9A                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE LOCKSPC                                                                
*INCLUDE ARREDIT                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*                                                                               
         TITLE 'EXTRACT PHASE CSECT LEVELS'                                     
         SPACE 1                                                                
CCDSP9   CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CCLI**,=A(WORKAREA),RA,R9,R8,R7                    
         USING WORKD,RC                                                         
         USING LIBUFFD,LIBUFF      DATASPACE     ARREDIT BLOCK                  
         USING PANRECD,IO                                                       
*                                                                               
         OPEN  (PANOUT,OUTPUT)   OPEN ALL FILES                                 
*                                                                               
         MVI   SSB+SSODSPAC-SSOOFF,C'S'                                         
*                                                                               
         BAS   RE,INIT                                                          
         BAS   RE,LOADDSP          GET THE DATASPACE                            
*                                                                               
         GOTO1 =V(LOCKSPC),DMCB,X'20004001',WORK                                
*                                                                               
         LAM   R2,R2,ALET          COPY BUFFER DETAIL FOR ARREDIT               
         SAC   512                                                              
         SAM31                                                                  
         LA    R1,WORK                                                          
         L     R2,DSPTFRST-DSPHDR(R1)                                           
         N     R2,=X'3FFFFFFF'                                                  
         MVC   LIBUFF,0(R2)                                                     
         SAC   0                                                                
         SAM24                                                                  
*                                                                               
         MVC   LIALET,ALET                                                      
         LA    R1,WORK                                                          
         MVC   WORK(3),=X'000A00'                                               
         ST    R1,LIAREC                                                        
         MVI   LIACTN,LIAHIGH                                                   
MAIN020  MVC   LIKEYL,=AL2(PSKEYL)                                              
         OI    LIFLAG1,LIF1ARS                                                  
         GOTO1 =V(ARREDIT),DMCB,LIBUFF                                          
         CLI   LIRTN,LIROK                                                      
         BNE   MAIN990                                                          
*                                                                               
         L     R4,LIAREC                                                        
         USING PROGSPCD,R4                                                      
         CLC   PSADR,=X'FFFFFFFF'  SKIP THESE                                   
         BE    MAIN050                                                          
         CLC   PSADR,=X'EEEEEEEE'                                               
         BE    MAIN050                                                          
*                                                                               
         ICM   R2,15,PSADR                                                      
         ICM   R5,15,PSLEN                                                      
         BZ    MAIN050                                                          
*                                                                               
         LAM   R2,R2,ALET                                                       
MAIN030  MVC   IO(100),SPACES                                                   
         GOTO1 =V(HEXOUT),DMCB,PSNAME,PANTEST,3                                 
*                                                                               
         MVI   PANTEST,C'T'                                                     
         CLI   PSLANG,0                                                         
         BE    *+10                                                             
         MVC   PANTEST(1),PSLANG                                                
*                                                                               
         CLI   PSLVL,0                                                          
         BE    *+10                                                             
         MVC   PANTEST+6(1),PSLVL                                               
*                                                                               
         SAC   512                                                              
         SAM31                                                                  
MAIN035  CLC   0(5,R2),=C'BOOK='                                                
         BNE   *+14                                                             
         CLC   16(6,R2),=C'LEVEL='                                              
         BE    MAIN040                                                          
         LA    R2,1(R2)                                                         
         BCT   R5,MAIN035                                                       
         B     MAIN050                                                          
*                                                                               
MAIN040  MVC   PANBK(10),5(R2)                                                  
*&&UK*&& MVC   PANLIB(2),=C'UK'                                                 
*&&US*&& MVC   PANLIB(2),=C'US'                                                 
         MVC   PANLIB+2(4),DSPACE                                               
         MVC   PANLEV(3),22(R2)                                                 
*                                                                               
*&&UK*&& CLC   PANLIB+2(4),=C'PGMC'                                             
*&&UK*&& BNE   *+10                                                             
*&&UK*&& MVC   PANLIB+2(4),=C'PGMF'                                             
*                                                                               
         MVC   DUB,31(R2)          DATE= FROM 31(R2)                            
         SAC   0                                                                
         SAM24                                                                  
*                                                                               
         CLI   DUB+2,C'/'          TEST JAN01/06 FORMAT                         
         BE    MAIN043                                                          
*                                                                               
         BAS   RE,DATCONV2                                                      
*                                                                               
MAIN043  CLC   DUB(2),=C'**'                                                    
         BE    MAIN050                                                          
*&&US                                                                           
         MVC   HALF,DUB+0                                                       
         MVC   DUB+0(2),DUB+3                                                   
         MVC   DUB+3(2),HALF                                                    
*&&                                                                             
         MVC   PANDAT1+0(2),DUB+6                                               
         MVC   PANDAT1+2(2),DUB+3                                               
         MVC   PANDAT1+4(2),DUB+0                                               
         GOTO1 =V(DATCON),DMCB,(0,PANDAT1),(11,PANDATE)                         
*                                                                               
         MVC   IOL,=X'00620000'    PUT RECORD TO PANOUT                         
         PUT   PANOUT,IOL                                                       
         SAM31                                                                  
         LA    R2,1(R2)                                                         
         SAM24                                                                  
         BCT   R5,MAIN030                                                       
*                                                                               
MAIN050  MVI   LIACTN,LIASEQ                                                    
         B     MAIN020                                                          
*                                                                               
MAIN990  CLOSE PANOUT                                                           
XBASE    XBASE                                                                  
         EJECT                                                                  
*************************************************************                   
*        INIT - READ CARDS ETC...                           *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
         OPEN  (SYSIN,INPUT)       OPEN SYSIN CARDS                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT010  GET   SYSIN                                                            
         CLC   0(7,R1),=C'DSPACE='                                              
         BNE   INIT010                                                          
         MVC   DSPACE,7(R1)                                                     
*                                                                               
CARDEND  EQU   *                                                                
*                                                                               
INITX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        LOCATE DATASPACE                                             *         
***********************************************************************         
         SPACE 1                                                                
LOADDSP  NTR1  ,                                                                
*                                                                               
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),=C'PGMSXXXXXXXX'                                      
         MVC   WORK+4(4),DSPACE                                                 
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
*        MVC   SSB+SSOPGMTO-SSOOFF(4),DMOFFS                                    
         MVC   ALET,WORK+24                                                     
         MVC   SSB+SSOPGMTA-SSOOFF(4),ALET                                      
         MVC   SSB+SSOTBLET-SSOOFF(4),ALET  <-- BUG IN LOCKSPC                  
         OC    ALET,ALET                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        CONVERT DATES  DUB=FEB01/06 TO DUB=01/02/06                  *         
***********************************************************************         
         SPACE 1                                                                
DATCONV2 NTR1                                                                   
         LA    R1,MONTAB+3                                                      
         LA    R0,1                                                             
DC2010   CLC   DUB(3),0(R1)        MATCH MONTH                                  
         BE    DC2020                                                           
*                                                                               
         LA    R1,3(R1)                                                         
         AHI   R0,1                                                             
         CLI   0(R1),C'*'          EOT                                          
         BNE   DC2010                                                           
         B     DATERR                                                           
*                                                                               
DC2020   CVD   R0,DUB1             0000000000000012                             
         UNPK  FULL,DUB1                                                        
         L     R1,FULL                                                          
         O     R1,=X'F0F0F0F0'                                                  
         STCM  R1,3,DUB                                                         
         MVI   DUB+2,C'/'                                                       
*                                                                               
*&&UK*&& MVC   FULL(2),DUB         REVERSE UK DATE TO MATCH NY                  
*&&UK*&& MVC   DUB(2),DUB+3                                                     
*&&UK*&& MVC   DUB+3(2),FULL                                                    
         B     EXIT                                                             
*                                                                               
DATERR   MVC   DUB(8),=C'********'                                              
         B     EXIT                                                             
MONTAB   DC    C'***JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC***'                    
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LTORG                                          *         
***********************************************************************         
         SPACE 1                                                                
PANOUT   DCB   DDNAME=PANOUT,DSORG=PS,MACRF=(PM),RECFM=VB,             X        
               BLKSIZE=8200,LRECL=4096,BUFNO=2                                  
*                                                                               
SYSIN    DCB   DSORG=PS,MACRF=GL,DDNAME=SYSIN,EODAD=CARDEND                     
*                                                                               
         DS    0D                                                               
LIBUFF   DS    XL(LIBUFFL)         DATASPACE ARREDIT PARAMETERS                 
*                                                                               
SPACES   DC    100C' '                                                          
         LTORG                                                                  
         EJECT                                                                  
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    H'0'                                                             
         DC    X'FF'                                                            
         DC    X'02'               SUPPRESS RECOVERY                            
         DC    1020X'00'                                                        
         EJECT                                                                  
***********************************************************************         
*        WORK AREA                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'WORKAREAWORKAREA'                                           
WORKAREA DC    100000X'00'                                                      
*                                                                               
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
DMOFFS   DS    A                                                                
ALET     DS    A                                                                
*                                                                               
DSPACE   DS    CL4                                                              
*                                                                               
WORK     DS    CL132                                                            
*                                                                               
IOL      DS    XL4                                                              
IO       DS    CL4096                                                           
WORKX    EQU   *                                                                
*                                                                               
************************************************************                    
*        PAN DSECT                                         *                    
************************************************************                    
         SPACE 1                                                                
PANRECD  DSECT                                                                  
PANBK    DS    CL10                BOOK NAME DDBIGBOOK                          
         DS    CL1                                                              
PANLEV   DS    CL3                 LEVEL 001                                    
         DS    CL1                                                              
PANDAT1  DS    CL7                 DAT1 YYMMDD                                  
         DS    CL1                                                              
PANDATE  DS    CL8                 DATE                                         
         DS    CL1                                                              
PANLANG  DS    CL5                 LANGUAGE ASMB                                
         DS    CL1                                                              
PANLIB   DS    CL8                 LIBRARY UKRMOR UKAPPL UKNYAPPL               
         DS    CL1                                                              
PANTEST  DS    CL8                 TEST PHASE T11000A                           
         DS    CL1                                                              
PANLIVE  DS    CL8                 LIVE PHASE T11000                            
         DS    CL1                                                              
PANRMTST DS    CL8                 RM BOOK TEST FAMNTRA                         
         DS    CL1                                                              
PANRMLIV DS    CL8                 RM BOOK LIVE FAMNTR                          
         DS    CL1                                                              
PANVAR   DS    0CL1                CSECT AND INCLUDE INFO                       
*                                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FAPROGSPCD                                                     
       ++INCLUDE DDARREDITD                                                     
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'254DDCCDSP9  07/17/07'                                      
         END                                                                    
