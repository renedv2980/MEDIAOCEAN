*          DATA SET NEWRI15    AT LEVEL 038 AS OF 05/01/02                      
*PHASE T32015A                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE NETACC                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T32015 - N4  REPORT  PHASE'                                     
T32015   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NW15**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1         ANETWS1=WORKING STORAGE                       
         USING MYD,R7                                                           
         MVC   AACCBLK,ANETWS2    ACCBLOCK IN WS2                               
         MVC   NBACLI,ANETWS3     CLIENT RECORD IN WS3                          
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
         MVC   MYOPTS+10,NDCLIRMD                                               
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         MVC   MYOPTS+11,NDPRGRMD                                               
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
         MVC   MYOPTS+13,NDESTRMD                                               
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPT,DMCB                                                       
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPAKH                PACKAGE                                
         NETGO NVPAKLOK,DMCB                                                    
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH               START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH               END DATE                               
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLADJH          PERCENT ADJUSTMENT                           
         NETGO NVGETFLD,DMCB                                                    
         BNZ   ADJ5                                                             
         MVC   FLD(4),=C'99.3'    DEFAULT=99.3                                  
         MVI   MYBYTE,2                                                         
         LA    R4,4                                                             
         B     CASH                                                             
ADJ5     LR    R4,R1               R1=LENGTH OF INPUT                           
         LR    RE,R4                                                            
         MVI   MYBYTE,0                                                         
         SR    R1,R1                                                            
         LA    R3,FLD                                                           
         AR    R3,R4                                                            
         BCTR  R3,0                POINT R3 AT END OF FLD                       
LOOPIT   CLI   0(R3),X'4B'         IS IT DECIMAL POINT                          
         BE    CASH                                                             
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
         BCTR  R3,0                                                             
         BCT   RE,LOOPIT                                                        
CASH     CLI   MYBYTE,2                                                         
         BNL   *+8                                                              
         MVI   MYBYTE,2                                                         
         PRINT GEN                                                              
         GOTO1 =V(CASHVAL),DMCB,(MYBYTE,FLD),(R4),RR=RELO                       
         PRINT NOGEN                                                            
         CLI   0(R1),X'FF'                                                      
         BE    EDINV                                                            
         MVC   NDADJPCT,4(R1)      SET PERCENTAGE                               
         MVC   NDADJPCT(1),MYBYTE    SET NUMBER OF DECIMALS                     
         SPACE                                                                  
*                                  DIG OUT ANY OPTIONS                          
         LA    R2,SPLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
EDT16    CLC   12(4,R3),=C'DOWN'   OPTION TO DOWNLOAD                           
         BNE   OPT3                                                             
         MVI   DOWNOPT,C'Y'                                                     
         B     EDT18                                                            
OPT3     MVI   PEROPT,0                                                         
         CLC   12(3,R3),=C'PER'    PERIOD OPTION                                
         BNE   EDT18                                                            
         MVC   PEROPT,22(R3)                                                    
         CLI   PEROPT,C'C'         CAN BE CALENDAR                              
         BE    EDT18                                                            
         CLI   PEROPT,C'B'         BROADCAST                                    
         BE    EDT18                                                            
         CLI   PEROPT,C'S'         OR SPECIAL                                   
         BE    EDT18                                                            
         B     EDINV               OTHERWISE ITS NO GOOD                        
         SPACE 1                                                                
EDT18    LA    R3,32(R3)                                                        
         BCT   R0,EDT16                                                         
         SPACE 1                                                                
EDT20    LA    R2,SPLTITLH                                                      
         MVC   NDTITLE,SPACES                                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT22                                                            
         MVC   NDTITLE,FLD                                                      
         SPACE 1                                                                
EDT22    LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         PRINT GEN                                                              
EDERR    GOTO1 ERREX                                                            
         PRINT NOGEN                                                            
         EJECT                                                                  
*              REPORT INITIALIZATION                                            
         SPACE 3                                                                
REPMOD   NTR1                                                                   
*                                         INITIALIZATION FOR DRIVER             
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9031EB7'  LOAD T31EB7 (GLOBAL STORAGE)          
         L     R6,DMCB                                                          
         USING GLOBALD,R6                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'  LOAD T00A3A (DRIVER)                  
         MVC   DRIVER,DMCB                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A41'  LOAD T00A41 (NETWORK DRIVER)          
         MVC   GLASYSDR,DMCB                                                    
         GOTO1 CALLOV,DMCB,0,X'D9032005'  LOAD T32005 (DPG PHASE)               
         MVC   GLAPROG,DMCB                                                     
         SPACE 1                                                                
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
         MVI   GLFHEADL,10                                                      
         MVI   GLGAP,2                                                          
         EJECT                                                                  
*              NOW CONTROL NETIO                                                
         SPACE 3                                                                
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         MVC   NBPEROVR,PEROPT     OPTIONAL PROFILE OVERRIDE                    
         SPACE 1                                                                
PROCDATE NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBVALDAT                                                  
         BNE   PROCDATE                                                         
         MVC   NUMMONS,=F'3'      SET UP FOR DATE LIST                          
         MVI   PERTYPE,C'M'                                                     
         MVI   PERTYPE+1,1                                                      
         MVI   PERTYPE+2,1                                                      
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         LA    R1,MONLIST                                                       
         ST    R1,APERLST1                                                      
         SPACE 1                                                                
         MVC   GLOPTS,MYOPTS       MOVE MYOPTS DATA FROM EDIT TO GLOPTS         
         SPACE 1                                                                
         CLI   DOWNOPT,C'Y'        OPTION TO DOWNLOAD                           
         BNE   *+8                                                              
         MVI   GLDOWNLD,X'80'                                                   
         SPACE 1                                                                
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R6)                                                 
         EJECT                                                                  
*              INPUT - PROGRAM I/O                                              
         SPACE 3                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    GOTONE                                                           
         CLI   NBMODE,NBREQLST                                                  
         BE    ALLDONE                                                          
         CLI   NBERROR,0                                                        
         BE    GETUNIT                                                          
         DC    H'0'                                                             
*                                                                               
GOTONE   OC    NBPROGNM,=16XL1'40'    REPLACE BINARY ZEROES W/ SPACES           
*                                     FOR SORTING BY PROG NAME                  
         SPACE 1                                                                
         LA    R2,MONLIST                                                       
         LA    R1,1                                                             
         LA    R0,3                 MAXIMUM=3 MONTHS                            
         SPACE 1                                                                
MON2     CLC   NBACTDAT,0(R2)      LOOK UP MONTH NUMBER                         
         BL    MON4                                                             
         CLC   NBACTDAT,2(R2)                                                   
         BH    MON4                                                             
         STC   R1,GLDATENO         PASS THAT THROUGH TO DRIVER                  
         MVI   DRIVLOOP,1                                                       
         MVI   NDSFTARG,1          SET ASSIGNED DOLLS IN SFTARG                 
         B     GOTIN                                                            
         SPACE 1                                                                
MON4     LA    R2,R4(R2)                                                        
         LA    R1,1(R1)                                                         
         BCT   R0,MON2                                                          
         MVI   GLDATENO,0                                                       
         B     GETUNIT                                                          
         SPACE 1                                                                
*                                                                               
GOTIN    DS    0H                  PASS REC TO DRIVER 4 TIMES                   
         MVI   GLMODE,GLINPUT                                                   
GTIN5    GOTO1 DRIVER,DMCB,(R6)                                                 
         ZIC   R1,DRIVLOOP                                                      
         LA    R1,1(R1)                                                         
         STC   R1,DRIVLOOP                                                      
         CH    R1,=H'3'            IS IT 3 PASSES                               
         BH    GETUNIT                                                          
         CLI   DRIVLOOP,2                                                       
         BNE   *+12                                                             
         MVI   NDSFTARG,6          DIFF(ACT-ASS)                                
         B     GOTIN                                                            
         CLI   DRIVLOOP,3                                                       
         BNE   GETUNIT                                                          
         MVI   NDSFTARG,3          ACTUAL                                       
         B     GOTIN                                                            
         SPACE 1                                                                
*                                  GO AND DO THE OUTPUT                         
ALLDONE  MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
*                                                                               
HOOK     NTR1                              HEAD HOOK                            
*                                                                               
         CLI   GLHOOK,GLHEAD              HEADLINES                             
         BNE   HK10                                                             
         NETGO NVHEAD,DMCB                                                      
         B     HKXIT                                                            
*                                                                               
HK10     CLI   GLHOOK,GLRESOLV            RESOLVE ADDRESSES                     
         BNE   HK20                                                             
         LA    R2,TABLE                                                         
LBLOOP   CLC   GLLABEL,0(R2)                                                    
         BE    SETADD                                                           
         CLI   0(R2),X'FF'                                                      
         BE    HKXIT                                                            
         LA    R2,12(R2)                                                        
         B     LBLOOP                                                           
SETADD   MVC   GLAROUT,8(R2)                                                    
         B     HKXIT                                                            
*                                                                               
HK20     CLI   GLHOOK,GLROUT       INPUT ROUTINES                               
         BNE   HKXIT                                                            
         CLI   GLMODE,GLINPUT                                                   
         BNE   HKXIT                                                            
         LA    R2,TABLE                                                         
HK23     CLC   GLLABEL,0(R2)                                                    
         BE    HK25                                                             
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BE    HKXIT                                                            
         B     HK23                                                             
HK25     L     RF,GLAROUT                                                       
         BASR  RE,RF                                                            
*                                                                               
HKXIT    B     XIT                                                              
*                                                                               
*                                                                               
TABLE    DS    0H                                                               
         DC    CL8'MYPROD',AL4(MYPROD)                                          
         DC    CL8'MYEST',AL4(MYEST)                                            
         DC    CL8'MYCLT',AL4(MYCLT)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
MYCLT    NTR1                                                                   
         L     R3,GLAIFLD                                                       
         CLI   DRIVLOOP,1                                                       
         BNE   MYCLT2                                                           
         MVC   0(3,R3),NBCLICOD                                                 
         GOTO1 NBCLPACK,DMCB,NBCLICOD,NDCLIKEY                                  
         B     MYCLTX                                                           
MYCLT2   CLI   DRIVLOOP,2                                                       
         BNE   MYCLT3                                                           
         MVI   0(R3),X'FF'                                                      
         MVI   1(R3),2                                                          
         B     MYCLTX                                                           
MYCLT3   CLI   DRIVLOOP,3                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R3),X'FF'                                                      
         MVI   1(R3),3                                                          
MYCLTX   B     XIT                                                              
         SPACE 2                                                                
*                                                                               
MYPROD   NTR1                                                                   
         L     R3,GLAIFLD                                                       
         CLI   DRIVLOOP,1                                                       
         BE    MYP5                                                             
         MVI   0(R3),X'FF'                                                      
         MVC   1(23,R3),0(R3)                                                   
         CLI   DRIVLOOP,3                                                       
         BE    *+10                                                             
         MVC   1(11,R3),=C'UNALLOCATED'                                         
         B     MYPX                                                             
MYP5     BAS   RE,GETPRD                                                        
MYPX     B     XIT                                                              
*                                                                               
*                                                                               
MYEST    NTR1                                                                   
         L     R3,GLAIFLD                                                       
         CLI   DRIVLOOP,1                                                       
         BE    MYEST5                                                           
         MVC   0(3,R3),=X'FFFFFF'                                               
         B     MYESTX                                                           
MYEST5   EDIT  (B1,NBACTEST),(3,0(R3)),ALIGN=LEFT                               
MYESTX   B     XIT                                                              
         EJECT                                                                  
GETPRD   NTR1                      GET PROD CODE/NAME                           
         L     R2,ANETWS3                                                       
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
         CLI   NBPRD,0                                                          
         BNE   GP10                                                             
GPUNA    MVC   PRDCDSV,=C'***'     PRODUCT IS UNALLOCATED                       
         B     GPX                                                              
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                SET TO UNDEFINED                             
         B     GPUNA                                                            
GP12     CLC   3(1,R2),NBPRD                                                    
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   PRDCDSV,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
         SPACE                                                                  
GPX      CLI   PRDCDSV,C'*'                                                     
         BNE   GPX5                                                             
         MVC   0(24,R3),=24X'40'                                                
         B     GPXIT                                                            
GPX5     DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),PRDCDSV                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,ANETWS4,DMWORK           
         L     R4,ANETWS4                                                       
         USING PRDHDR,R4                                                        
         MVC   0(3,R3),PKEYPRD                                                  
         MVC   4(20,R3),PNAME                                                   
*                                                                               
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         XC    FILENAME,FILENAME                                                
GPXIT    B     XIT                                                              
         EJECT                                                                  
*****************************************                                       
         EJECT                                                                  
*                                                                               
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,46,C'PRODUCT RECAP'                                           
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,53,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG                                                            
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
DRIVER   DS    A                                                                
ADRONE   DS    A                                                                
DOLADJ   DS    F            DOLLAR ADJUSTMENT PERCENTAGE                        
MYOPTS   DS    CL20         SET AT EDIT TIME(GLOBAL WS NOT AVAILABLE)           
*                               AND MOVE TO GLOPTS AT RUN TIME                  
DOLOPT   DS    CL1          1=ACTUAL,2=ASSIGNED                                 
DOWNOPT  DS    CL1          DOWNLOAD OPTION                                     
PEROPT   DS    CL1                                                              
DRIVLOOP DS    CL1                                                              
MYBYTE   DS    CL1                                                              
MONLIST  DS    CL64                                                             
NUMMONS  DS    F                                                                
PERTYPE  DS    CL4                                                              
PRDCDSV  DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF5D                                                       
       ++INCLUDE DRDICFILE                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038NEWRI15   05/01/02'                                      
         END                                                                    
