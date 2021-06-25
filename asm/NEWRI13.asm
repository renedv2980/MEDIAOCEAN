*          DATA SET NEWRI13    AT LEVEL 093 AS OF 05/01/02                      
*PHASE T32013A                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T32013 - N2  REPORT  PHASE'                                     
T32013   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE34**,RR=R2                                                 
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
         MVC   NBACLI,ANETWS3     CLIENT RECORD IN W23                          
         MVC   NBANBUFF,ANETWS4   NETWORK BUFFERIN W4                           
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
         MVC   MYOPTS+10(1),NDCLIRMD                                            
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         MVC   MYOPTS+11(1),NDPRGRMD                                            
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
         MVC   MYOPTS+13(1),NDESTRMD                                            
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
         MVC   MYOPTS+12(1),NDNETRMD                                            
         CLC   SPLNET(3),=C'ALL'                                                
         BE    *+8                                                              
         MVI   MYOPTS+12,0                                                      
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
         EJECT                                                                  
*                                                                               
         LA    R2,SPLDOLH                                                       
         MVI   NDSFTARG,3            ACTUAL DOLLARS(DEFAULT)                    
         MVC   HEADSV(6),=C'ACTUAL'                                             
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT10                                                            
         XC    HEADSV(6),HEADSV                                                 
         MVI   NDSFTARG,0                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A39'  LOAD T00A39 (DRONE)                   
         MVC   ADRONE,DMCB                                                      
         MVI   DRWHO,DRNETWHO                                                   
         MVI   DRACTION,DRENTRY                                                 
         MVC   DRDICT(4),=C'NETA'                                               
         ST    R2,DRNETFLD                                                      
         L     R1,ANETWS4                                                       
         A     R1,=F'1000'                                                      
         ST    R1,DRNETIO                                                       
         OC    NBACOM,NBACOM                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DRCOMFAC,NBACOM                                                  
         LA    R3,DRGEN                                                         
         GOTO1 ADRONE,DMCB,(R3)                                                 
         CLI   DRERROR,0                                                        
         BNE   EDINV                                                            
         L     R5,DRNETIO                                                       
         LA    R5,42(R5)                                                        
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEITD,R5                                                         
         CLC   DEITYPE,=C'M+'      CHECK DATA TYPE                              
         BNE   EDINV                                                            
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEIAD,R5                                                         
         MVC   NDSFTARG(1),DEIARGS       SET SOFT DOLLAR ARG                    
         MVI   ELCODE,X'87'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DELITD,R5                                                        
         LA    R3,HEADSV                                                        
MOVHED   DS    0H                                                               
         ZIC   R1,DELITLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DELITRAL                                                 
         BAS   RE,NEXTEL                                                        
         BNE   EDT10                                                            
         LA    R3,12(R3)                                                        
         B     MOVHED                                                           
         DROP  R5                                                               
         EJECT                                                                  
EDT10    LA    R2,SPLADJH               DOLLAR ADJUSTMENT PERCENTAGE            
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT12                                                            
         LR    R4,R1                                                            
         GOTO1 =V(CASHVAL),DMCB,(2,FLD),(R4),RR=RELO                            
         CLI   0(R1),X'FF'                                                      
         BE    EDINV                                                            
         MVC   NDADJPCT,4(R1)                                                   
*                                                                               
EDT12    LA    R2,SPLNOPTH               OPTION TO SHOW NETWORK DETAILS         
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT14                                                            
         CLI   FLD,C'N'                                                         
         BE    EDT14                                                            
         CLI   FLD,C'Y'                                                         
         BNE   EDINV                                                            
*        CLI   MYOPTS+12,2                                                      
*        BNE   EDINV               IF NET=ALL NO NET DETAILS                    
         MVI   MYOPTS,C'Y'                                                      
*                                  DIG OUT ANY OPTIONS                          
EDT14    LA    R2,SPLOPTH          OPTIONS                                      
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
         BNE   OPT5                                                             
         MVC   PEROPT,22(R3)                                                    
         CLI   PEROPT,C'C'         CAN BE CALENDAR                              
         BE    EDT18                                                            
         CLI   PEROPT,C'B'         BROADCAST                                    
         BE    EDT18                                                            
         CLI   PEROPT,C'S'         OR SPECIAL                                   
         BE    EDT18                                                            
         B     EDINV               OTHERWISE ITS NO GOOD                        
OPT5     CLC   12(4,R3),=C'SKIP'  SEPERATE PAGE PER PRODUCT                     
         BNE   *+12                                                             
         MVI   MYOPTS+1,C'Y'                                                    
         B     EDT18                                                            
         B     EDINV                                                            
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
         GOTO1 CALLOV,DMCB,0,X'D9032003'  LOAD T32003 (DPG PHASE)               
         MVC   GLAPROG,DMCB                                                     
         SPACE 1                                                                
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
         MVI   GLFHEADL,10                                                      
         EJECT                                                                  
*              NOW CONTROL NETIO                                                
         SPACE 3                                                                
         MVI   NBDATA,C'U'                                                      
         MVI   NBRESUME,NBPROCPK                                                
*        MVI   NBTRCOPT,C'Y'                                                    
*        MVC   NBPRINT,VPRINT                                                   
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         MVC   NBPEROVR,PEROPT     OPTIONAL PROFILE OVERRIDE                    
         SPACE 1                                                                
PROCDATE NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBVALDAT                                                  
         BNE   PROCDATE                                                         
         MVC   NUMMONS,=F'12'      SET UP FOR DATE LIST                         
         MVI   PERTYPE,C'M'                                                     
         MVI   PERTYPE+1,1                                                      
         MVI   PERTYPE+2,1                                                      
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         L     R1,NUMMONS                                                       
         STC   R1,MYOPTS+2         PASS THE ACTUAL N'MONTHS                     
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
         LA    R0,12                                                            
         SPACE 1                                                                
MON2     CLC   NBACTDAT,0(R2)      LOOK UP MONTH NUMBER                         
         BL    MON4                                                             
         CLC   NBACTDAT,2(R2)                                                   
         BH    MON4                                                             
         STC   R1,GLDATENO         PASS THAT THROUGH TO DRIVER                  
         B     GOTIN                                                            
         SPACE 1                                                                
MON4     LA    R2,R4(R2)                                                        
         LA    R1,1(R1)                                                         
         BCT   R0,MON2                                                          
         MVI   GLDATENO,0                                                       
         B     GETUNIT                                                          
         SPACE 1                                                                
GOTIN    MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     GETUNIT                                                          
         SPACE 1                                                                
*                                  GO AND DO THE OUTPUT                         
ALLDONE  MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
*                                                                               
MYHEAD   NTR1                                                                   
         L     R3,GLAOFLD                                                       
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,HEADSV                                                        
         LA    R2,12                                                            
         SR    R4,R4                                                            
MHLOOP   CLI   0(R1),X'40'                                                      
         BNH   MHLX                                                             
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,MHLOOP                                                        
MHLX     DS    0H                                                               
         LR    R1,R4                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),HEADSV                                                   
MYHEADX  XIT1                                                                   
*                                                                               
MYHEAD2  NTR1                                                                   
         L     R3,GLAOFLD                                                       
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,HEADSV2                                                       
         LA    R2,12                                                            
         SR    R4,R4                                                            
MHLOOP2  CLI   0(R1),X'40'         CENTER HEADSV IN HEADLINE                    
         BNH   MHLX2                                                            
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,MHLOOP2                                                       
MHLX2    DS    0H                                                               
         LR    R1,R4                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),HEADSV2                                                  
MYHEADX2 XIT1                                                                   
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
*              HEADLINE ROUTINES, ETC                                           
*                                                                               
HOOK     NTR1                              HEAD HOOK                            
*                                                                               
         CLI   GLHOOK,GLHEAD              HEADLINES                             
         BNE   HK10                                                             
         NETGO NVHEAD,DMCB                                                      
         B     HKXIT                                                            
HK10     CLI   GLHOOK,GLRESOLV            INITIALIZATION                        
         BNE   HK12                                                             
         CLC   GLLABEL(7),=C'MYHEAD2 '                                          
         BNE   HK11                                                             
         LA    R1,MYHEAD2                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK11     CLC   GLLABEL(6),=C'MYHEAD  '                                          
         BNE   HK12                                                             
         LA    R1,MYHEAD                                                        
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK12     CLI   GLHOOK,GLROUT               MYHEAD ROUTINE                       
         BNE   HKXIT                                                            
         CLI   GLMODE,GLOUTPUT                                                  
         BNE   HKXIT                                                            
         CLC   GLLABEL(6),=C'MYHEAD  '                                          
         BNE   HKXIT                                                            
         L     RF,GLAROUT                                                       
         BASR  RE,RF                                                            
         SPACE 1                                                                
HKXIT    B     XIT                                                              
*                                                                               
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
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
MONLIST  DS    CL64                                                             
NUMMONS  DS    F                                                                
PERTYPE  DS    CL4                                                              
HEADSV   DS    CL12                                                             
HEADSV2  DS    CL12                                                             
HEADSV3  DS    CL12                                                             
HEADSV4  DS    CL12                                                             
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF3D                                                       
       ++INCLUDE CTGENDIC                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093NEWRI13   05/01/02'                                      
         END                                                                    
