*          DATA SET NEWRI27    AT LEVEL 053 AS OF 05/01/02                      
*PHASE T32027A                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T32027 - N7  REPORT  PHASE'                                     
T32027   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE27**,RR=R2                                                 
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
****     MVC   MYOPTS+13(1),NDESTRMD                                            
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
*                                  DIG OUT ANY OPTIONS                          
EDT14    LA    R2,SPLOPTH          OPTIONS                                      
         MVI   MYOPTS+13,C'Y'      PRESET PIGGY OPTION                          
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
         BNE   OPT7                                                             
         MVI   MYOPTS+1,C'Y'                                                    
         B     EDT18                                                            
OPT7     CLC   12(6,R3),=C'PIGGYS'                                              
         BNE   OPTINV                                                           
         MVI   MYOPTS+13,0                                                      
*        CLI   SPLPRO,C'V'         NO PIGGYS WITH PRDGRPS                       
*        BE    EDINV                                                            
         B     EDT18                                                            
OPTINV   B     EDINV                                                            
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
         GOTO1 CALLOV,DMCB,0,X'D9032028'  LOAD T32028 (DPG PHASE)               
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
         MVI   NBSELUOP,C'A'                                                    
         MVC   NBPEROVR,PEROPT     OPTIONAL PROFILE OVERRIDE                    
         CLI   MYOPTS+13,C'Y'      SPLIT                                        
         BNE   PROCDATE                                                         
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POL                  
         SPACE 1                                                                
PROCDATE NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBVALDAT                                                  
         BNE   PROCDATE                                                         
         MVC   NUMMONS,=F'16'      SET UP FOR DATE LIST                         
         MVI   PERTYPE,C'W'                                                     
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
         MVI   NBRESUME,NBPROCPK                                                
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
MON1     LA    R2,MONLIST                                                       
         LA    R1,1                                                             
         L     R0,NUMMONS                                                       
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
*                                                                               
HK10     CLI   GLHOOK,GLRESOLV            INITIALIZATION                        
         BNE   HK14                                                             
         CLC   GLLABEL(4),=C'MYIN'                                              
         BNE   HK12                                                             
         LA    R1,MYIN                                                          
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK12     CLC   GLLABEL(6),=C'MYOUT1'                                            
         BNE   HK12A                                                            
         LA    R1,MYOUT1                                                        
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK12A    DS    0H                                                               
HK12B    DS    0H                                                               
         CLC   GLLABEL(5),=C'TOTIN'                                             
         BNE   HK12C                                                            
         LA    R1,MYIN                                                          
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK12C    CLC   GLLABEL(7),=C'MYLABEL'                                           
         BNE   HK12D                                                            
         LA    R1,MYLABEL                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK12D    CLC   GLLABEL(6),=C'MYOUT2'                                            
         BNE   HK12E                                                            
         LA    R1,MYOUT2                                                        
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK12E    CLC   GLLABEL(6),=C'MYOUT3'                                            
         BNE   HK14                                                             
         LA    R1,MYOUT3                                                        
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
*                                                                               
HK14     CLI   GLHOOK,GLROUT               ROUTINES                             
         BNE   HKXIT                                                            
*                                                                               
         CLI   GLMODE,GLOUTPUT                                                  
         BNE   HK30                                                             
         CLC   GLLABEL(6),=C'MYOUT1'                                            
         BNE   HK20                                                             
         L     RF,GLAROUT                                                       
         BR    RF                                                               
HK20     DS    0H                                                               
HK22     CLC   GLLABEL(6),=C'MYOUT2'                                            
         BNE   HK24                                                             
         L     RF,GLAROUT                                                       
         BR    RF                                                               
HK24     CLC   GLLABEL(6),=C'MYOUT3'                                            
         BNE   HKXIT                                                            
         L     RF,GLAROUT                                                       
         BR    RF                                                               
*                                                                               
HK30     CLI   GLMODE,GLINPUT                                                   
         BNE   HKXIT                                                            
         CLC   GLLABEL(4),=C'MYIN'                                              
         BNE   HK32                                                             
         L     RF,GLAROUT                                                       
         BR    RF                                                               
HK32     CLC   GLLABEL(5),=C'TOTIN'                                             
         BNE   HK34                                                             
         L     RF,GLAROUT                                                       
         BR    RF                                                               
HK34     CLC   GLLABEL(7),=C'MYLABEL'                                           
         BNE   HKXIT                                                            
         L     RF,GLAROUT                                                       
         BR    RF                                                               
HKXIT    B     XIT                                                              
         EJECT                                                                  
MYIN     DS    0H                                                               
         L     R1,GLAIFLD                                                       
         LA    R2,1                                                             
         TM    NBUNITST,X'04'      IS IT PFB                                    
         BO    *+12                                                             
         ST    R2,0(R1)                                                         
         B     *+8                                                              
         ST    R2,4(R1)            FOR PFB                                      
         B     XIT                                                              
*                                                                               
MYOUT1   DS    0H                  DUMMY FOR BLANK LINE                         
**       L     R1,GLAOFLD                                                       
**       MVI   0(R1),0                                                          
         B     XIT                                                              
*                                                                               
MYOUT2   DS    0H                                                               
         L     R5,GLAIFLD                                                       
         OC    0(4,R5),0(R5)                                                    
         BZ    XIT                                                              
         L     R3,GLAOFLD                                                       
         EDIT  (B4,0(R5)),(5,0(R3))                                             
         B     XIT                                                              
*                                                                               
MYOUT3   DS    0H                                                               
         L     R5,GLAIFLD                                                       
         OC    4(4,R5),4(R5)                                                    
         BZ    XIT                                                              
         L     R3,GLAOFLD                                                       
         EDIT  (B4,4(R5)),(5,0(R3))                                             
         B     XIT                                                              
*                                                                               
MYLABEL  DS    0H                  DUMMY                                        
**       L     R1,GLAIFLD                                                       
**       MVI   0(R1),0                                                          
         B     XIT                                                              
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
MYOPTS   DS    CL20         SET AT EDIT TIME(GLOBAL WS NOT AVAILABLE)           
*                               AND MOVE TO GLOPTS AT RUN TIME                  
DOWNOPT  DS    CL1          DOWNLOAD OPTION                                     
PEROPT   DS    CL1                                                              
MONLIST  DS    CL72                                                             
NUMMONS  DS    F                                                                
PERTYPE  DS    CL4                                                              
SCH      DS    CL1                                                              
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE4D                                                       
       ++INCLUDE CTGENDIC                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053NEWRI27   05/01/02'                                      
         END                                                                    
