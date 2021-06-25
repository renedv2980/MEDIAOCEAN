*          DATA SET NEWRI14    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T32014A                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T32014 - N3  REPORT  PHASE'                                     
T32014   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEN3**,RR=R2                                                 
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
         MVC   NBACLI,ANETWS2     CLIENT RECORD IN WS2                          
         ST    R2,RELO            ANETWS3 = DICTIONARY RECORD (DRONE)           
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
         CLC   SPLEST(3),=C'ALL'                                                
         BNE   *+8                                                              
         MVI   MYOPTS+13,1                                                      
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
         MVC   MYOPTS(1),NDNETRMD                                               
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
         LA    R2,SPLFLD1H               DOLLAR COLUMNS                         
         NETGO NVDATA,DMCB,(3,FLDHEAD1)                                         
         BNE   ERRDATA                                                          
         CLI   FLDHEAD1,0                                                       
         BE    EDT10                                                            
         MVI   MYOPTS+1,1                                                       
         CLI   FLDHEAD2,0                                                       
         BE    EDT10                                                            
         MVI   MYOPTS+2,1                                                       
         CLI   FLDHEAD3,0                                                       
         BE    EDT10                                                            
         MVI   MYOPTS+3,1                                                       
*                                                                               
EDT10    LA    R2,SPLADJH               DOLLAR ADJUSTMENT PERCENTAGE            
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT12                                                            
         LR    R4,R1                                                            
         GOTO1 =V(CASHVAL),DMCB,(2,FLD),(R4),RR=RELO                            
         CLI   0(R1),X'FF'                                                      
         BE    EDINV                                                            
         MVC   NDADJPCT,4(R1)                                                   
*                                                                               
EDT12    DS    0H                                                               
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
         BNE   *+12                                                             
         MVI   DOWNOPT,C'Y'                                                     
         B     EDT18                                                            
         CLC   12(4,R3),=C'SKIP'  SEPERATE PAGE PER PRODUCT                     
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
ERRDATA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'ERROR IN DATA FIELD #   '                         
         LA    R3,CONHEAD+22                                                    
         EDIT  (R1),(2,0(R3))                                                   
         GOTO1 ERREX2                                                           
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
EDERR    GOTO1 ERREX                                                            
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
         GOTO1 CALLOV,DMCB,0,X'D9032004'  LOAD T32004 (DPG PHASE)               
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
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         MVC   GLOPTS,MYOPTS       MOVE MYOPTS DATA FROM EDIT TO GLOPTS         
         MVI   GLTRACE,C'N'                                                     
*                                                                               
         CLI   DOWNOPT,C'Y'        OPTION TO DOWNLOAD                           
         BNE   *+8                                                              
         MVI   GLDOWNLD,X'80'                                                   
*                                                                               
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R6)                                                 
*                                                                               
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
GOTIN    MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     GETUNIT                                                          
         SPACE 1                                                                
*                                  GO AND DO THE OUTPUT                         
ALLDONE  MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
         EJECT                                                                  
FIELD1   DS    0H                                                               
         LA    R1,FLDHEAD1                                                      
         LR    R5,R1                                                            
         B     MYHEAD                                                           
*                                                                               
FIELD1A  DS    0H                                                               
         LA    R1,FLDHEAD1+12                                                   
         LR    R5,R1                                                            
         B     MYHEAD                                                           
*                                                                               
FIELD2   DS    0H                                                               
         LA    R1,FLDHEAD2                                                      
         LR    R5,R1                                                            
         B     MYHEAD                                                           
*                                                                               
FIELD2A  DS    0H                                                               
         LA    R1,FLDHEAD2+12                                                   
         LR    R5,R1                                                            
         B     MYHEAD                                                           
*                                                                               
FIELD3   DS    0H                                                               
         LA    R1,FLDHEAD3                                                      
         LR    R5,R1                                                            
         B     MYHEAD                                                           
*                                                                               
FIELD3A  DS    0H                                                               
         LA    R1,FLDHEAD3+12                                                   
         LR    R5,R1                                                            
         B     MYHEAD                                                           
         SPACE 2                                                                
************************************                                            
* EXPECTS  R3 - GLAOFLD                                                         
*          R1,R5 - SAVED FIELD HEADER (FLDHEAD1/2/3)                            
*                                                                               
MYHEAD   DS    0H                                                               
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
         MVC   0(0,R3),0(R5)                                                    
MYHEADX  XIT1                                                                   
*                                                                               
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
         BNE   HK12                                                             
         LA    R3,TABLE                                                         
         LA    R2,6                                                             
HK11     CLC   GLLABEL,0(R3)                                                    
         BE    HK11A                                                            
         LA    R3,12(R3)                                                        
         BCT   R2,HK11                                                          
         B     HKXIT                                                            
HK11A    MVC   GLAROUT,8(R3)                                                    
         B     HKXIT                                                            
*                                                                               
HK12     CLI   GLHOOK,GLROUT               MYHEAD ROUTINE                       
         BNE   HKXIT                                                            
         CLI   GLMODE,GLOUTPUT                                                  
         BNE   HKXIT                                                            
         CLC   GLLABEL(5),=C'FIELD'                                             
         BNE   HKXIT                                                            
         L     R3,GLAOFLD                                                       
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,GLAROUT                                                       
         BASR  RE,RF                                                            
         SPACE 1                                                                
HKXIT    B     XIT                                                              
*                                                                               
TABLE    DS    0H                                                               
         DC    CL8'FIELD1  ',AL4(FIELD1)                                        
         DC    CL8'FIELD1A ',AL4(FIELD1A)                                       
         DC    CL8'FIELD2  ',AL4(FIELD2)                                        
         DC    CL8'FIELD2A ',AL4(FIELD2A)                                       
         DC    CL8'FIELD3  ',AL4(FIELD3)                                        
         DC    CL8'FIELD3A ',AL4(FIELD3A)                                       
         DC    X'FF'                                                            
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
MONLIST  DS    CL64                                                             
NUMMONS  DS    F                                                                
PERTYPE  DS    CL4                                                              
FLDHEAD1 DS    CL12                                                             
         DS    CL12                                                             
FLDHEAD2 DS    CL12                                                             
         DS    CL12                                                             
FLDHEAD3 DS    CL12                                                             
         DS    CL12                                                             
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF4D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030NEWRI14   05/01/02'                                      
         END                                                                    
