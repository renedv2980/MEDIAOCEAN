*          DATA SET SRDIS00    AT LEVEL 009 AS OF 09/19/11                      
*PROCESS USING(WARN(15))                                                        
*PHASE T11800A                                                                  
         TITLE '$DISPLAY/PATCH - DISPLAY/ALTER CORE/PRGMS'                      
         PRINT NOGEN                                                            
DISPLAY  RSECT                                                                  
         NMOD1 WORKL,*$DISP**,RA,R9,CLEAR=YES,RR=RE                             
         USING WORKD,RC                                                         
         USING PROGSPCD,PSREC                                                   
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   IPARMS,0(R1)                                                     
*                                                                               
         L     R8,ATWA                                                          
         USING SRDISFFD,R8         R9=A(TWA)                                    
         L     R7,AUTL                                                          
         USING UTLD,R7             R8=A(UTL ENTRY)                              
         BRAS  RE,INIT             DO INITIALISATION                            
*                                                                               
         CLC   =C'00',SRVSAV+00                                                 
         BH    MAIN06                                                           
         CLC   =C'00',SRVSAV+20                                                 
         BH    MAIN06                                                           
         CLC   =C'00',SRVSAV+40                                                 
         BH    MAIN06                                                           
*                                                                               
         PACK  DUB,SRVSAV+00(2)                                                 
         CVB   RF,DUB                                                           
         CLM   RF,1,SRVP1H+(FHIL-FHD)                                           
         BNE   MAIN06                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   MAIN06                                                           
         CLC   SRVP1(0),SRVSAV+02                                               
         PACK  DUB,SRVSAV+20(2)                                                 
         CVB   RF,DUB                                                           
         CLM   RF,1,SRVP2H+(FHIL-FHD)                                           
         BNE   MAIN06                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   MAIN06                                                           
         CLC   SRVP2(0),SRVSAV+22                                               
         PACK  DUB,SRVSAV+40(2)                                                 
         CVB   RF,DUB                                                           
         CLM   RF,1,SRVP3H+(FHIL-FHD)                                           
         BNE   MAIN06                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   MAIN06                                                           
         CLC   SRVP3(0),SRVSAV+42                                               
         B     MAIN08                                                           
*                                                                               
MAIN06   MVC   SRVID(8),=CL8'=DISPLAY'                                          
*                                                                               
MAIN08   BRAS  RE,VALP1            VALIDATE P1                                  
         BNE   MAINX                                                            
*                                                                               
         CLI   INCORE,YES          LOOKING AT CORE?                             
         BE    *+8                 YES                                          
         BRAS  RE,LOADIT           LOAD OUR PHASE                               
*                                                                               
         BRAS  RE,VALP2            VALIDATE P2                                  
         BNE   MAINX                                                            
*                                                                               
         BRAS  RE,VALPFK           CHECK AND VALIDATE SCROLL PFKEYS             
         BH    *+12                                                             
         BRAS  RE,VALP3            VALIDATE P3                                  
         BNE   MAINX                                                            
*                                                                               
         CLI   WPATCH,YES          WANT TO DO A PATCH?                          
         BNE   MAIN10              NO                                           
*                                                                               
         LA    RF,SRVIDH                                                        
         USING FHD,RF                                                           
         CLI   FHDA+1,C'P'         ARE WE ABOUT TO DO A PATCH?                  
         BNE   MAIN10              NO                                           
         DROP  RF                                                               
*                                                                               
         MVI   DDDSP,YES           SET UP FOR DISPLAY AFTER PATCH               
         MVI   DDHEX,YES                                                        
         MVI   DDALF,YES                                                        
         BRAS  RE,PATCH            DO PATCH STUFF                               
         BE    *+8                                                              
         MVI   DDHEX,NO            IF ERROR - LEAVE HEX AS IT IS                
         BRAS  RE,DIS              REDISPLAY FIELDS FOR USER                    
         B     MAINX                                                            
*                                                                               
MAIN10   BRAS  RE,DISP             DO DISPLAY FUNCTIONS                         
*                                                                               
MAINX    CLI   INHELP,YES                                                       
         BE    MAINX1                                                           
*                                                                               
         LA    R2,SRVSAVH                                                       
         USING FHD,R2              RF=A(SAVE FLD HDR)                           
         XR    R0,R0                                                            
         IC    R0,SRVP1H+(FHIL-FHD)                                             
         EDIT  (R0),(2,FHDA+00),0,ZERO=NOBLANK,FILL=0                           
         MVC   FHDA+02(16),SRVP1                                                
         XR    R0,R0                                                            
         IC    R0,SRVP2H+(FHIL-FHD)                                             
         EDIT  (R0),(2,FHDA+20),0,ZERO=NOBLANK,FILL=0                           
         MVC   FHDA+22(16),SRVP2                                                
         XR    R0,R0                                                            
         IC    R0,SRVP3H+(FHIL-FHD)                                             
         EDIT  (R0),(2,FHDA+40),0,ZERO=NOBLANK,FILL=0                           
         MVC   FHDA+42(16),SRVP3                                                
         MVI   FHDA+60,C'X'                                                     
         OC    FHDA(78),SPACES                                                  
         MVI   FHOL,61                                                          
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R2,SRVIDH                                                        
         MVC   SRVID,SPACES                                                     
         MVC   FHDA(8),=CL8'=DISPLAY'                                           
         OI    FHOI,FHOITR                                                      
         CLI   WPATCH,YES          PATCH AFTER DISP?                            
         BNE   MAINX1                                                           
         CLI   HDRN,7              ALREADY PATCHED?                             
         BE    *+10                YES - BACK TO DISP                           
         MVC   FHDA(8),=CL8'=PATCH'                                             
         DROP  R2                                                               
*                                                                               
MAINX1   CLI   FERN,0              ERROR?                                       
         BE    *+12                NO                                           
         BRAS  RE,DISERR           DISPLAY ERROR MESSAGE                        
         B     XMOD                                                             
*                                                                               
         BRAS  RE,DISOK            DISPLAY OK MESSAGE                           
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVI   HDRN,1              SET DEFAULT OUTPUT MESSAGE                   
*                                                                               
         L     RF,ASYSFACS                                                      
         USING SYSFACD,RF          R4=A(SYS FAC LIST)                           
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ALOGGER,VLOGGER                                                  
         MVC   ADMOD000,VDMOD000                                                
         MVC   AWCTYPE,VWCTYPE                                                  
         MVC   ALNKTAB,VLNKTAB                                                  
         MVC   ASSB,VSSB                                                        
         MVC   ATCB,VTCB                                                        
         DROP  RF                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF         GET COMMON FACILITIES                        
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   APERSON,CPERSON                                                  
         DROP  RF                                                               
*                                                                               
         L     RF,ASSB                                                          
         USING SSBD,RF                                                          
         MVC   ALANGTAB,SSBALANG                                                
         MVC   SYSID,SSBSYSID                                                   
         MVC   SYSCH,SSBSYSCH                                                   
         MVC   SYSNAME,SSBSYSN4                                                 
         MVC   SYSSHRT,SSBSYSNA                                                 
         MVC   ST24,SSBLOADR                                                    
         MVC   EN24,SSBHIADR                                                    
         MVC   ST31,SSBXALO                                                     
         MVC   EN31,SSBXAHI                                                     
         MVI   PRODSYS,C'Y'                                                     
         TM    SSBSYSFL,X'80'      PRODUCTION SYSTEM?                           
         BZ    *+8                 YES                                          
         MVI   PRODSYS,C'N'                                                     
         MVC   MYTCB,SSBTKADR                                                   
         DROP  RF                                                               
*                                                                               
         L     RF,ATIOB            LOAD A(TIOB)                                 
         USING TIOBD,RF                                                         
         XR    R0,R0                                                            
         IC    R0,TIOBAID          USE PFKEYS 1-12 ONLY                         
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY            SAVE PFKEY PRESSED                           
         ICM   R1,3,TIOBCURS       DISPLACEMENT TO FIELD FOR CURSOR             
         STCM  R1,3,SCURSOR                                                     
         DROP  RF                                                               
*                                                                               
         MVC   SAVELANG,TLANG      SAVE CONNECTED LANGUAGE                      
         TM    TSVCREQ,X'01'                                                    
         BNO   *+8                                                              
         OI    TSVCREQ,X'02'                                                    
*                                                                               
         GOTO1 ATICTOC,DMCB,C'SGET'                                             
         MVC   SAVETIME,DMCB       SAVE TIME FOR LOGREC                         
*                                                                               
         LA    RF,SRVIDH           LOOK FOR =DISP,XXX                           
         USING FHD,RF                                                           
         CLI   FHDA+1,C'D'         IGNORE IF =PATCH                             
         BNE   INIT06                                                           
*                                                                               
         LA    RE,FHDA+1                                                        
         LHI   R0,8                                                             
INIT02   CLI   0(RE),C','                                                       
         BE    INIT04                                                           
         AHI   RE,1                                                             
         BCT   R0,INIT02                                                        
         B     INIT06                                                           
*                                                                               
INIT04   LA    RF,SRVP1H                                                        
         MVC   FHDA(8),1(RE)       SAVE ANY POSSIBLE INPUT                      
         MVI   FHIL,8              FLAG INPUT                                   
         DROP  RF                                                               
*                                                                               
INIT06   B     EXITOK                                                           
*&&DO                                                                           
INIT06   GOTO1 APERSON,DMCB,0,0,ACOMFACS,ASYSFACS                               
         CLI   8(R1),0                                                          
         BNE   EXITOK                                                           
*                                                                               
         L     R1,4(R1)                                                         
         USING PERSOND,R1                                                       
         LA    RF,SRVP4H                                                        
         USING FHD,RF                                                           
         MVC   FHDA(8),PERPID                                                   
         MVI   FHIL,8                                                           
         B     EXITOK                                                           
         DROP  R1,RF                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALP1  VALIDATE INPUT PARAMETERS                                    *         
***********************************************************************         
         SPACE 1                                                                
VALP1    NTR1  ,                                                                
         LA    R2,SRVP1H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
*                                                                               
VP101    CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,1              MISSING INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   VP101A                                                           
         BRAS  RE,P1HELP                                                        
         BH    VP101               SELECT FROM HELP BY PFKEY                    
         MVI   INHELP,YES                                                       
         B     EXITL                                                            
*                                                                               
VP101A   MVI   INXA,NO                                                          
         MVI   INCORE,NO                                                        
*                                                                               
         LARL  R3,SYSTAB           ASK FOR ADDRESS FROM SYSFACS                 
         USING NAMED,R3                                                         
         XR    R1,R1                                                            
         L     R0,ASYSFACS                                                      
VP102    CLI   0(R3),EOT                                                        
         BE    VP104                                                            
         IC    R1,NLEN                                                          
         BCTR  R1,0                                                             
         EX    R1,VP1CLC                                                        
         BE    VP120                                                            
         AHI   R3,NAMEL                                                         
         B     VP102                                                            
*                                                                               
VP104    LARL  R3,TCBTAB           ASK FOR ADDRESS FROM TCB                     
         USING NAMED,R3                                                         
         XR    R1,R1                                                            
         L     R0,MYTCB                                                         
VP106    CLI   0(R3),EOT                                                        
         BE    VP108                                                            
         IC    R1,NLEN                                                          
         BCTR  R1,0                                                             
         EX    R1,VP1CLC                                                        
         BE    VP120                                                            
         AHI   R3,NAMEL                                                         
         B     VP106                                                            
*                                                                               
VP108    LARL  R3,COMTAB           ASK FOR ADDRESS FROM COMFACS                 
         USING NAMED,R3                                                         
         XR    R1,R1                                                            
         L     R0,ACOMFACS                                                      
VP110    CLI   0(R3),EOT                                                        
         BE    VP112                                                            
         IC    R1,NLEN                                                          
         BCTR  R1,0                                                             
         EX    R1,VP1CLC                                                        
         BE    VP120                                                            
         AHI   R3,NAMEL                                                         
         B     VP110                                                            
*                                                                               
VP112    LARL  R3,LNKTAB           ASK FOR ADDRESS FROM LNKTAB                  
         USING NAMED,R3                                                         
         XR    R1,R1                                                            
         L     R0,ALNKTAB                                                       
VP114    CLI   0(R3),EOT                                                        
         BE    VP116                                                            
         IC    R1,NLEN                                                          
         BCTR  R1,0                                                             
         EX    R1,VP1CLC                                                        
         BE    VP120                                                            
         AHI   R3,NAMEL                                                         
         B     VP114                                                            
*                                                                               
VP116    B     VP122               ROOM FOR EXTRA TABLES                        
*                                                                               
VP1CLC   CLC   FHDA(0),NNAME                                                    
*                                                                               
VP120    XR    R1,R1                                                            
         ICM   R1,3,NDISP                                                       
         AR    R1,R0                                                            
         MVC   FULL,0(R1)                                                       
         MVC   ACORE,0(R1)         SAVE ADDRESS                                 
*                                                                               
         MVI   INCORE,YES          SET IN CORE                                  
         CLI   START,0                                                          
         BE    *+8                                                              
         MVI   INXA,YES                                                         
         MVC   LENGTH,=AL2(MAXLEN) SET DISPLAY LENGTH TO MAXIMUM                
*                                                                               
         MVC   SRVP1,SPACES        CLEAR FIELD AND SET NAME CORRECTLY           
         MVC   SRVP1(L'NNAME),NNAME                                             
         MVC   FHIL,NLEN                                                        
         MVC   FHOL,NLEN                                                        
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R2,SRVP2H           SET DEFAULT DISPLACEMENT IN P2               
         CLI   FHIL,0                                                           
         BNE   EXITOK                                                           
         MVI   FHDA,C'0'                                                        
         OI    FHII,FHIIHE         SET VALID HEX                                
         MVI   FHIL,1                                                           
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
VP122    LA    R2,SRVP1H           INPUT MUST BE CORE/COREX/PHASE               
         CLI   FHIL,5                                                           
         BH    VP124                                                            
         CLI   FHIL,3              MAKE SURE WE HAVE ENOUGH INPUT               
         BH    *+12                                                             
         MVI   FERN,4                                                           
         B     EXITL                                                            
*                                                                               
         MVI   INCORE,YES                                                       
         MVC   FHDA(16),SRVP1                                                   
         CLC   FHDA(4),=C'CORE'                                                 
         BE    EXITOK                                                           
         MVI   INXA,YES                                                         
         CLC   FHDA(5),=C'COREX'                                                
         BE    EXITOK                                                           
         XC    FHDA(16),FHDA                                                    
         MVI   FERN,2              KEYWORD/PHASE/CORE(X)                        
         B     EXITL                                                            
*                                                                               
VP124    CLI   FHIL,8              TRY FOR TSPP00(L)                            
         BL    *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVI   INCORE,NO                                                        
*                                                                               
         CLI   FHIL,6              WANTED A TEST PHASE?                         
         BE    VP128               NO                                           
         MVC   PSLVL,FHDA+6        CHECK LEVEL (S/B A,B OR C)                   
         CLI   PSLVL,C'A'                                                       
         BL    VP126                                                            
         CLI   PSLVL,C'C'                                                       
         BH    VP126                                                            
         B     VP128                                                            
*                                                                               
VP126    MVI   FERN,5              INVALID TEST LEVEL                           
         MVI   FERRDSP,6                                                        
         B     EXITL                                                            
*                                                                               
VP128    MVC   DDSLVL,PSLVL        SAVE TEST LEVEL FOR UTL                      
         NI    DDSLVL,TTESTLVL                                                  
*                                                                               
         MVC   PSLANG,FHDA         SAVE FIRST LETTER OF PHASE NAME              
         L     R1,ALANGTAB                                                      
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1                                                      
VP130    CLC   LANGOVL1,PSLANG  TEST LANGUAGE OVERLAY LETTER                    
         BE    VP132                                                            
         BXLE  R1,RE,VP130                                                      
         MVI   FERRDSP,0                                                        
         MVI   FERN,6              INVALID LANGUAGE OVERLAY                     
         B     EXITL                                                            
*                                                                               
VP132    MVC   DDSLANG,LANGCODE    SET LANGUAGE CODE                            
         CLI   DDSLANG,2                                                        
         BH    *+8                                                              
         MVI   DDSLANG,0                                                        
         DROP  R1                                                               
*                                                                               
         MVC   DUB(6),FHDA         MOVE PHASE NAME TO CALLOV PARMLIST           
         MVI   DUB,C'0'                                                         
         GOTO1 AHEXIN,DMCB,DUB,PSNAME,6                                         
         OC    12(4,R1),12(R1)                                                  
         BNZ   VP134                                                            
         MVI   FERRDSP,1                                                        
         MVI   FERN,7                                                           
         B     EXITL                                                            
*                                                                               
VP134    MVC   TLANG,DDSLANG                                                    
         MVC   SAVETEST,TTEST      SAVE OUR TTEST INFO                          
         MVC   SAVETST1,TTEST1                                                  
         MVC   TTEST,DDSLVL                                                     
         OI    TTEST1,TTESTURT     SET USE REAL TTEST                           
*NOPAHYD OI    TTEST,TTESTSRA      *TEMP*                                       
         MVC   SPSREC,PSREC        SAVE REQUESTED RECORD                        
         ICM   R0,7,PSNAME                                                      
         ICM   R0,8,=C'S'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(R0),(C'D',0)                                 
         CLI   4(R1),255                                                        
         BNE   VP136                                                            
         MVI   FERN,8              PHASE NAME IS NOT IN PHASE LIST              
         B     EXITL                                                            
*                                                                               
VP136    MVI   TLANG,0             RESET LANGUAGE                               
         MVC   TTEST,SAVETEST      RESET TTEST                                  
         MVC   TTEST1,SAVETST1                                                  
         CLC   PSREC(PSKEYL),SPSREC                                             
         BE    EXITOK                                                           
         MVI   FERN,8              PHASE NAME IS NOT IN PHASE LIST              
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALP2  VALIDATE INPUT PARAMETERS                                    *         
***********************************************************************         
         SPACE 1                                                                
VALP2    NTR1  ,                                                                
         LA    R2,SRVP2H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
*                                                                               
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,1              MISSING INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         XR    RF,RF               LOOK FOR SPECIAL SEARCH STRINGS              
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         MVI   BYTE,C'L'                                                        
         EX    RF,*+8                                                           
         BE    VP202                                                            
         CLC   FHDA(0),=C'LEVEL='  "L": LEVEL=                                  
         MVI   BYTE,C'B'                                                        
         EX    RF,*+8                                                           
         BE    VP202                                                            
         CLC   FHDA(0),=C'BOOK='   "B": BOOK=                                   
         MVI   BYTE,C'K'                                                        
         CLI   FHDA,C'K'           "K": *LNKSTAMP* (FOR LNK MODULES)            
         BNE   VP228               IT'S NOT A SPECIAL STRING                    
*                                                                               
VP202    DS    0H                                                               
         CLC   =C'CORE',SRVP1      IS THIS A "CORE" (OR COREX) SEARCH?          
         BNE   *+12                NO                                           
         MVI   FERN,9              OPTION NOT VALID WITH <CORE>                 
         B     EXITL                                                            
*                                                                               
         LHI   R0,1                DEFAULT IS LEVEL=1                           
*                                                                               
         L     R3,APHASE           ASSUME NOT CORE-RESIDENT                     
         CLI   INCORE,YES          LOOKING AT CORE?                             
         BNE   *+8                 NO                                           
         L     R3,ACORE            IT IS CORE-RESIDENT                          
*                                                                               
         LA    R2,SRVP3H                                                        
         CLI   FHIL,0                                                           
         BE    VP212                                                            
         TM    FHII,FHIINU                                                      
         BNO   VP206                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FHDA(0)                                                      
         CVB   R0,DUB              R0 = LOOP COUNTER                            
         B     VP212                                                            
*                                                                               
VP206    XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         XR    R0,R0                                                            
         LA    RE,FHDA                                                          
*                                                                               
VP208    CLI   0(RE),C'0'                                                       
         BL    VP210                                                            
         CLI   0(RE),C'9'                                                       
         BH    VP210                                                            
         AHI   RE,1                                                             
         AHI   R0,1                                                             
         BCT   RF,VP208                                                         
*                                                                               
VP210    ST    R2,FADRH                                                         
         STC   R0,FERRDSP          SET DISPLACEMENT TO NON-NUMERIC              
         MVI   FERN,10                                                          
         B     EXITL                                                            
*                                                                               
VP212    DS    0H                                                               
         LHI   RE,1                                                             
         ICM   RF,15,PSLEN                                                      
         CLI   INCORE,YES          LOOKING AT CORE?                             
         BNE   *+8                 NO                                           
         ICM   RF,15,=AL4(SRCHCLQ) YES: SEARCH THE MAXIMUM LENGTH               
         AR    RF,R3                                                            
         BCTR  RF,0                                                             
*                                                                               
         CLI   BYTE,C'L'           "LEVEL=" SEARCH?                             
         BNE   VP220                                                            
         CLC   =C'LEVEL=',0(R3)                                                 
         BE    *+16                                                             
         BXLE  R3,RE,*-10          NEXT SEARCH                                  
         MVI   FERN,11                                                          
         B     EXITL                                                            
         BCT   R0,*-12             LOOP ROUND FOR NUMBER OF 'LEVEL'S            
         B     VP226                                                            
*                                                                               
VP220    DS    0H                                                               
         CLI   BYTE,C'B'           "BOOK=" SEARCH?                              
         BNE   VP222                                                            
         CLC   =C'BOOK=',0(R3)                                                  
         BE    *+16                                                             
         BXLE  R3,RE,*-10          NEXT SEARCH                                  
         MVI   FERN,19                                                          
         B     EXITL                                                            
         BCT   R0,*-12             LOOP ROUND FOR NUMBER OF 'BOOK'S             
         B     VP226                                                            
*                                                                               
VP222    DS    0H                                                               
         CLI   BYTE,C'K'           "*LNKSTAMP*" SEARCH?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'*LNKSTAMP*',0(R3)                                             
         BE    *+16                                                             
         BXLE  R3,RE,*-10          NEXT SEARCH                                  
         MVI   FERN,18                                                          
         B     EXITL                                                            
*                                                                               
VP226    MVI   INLVL,YES                                                        
         AHI   RF,1                                                             
         SR    RF,R3                                                            
         STH   RF,LENGTH           SET DISPLAY LENGTH                           
         ST    R3,START                                                         
         S     R3,APHASE                                                        
         ST    R3,OFFSET           SET OFFSET                                   
         B     EXITOK                                                           
*                                                                               
VP228    CLI   FHIL,8              TOO MUCH INPUT?                              
         BNH   *+12                NO                                           
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         TM    FHII,FHIIHE         VALID HEX?                                   
         BZ    VP236               NO                                           
*                                                                               
         MVC   DUB,ZEROS           JUSTIFY INPUT INTO DUB                       
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         LHI   R1,8                                                             
         SR    R1,RF                                                            
         LA    R1,DUB(R1)                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),FHDA                                                     
*                                                                               
         GOTO1 AHEXIN,PLIST,DUB,OFFSET,8                                        
         CLI   INCORE,YES                                                       
         BNE   VP234                                                            
*                                                                               
         MVC   DUB+0(4),ST31       SET XA START/END ADDRESSES                   
         MVC   DUB+4(4),EN31                                                    
         CLI   OFFSET,0            WANT XA?                                     
         BNE   VP230               YES                                          
         MVC   DUB+0(4),ST24                                                    
         MVC   DUB+4(4),EN24                                                    
*                                                                               
VP230    ICM   RF,15,ACORE         KEYWORD SETS ACORE                           
         A     RF,OFFSET                                                        
         STCM  RF,15,START         SET START ADDRESS                            
*                                                                               
         CLC   START,DUB+0         ENSURE WITHIN RANGE                          
         BL    VP232                                                            
         CLC   START,DUB+4                                                      
         BH    VP232                                                            
*                                                                               
         MVC   LENGTH,=AL2(MAXLEN) SET DEFAULT DISPLAY LENGTH                   
         ICM   RF,15,DUB+4                                                      
         S     RF,START                                                         
         C     RF,=AL4(MAXLEN)     LESS THAN (MAXLEN) FROM END                  
         BH    EXITOK              NO                                           
         STH   RF,LENGTH           SET DISPLAY LENGTH                           
         B     EXITOK                                                           
*                                                                               
VP232    MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
VP234    ICM   RF,15,PSLEN         MAKE SURE OFFSET WITHIN PHASE LENGTH         
         S     RF,OFFSET                                                        
         BNM   *+12                                                             
         MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
         STH   RF,LENGTH           SAVE DISPLAY LENGTH                          
         L     RF,APHASE                                                        
         A     RF,OFFSET                                                        
         ST    RF,START            SET START ADDRESS                            
         B     EXITOK              AND EXIT                                     
*                                                                               
VP236    XR    RF,RF               INPUT IS NOT VALID HEX                       
         IC    RF,FHIL             FIND FIRST BAD BYTE                          
         XR    R0,R0                                                            
         LA    RE,FHDA                                                          
*                                                                               
VP238    CLI   0(RE),C'9'                                                       
         BH    VP242                                                            
         CLI   0(RE),C'0'                                                       
         BNL   VP240                                                            
         CLI   0(RE),C'A'                                                       
         BL    VP242                                                            
         CLI   0(RE),C'F'                                                       
         BH    VP242                                                            
*                                                                               
VP240    AHI   RE,1                                                             
         AHI   R0,1                                                             
         BCT   RF,VP238                                                         
*                                                                               
VP242    STC   R0,FERRDSP          SET DISPLACEMENT TO NON-HEXADEXIMAL          
         MVI   FERN,12                                                          
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE P3 INPUT                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALP3    NTR1  ,                                                                
         MVI   WPATCH,NO                                                        
         MVI   WSRCH,NO                                                         
         LA    R2,SRVP3H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0              INPUT TO THIS FIELD IS OPTIONAL              
         BE    EXITOK                                                           
*                                                                               
         CLI   INLVL,YES           DOING LEVEL LOOP?                            
         BE    EXITOK              YES                                          
*                                                                               
         CLI   FHDA,C'P'           LOOKING TO PATCH?                            
         BNE   VP302               NO                                           
         MVI   WPATCH,YES                                                       
         B     EXITOK                                                           
*                                                                               
VP302    CLI   FHIL,3              NEED AT LEAST C'X' = 4 CHARACTERS            
         BH    *+12                                                             
         MVI   FERN,4                                                           
         B     EXITL                                                            
*                                                                               
         CLC   FHDA(2),=CL2'C'''                                                
         BE    VP304                                                            
         CLC   FHDA(2),=CL2'X'''                                                
         BE    VP306                                                            
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
VP304    XR    RF,RF               MAKE SURE THERE IS A ' AT THE END            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         LA    RE,FHDA(RF)                                                      
         CLI   0(RE),C''''                                                      
         BE    *+12                                                             
         MVI   FERN,15                                                          
         B     EXITL                                                            
*                                                                               
         AHI   RF,-2               GET LENGTH OF SEARCH STRING                  
         STC   RF,STRLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRCHSTR(0),FHDA+2   SAVE SEARCH STRING ITSELF                    
         B     VP316                                                            
*                                                                               
VP306    CLI   FHIL,4              NEED AT LEAST X'XX' = 5 CHARACTERS           
         BH    *+12                                                             
         MVI   FERN,4                                                           
         B     EXITL                                                            
*                                                                               
         XR    RF,RF               MAKE SURE THERE IS A ' AT THE END            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         LA    RE,FHDA(RF)                                                      
         CLI   0(RE),C''''                                                      
         BE    *+12                                                             
         MVI   FERN,15                                                          
         B     EXITL                                                            
*                                                                               
         TM    FHIL,X'01'                                                       
         BNZ   VP308                                                            
         MVI   FERRDSP,2                                                        
         MVI   FERN,16             NEED EVEN NUMBER OF VALID HEX DIGITS         
         B     EXITL                                                            
*                                                                               
VP308    AHI   RF,-2                                                            
         LR    R0,RF               SAVE LENGTH                                  
         GOTO1 AHEXIN,DMCB,FHDA+2,SRCHSTR,(RF)                                  
         ICM   RF,15,12(R1)                                                     
         BZ    *+12                                                             
         STC   RF,STRLEN                                                        
         B     VP316                                                            
*                                                                               
         LR    RF,R0               INPUT IS NOT VALID HEX                       
         XR    R0,R0               FIND FIRST BAD BYTE                          
         LA    RE,FHDA+2                                                        
*                                                                               
VP310    CLI   0(RE),C'9'                                                       
         BH    VP314                                                            
         CLI   0(RE),C'0'                                                       
         BNL   VP312                                                            
         CLI   0(RE),C'A'                                                       
         BL    VP314                                                            
         CLI   0(RE),C'F'                                                       
         BH    VP314                                                            
*                                                                               
VP312    AHI   RE,1                                                             
         AHI   R0,1                                                             
         BCT   RF,VP310                                                         
*                                                                               
VP314    STC   R0,FERRDSP          SET DISPLACEMENT TO NON-HEXADEXIMAL          
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
VP316    CLI   INCORE,YES                                                       
         BE    VP318                                                            
         ICM   RF,15,PSLEN         SET MAX SEARCH LENGTH                        
         S     RF,OFFSET                                                        
         STCM  RF,15,SRCHLEN                                                    
         B     VP320                                                            
*                                                                               
VP318    MVC   SRCHLEN,=AL4(SRCHCLQ)                                            
         ICM   RF,15,OFFSET                                                     
         A     RF,ACORE                                                         
         ICM   R0,15,EN31                                                       
         TMH   RF,X'FF00'                                                       
         BNZ   *+8                                                              
         ICM   R0,15,EN24                                                       
         SR    R0,RF                                                            
         C     R0,=AL4(SRCHCLQ)                                                 
         BH    VP320                                                            
         ST    R0,SRCHLEN                                                       
*                                                                               
VP320    MVI   WSRCH,YES                                                        
         BRAS  RE,SEARCH           DO THE SEARCH                                
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CALL CALLOV TO LOAD PHASE                                           *         
***********************************************************************         
         SPACE 1                                                                
LOADIT   NTR1  ,                                                                
         MVC   TLANG,DDSLANG                                                    
         MVC   SAVETEST,TTEST      SAVE OUR TTEST INFO                          
         MVC   SAVETST1,TTEST1                                                  
         MVC   TTEST,DDSLVL                                                     
         OI    TTEST1,TTESTURT     SET USE REAL TTEST                           
*NOPAHYD OI    TTEST,TTESTSRA      *TEMP*                                       
*                                                                               
         ICM   RF,15,VLOADPNT                                                   
         A     RF,RELO                                                          
         ICM   R0,7,PSNAME                                                      
         ICM   R0,8,=C'R'                                                       
         GOTO1 ACALLOV,DMCB,(RF),(R0),0                                         
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TLANG,0             RESET LANGUAGE                               
         MVC   TTEST,SAVETEST      RESET TTEST                                  
         MVC   TTEST1,SAVETST1                                                  
         MVC   START,0(R1)         SET START ADDRESS                            
         MVC   APHASE,0(R1)                                                     
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY PHASE/CORE FROM OFFSET UNTIL END OF EITHER PHASE OR SCREEN  *         
***********************************************************************         
         SPACE 1                                                                
DISP     NTR1  ,                                                                
         MVI   DDDSP,YES                                                        
         MVI   DDHEX,YES                                                        
         MVI   DDALF,YES                                                        
         BRAS  RE,DIS                                                           
*                                                                               
         LA    RF,SRVL1HH          SET CURSOR ON FIRST LINE                     
         ST    RF,FADRH                                                         
         CLI   WPATCH,YES          PATCH AFTER DISPLAY ?                        
         BNE   DISP02                                                           
*                                                                               
         BRAS  RE,CONTROL          CHECK FOR PROFSID                            
         BNE   EXITL                                                            
         LA    RF,SRVL1HH          SET CURSOR ON FIRST LINE                     
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
*                                                                               
DISP02   CLI   WSRCH,YES           SEARCH?                                      
         BNE   EXITOK                                                           
         CLI   SRCHIND,YES         SEARCH WAS GOOD?                             
         BNE   DISP04                                                           
         MVI   HDRN,3              SEARCH SUCCESSFUL                            
*                                                                               
         LA    R2,SRVL2OH          HIGHLIGHT LINE 2                             
         USING FHD,R2                                                           
         OI    FHAT,FHATHI                                                      
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         OI    FHAT,FHATHI                                                      
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         OI    FHAT,FHATHI                                                      
         LA    RF,SRVP4H           SET CURSOR AT P4                             
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
*                                                                               
DISP04   LA    RF,SRVP4H           SET CURSOR AT P4                             
         ST    RF,FADRH                                                         
         MVI   HDRN,4              SEARCH UNSUCCESS.: STRING NOT FOUND          
         CLI   INCORE,YES          IN CORE?                                     
         BNE   EXITOK              NO                                           
         MVI   HDRN,5              SEARCH UNSUCCESS.: HIT ENTER TO CONT         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DO DISPLAY TO SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
DIS      NTR1  ,                                                                
         TWAXC SRVL1AH,SRVDUMH                                                  
*                                                                               
         LA    R2,SRVL1AH                                                       
         USING FHD,R2              R2=A(SCR FLD HDR)                            
         ICM   RE,15,OFFSET        SET HEADER NUMBERS PROPERLY                  
         SRDL  RE,4                                                             
         SRL   RF,32-4                                                          
         LA    RE,HDRHEX(RF)                                                    
         MVC   SRVH1A,0(RE)                                                     
         OI    SRVH1AH+(FHOI-FHD),FHOITR                                        
         SLL   RF,1                                                             
         LA    RE,HDRDEC(RF)                                                    
         MVC   SRVH1H,0(RE)                                                     
         OI    SRVH1HH+(FHOI-FHD),FHOITR                                        
*                                                                               
         MVC   FULL,OFFSET                                                      
         XR    R0,R0                                                            
         ICM   R0,3,LENGTH                                                      
         ST    R0,FULL1                                                         
         LA    R2,SRVL1OH                                                       
         ICM   R5,15,START         R5=A(DATA)                                   
*                                                                               
DIS02    ICM   R3,15,FULL1         SET R3 TO DATA LENGTH                        
         BZ    EXITOK                                                           
         CHI   R3,L'SRVL1A                                                      
         BL    *+8                                                              
         LHI   R3,L'SRVL1A         OR LINE LENGTH                               
*                                                                               
         SAM31 ,                                                                
         XC    WORK,WORK           MOVE DATA TO WORK (IN CASE IN XA)            
         LR    RF,R3                                                            
         AHI   RF,-1                                                            
         BM    DIS04                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R5)                                                    
*                                                                               
DIS04    SAM24 ,                                                                
         CLI   DDDSP,YES           DISPLAY DISPLACEMENTS?                       
         BNE   DIS06               NO                                           
*                                                                               
         GOTO1 AHEXOUT,PLIST,FULL,FHDA,4,0                                      
         MVI   FHDA+8,C'-'                                                      
         L     R0,FULL                                                          
         AR    R0,R3                                                            
         BCTR  R0,0                                                             
         ST    R0,FULL                                                          
         GOTO1 AHEXOUT,PLIST,FULL,FHDA+9,4                                      
         AHI   R0,1                                                             
         ST    R0,FULL             BUMP OFFSET FOR NEXT LINE                    
*                                                                               
DIS06    XR    R0,R0               NEXT FIELD (HEX OUTPUT)                      
         IC    R0,FHLN                                                          
         AR    R2,R0                                                            
         CLI   DDHEX,YES           DISPLAY HEX?                                 
         BNE   DIS08                                                            
*                                                                               
         BRAS  RE,FHDACLR                                                       
         GOTO1 AHEXOUT,PLIST,(R5),(X'80',FHDA),(R3),0                           
         OI    FHOI,FHOITR                                                      
*                                                                               
DIS08    XR    R0,R0               NEXT FIELD (ALPHA OUTPUT)                    
         IC    R0,FHLN                                                          
         AR    R2,R0                                                            
         CLI   DDALF,YES           DISPLAY ALPHA?                               
         BNE   DIS10               NO                                           
*                                                                               
         BRAS  RE,FHDACLR                                                       
         LARL  RE,DDTRTBL                                                       
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         SAM31 ,                                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),0(R5)       MOVE IN ALPHA                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         TR    FHDA(0),0(RE)       AND TRANSLATE IT                             
         OI    FHOI,FHOITR                                                      
*                                                                               
DIS10    AR    R5,R3               NEXT DATA                                    
         SAM24 ,                                                                
*                                                                               
         XR    R0,R0               NEXT LINE                                    
         IC    R0,FHLN                                                          
         AR    R2,R0                                                            
         LA    RF,SRVDUMH          STILL WITHIN DISPLAY LOGIC?                  
         CR    R2,RF                                                            
         BNL   EXITOK              YES                                          
*                                                                               
         L     RF,FULL1            DECREMENT DATA LEFT TO DISPLAY               
         SR    RF,R3                                                            
         BNP   EXITOK                                                           
         ST    RF,FULL1                                                         
         B     DIS02                                                            
*                                                                               
FHDACLR  XR    RF,RF               ROUTINE TO CLEAR FIELD                       
         IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RF,-(FHDAD)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FHDA(0),FHDA                                                     
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PATCH PHASE/CORE FROM OFFSET UNTIL END OF EITHER PHASE OR SCREEN    *         
***********************************************************************         
         SPACE 1                                                                
PATCH    NTR1  ,                                                                
         BRAS  RE,CONTROL                                                       
         BNE   EXITL                                                            
         BRAS  RE,VAL                                                           
         BNE   EXITL                                                            
         BRAS  RE,WTO                                                           
*                                                                               
         SAM31 ,                                                                
         ICM   R2,15,START                                                      
         XR    RF,RF                                                            
         ICM   RF,3,DATA+0                                                      
         AR    R2,RF               R2 = A(PATCH POINT)                          
         ICM   RF,3,DATA+2                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),DATA+4                                                   
         SAM24 ,                                                                
*                                                                               
         CLI   INCORE,YES          PATCHING CORE?                               
         BE    PATCHX                                                           
*                                                                               
         LA    RF,PSREC                                                         
         ICM   RF,8,=C'W'                                                       
         GOTO1 ACALLOV,DMCB,APHASE,(RF),0                                       
         CLI   4(R1),0                                                          
         BE    PATCHX                                                           
         DC    H'0'                                                             
*                                                                               
PATCHX   MVI   HDRN,7              SET PATCHED MESSAGE                          
         LA    RF,SRVL1HH          SET CURSOR ON FIRST LINE                     
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE A SCREENS WORTH OF HEX INFORMATION                         *         
***********************************************************************         
         SPACE 1                                                                
VAL      NTR1  ,                                                                
         LA    R2,SRVL1HH                                                       
         USING FHD,R2              R2=A(SCR FLD HDR)                            
         LA    R0,SRVDUMH                                                       
         LA    R3,DATAC                                                         
         XR    RF,RF                                                            
VAL02    IC    RF,FHLN             POPULATE CHARACTER BUFFER                    
         LR    R1,RF                                                            
         AHI   R1,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   R1,-(FHDAD)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FHDA        SAVE FIELD CONTENTS                          
         LA    R3,1(R1,R3)         NEXT IN BUFFER                               
         AHI   R2,SRVL2OH-SRVL1OH  NEXT FIELD ON SCREEN                         
         CR    R2,R0               END OF SCREEN?                               
         BL    VAL02               NO                                           
*                                                                               
         LA    R2,SRVL1HH          QUICK CHECK TO SEE IF ALL IS HEX             
         LA    R0,SRVDUMH                                                       
VAL04    CLI   FHIL,0                                                           
         BE    *+12                                                             
         TM    FHII,FHIIHE                                                      
         BZ    VAL06                                                            
         AHI   R2,SRVL2OH-SRVL1OH  NEXT FIELD ON SCREEN                         
         CR    R2,R0                                                            
         BL    VAL04                                                            
         B     VAL08               SCREEN IS ALL VALID HEX                      
         DROP  R2                                                               
*                                                                               
VAL06    BRAS  RE,CHEVRON          SEE IF THIS WAS A CHEVRON                    
         BL    EXITL               NO - SOME KIND OF ERROR                      
         B     EXITOK                                                           
*                                                                               
VAL08    LA    RF,DATAC            COUNT HEX CHARACTERS                         
         XR    R0,R0                                                            
*                                                                               
VAL10    CLI   0(RF),C' '                                                       
         BNH   VAL12                                                            
         CHI   R0,MAXLEN*2                                                      
         BH    VAL12                                                            
         AHI   R0,1                                                             
         AHI   RF,1                                                             
         B     VAL10                                                            
*                                                                               
VAL12    GOTO1 AHEXIN,DMCB,DATAC,DATA+4,(R0)                                    
         ICM   R0,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R0,3,DATA+2         SET LENGTH TO PATCH                          
         LHI   RF,0                                                             
         STCM  RF,3,DATA           SET DISPLACEMENT TO PATCH                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR PATCHES SMALLER THAN THE SCREEN                *         
***********************************************************************         
         SPACE 1                                                                
CHEVRON  NTR1  ,                                                                
         XC    FULL,FULL                                                        
         XC    FULL1,FULL1                                                      
         LA    R2,DATAC                                                         
         LA    R3,DATAC+DATACL                                                  
         LHI   R0,C'<'                                                          
CHV04    SRST  R3,R2               LOOK FOR OPENING CHEVRON                     
         BC    1,CHV04                                                          
         BC    2,*+8               NO OPENING CHEVRON                           
         ST    R3,FULL             SAVE A(OPENING CHEVRON)                      
*                                                                               
         LA    R2,DATAC                                                         
         LA    R3,DATAC+DATACL                                                  
         LHI   R0,C'>'                                                          
CHV06    SRST  R3,R2               LOOK FOR CLOSING CHEVRON                     
         BC    1,CHV06                                                          
         BC    2,*+8               NO CLOSING CHEVRONS                          
         ST    R3,FULL1            SAVE A(BEFORE CLOSING CHEVRON)               
*                                                                               
         ICM   RF,15,FULL          A(OPENING)                                   
         BNZ   CHV10                                                            
         ICM   RE,15,FULL1         A(CLOSE)                                     
         BNZ   CHV08                                                            
         MVI   FERN,12             INVALID CHARACTER IN INPUT STRING            
         MVI   BYTE,C'!'           FIND FIRST NOT CHEVRON OR VALID HEX          
         B     CHVFCHR                                                          
*                                                                               
CHV08    MVI   FERN,22             CLOSE BUT NO OPEN                            
         MVI   BYTE,C'>'                                                        
         B     CHVFCHR                                                          
*                                                                               
CHV10    ICM   RE,15,FULL1         A(CLOSE)                                     
         BNZ   CHV12                                                            
         MVI   FERN,21                                                          
         MVI   BYTE,C'<'                                                        
         B     CHVFCHR             OPEN BUT NO CLOSE                            
*                                                                               
CHV12    CR    RE,RF               MAKE SURE OPEN BEFORE CLOSE                  
         BH    CHV14                                                            
         MVI   FERN,23                                                          
         MVI   BYTE,C'>'                                                        
         B     CHVFCHR             CLOSE BEFORE OPEN                            
*                                                                               
CHV14    LA    R0,DATAC            MAKE SURE DATA IS BYTE ALIGNED               
         SR    RF,R0                                                            
         TML   RF,X'0001'                                                       
         BNZ   CHV16                                                            
         MVI   FERN,24                                                          
         MVI   BYTE,C'<'                                                        
         B     CHVFCHR             OPEN IS NOT BYTE ALIGNED                     
*                                                                               
CHV16    SR    RE,R0               MAKE SURE DATA IS BYTE ALIGNED               
         TML   RE,X'0001'                                                       
         BZ    CHV18                                                            
         MVI   FERN,24                                                          
         MVI   BYTE,C'>'                                                        
         B     CHVFCHR             CLOSE IS NOT BYTE ALIGNED                    
*                                                                               
CHV18    LR    R0,RF               MUST BE AT LEAST 2 DIGITS BETWEEN            
         AHI   R0,2                                                             
         CR    RE,R0                                                            
         BH    CHV20                                                            
         MVI   FERN,25                                                          
         MVI   BYTE,C'<'                                                        
         B     CHVFCHR             CHEVRONS TOO CLOSE TOGETHER                  
*                                                                               
CHV20    AHI   RF,1                                                             
         SRL   RF,1                                                             
         STCM  RF,3,DATA           SET DISPLACEMENT TO START OF DATA            
         SLL   RF,1                                                             
         LR    R0,RE                                                            
         SR    R0,RF               GET LENGTH OF DATA                           
         L     RF,FULL                                                          
         AHI   RF,1                                                             
         GOTO1 AHEXIN,DMCB,(RF),DATA+4,(R0),0                                   
         ICM   R0,15,12(R1)                                                     
         BNZ   CHV22                                                            
         MVI   FERN,12                                                          
         MVI   BYTE,C'!'           FIND FIRST NOT CHEVRON OR VALID HEX          
         B     CHVFCHR                                                          
*                                                                               
CHV22    STCM  R0,3,DATA+2         SET LENGTH OF DATA                           
         B     EXITOK                                                           
*                                                                               
CHVFCHR  LA    R3,SRVL1HH                                                       
         USING FHD,R3                                                           
         LA    R0,SRVDUMH                                                       
         XR    RF,RF                                                            
         XR    RE,RE                                                            
CHVF02   IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RF,-(FHDAD)                                                      
*                                                                               
         LA    R1,FHDA             SEE IF CHARACTER IS IN THIS FIELD            
*                                                                               
CHVF04   CLI   BYTE,C'!'           WANT BAD HEX?                                
         BNE   CHVF10              NO                                           
*                                                                               
         CLI   0(R1),C'<'          CHEVRONS ARE SPECIAL                         
         BNE   CHVF06                                                           
         TML   RE,X'00FF'          FOUND OPENER BEFORE?                         
         BNZ   CHVF14              YES                                          
         ICM   RE,1,=AL1(255)      SAVE FOUND OPENER                            
         B     CHVF12                                                           
*                                                                               
CHVF06   CLI   0(R1),C'>'                                                       
         BNE   CHVF08                                                           
         TMH   RE,X'FF00'          FOUND CLOSER BEFORE?                         
         BNZ   CHVF14              YES                                          
         ICM   RE,8,=AL1(255)      SAVE FOUND CLOSER                            
         B     CHVF12                                                           
*                                                                               
CHVF08   CLI   0(R1),C'A'          LOOK FOR FIRST NON-HEX CHARACTER             
         BL    CHVF14                                                           
         CLI   0(R1),C'F'                                                       
         BNH   CHVF12                                                           
         CLI   0(R1),C'0'                                                       
         BL    CHVF14                                                           
         CLI   0(R1),C'9'                                                       
         BH    CHVF14                                                           
         B     CHVF12              THIS ONE IS HEX ALRIGHT                      
*                                                                               
CHVF10   CLC   BYTE,0(R1)          COMPARE FOR WANTED CHARACTER                 
         BE    CHVF14                                                           
*                                                                               
CHVF12   AHI   R1,1                                                             
         BCT   RF,CHVF04                                                        
*                                                                               
         AHI   R3,SRVL2OH-SRVL1OH  NEXT FIELD ON SCREEN                         
         CR    R3,R0               END OF SCREEN?                               
         BL    CHVF02              NO                                           
         DC    H'0'                WHY WAS I CALLED?                            
*                                                                               
CHVF14   ST    R3,FADRH            SET FIELD                                    
         LA    R0,FHDA             SET DISPLACEMENT TO CHARACTER                
         SR    R1,R0                                                            
         STC   R1,FERRDSP                                                       
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR USERID AND WRITE LOG MESSAGES                             *         
***********************************************************************         
         SPACE 1                                                                
CONTROL  NTR1  ,                                                                
         CLI   PRODSYS,C'Y'        DONT DO THIS FOR TEST SYSTEMS                
         BNE   EXITOK                                                           
*                                                                               
         MVC   PROFSID,SPACES      P4=PROFSID(,PASSWORD)                        
         MVC   PASSWORD,SPACES                                                  
         LA    R2,SRVP4H                                                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,26             ID REQUIRED FOR PATCH                        
         B     EXITL                                                            
         DROP  R2                                                               
*                                                                               
         GOTO1 ASCANNER,DMCB,SRVP4H,(X'82',WORK)                                
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   FERN,27                                                          
         B     EXITL                                                            
*                                                                               
         LA    R2,WORK                                                          
         USING SCANBLKD,R2                                                      
         CLI   SC2NDLEN,0          SECOND FIELD NOT ALLOWED                     
         BE    CNT02                                                            
         MVI   FERN,27                                                          
         MVC   FERRDSP,SC2NDNUM                                                 
         B     EXITL                                                            
*                                                                               
CNT02    CLI   SC1STLEN,8          INPUT LENGTH OK?                             
         BNH   CNT04                                                            
         MVI   FERN,3                                                           
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
CNT04    MVC   PROFSID,SC1STFLD                                                 
         CLI   4(R1),2             PASSWORD FIELD?                              
         BNE   EXITOK              NO                                           
*                                                                               
         AHI   R2,SCBLKLQ                                                       
         CLI   SC2NDLEN,0          SECOND FIELD NOT ALLOWED                     
         BE    CNT06                                                            
         MVI   FERN,27                                                          
         MVC   FERRDSP,SC2NDNUM                                                 
         B     EXITL                                                            
*                                                                               
CNT06    CLI   SC1STLEN,8          INPUT LENGTH OK?                             
         BNH   CNT08                                                            
         MVI   FERN,3                                                           
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
CNT08    MVC   PASSWORD,SC1STFLD                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
***********************************************************************         
         SPACE 1                                                                
WTO      NTR1  ,                                                                
*                                                                               
         MVC   OPERID,=CL9'+PATCH+' SET UP OPER MSG                             
         MVC   OPERP1,SRVP1        SHOW P1 TO P3                                
         MVC   OPERP2,SRVP2                                                     
         MVC   OPERP3,SRVP3                                                     
         MVC   OPERP4,SRVP4                                                     
         MVC   OPERFAC,=C'(FACPAK)'                                             
         MVC   OPERFAC+4(3),SYSSHRT                                             
*                                                                               
         CLI   PRODSYS,C'N'        ONLY FOR PROD SYSTEMS                        
         BE    WTO02                                                            
*??      OC    LOGID,LOGID         TEST FOR LOG RECORD                          
*??      BZ    WTO02                                                            
         MVC   LOGID,=C'$PA.'      SET UP LOGREC                                
         MVC   LOGLUID,TSYM                                                     
         MVC   LOGTIME,SAVETIME                                                 
         MVC   LOGPHS,SRVP1                                                     
         MVC   LOGOFFS,SRVP2                                                    
         MVC   LOGUSER,SRVP4                                                    
         GOTO1 ALOGGER,LOGREC      LOG TO ADRFILE                               
*                                                                               
WTO02    OC    OPERMSG,SPACES                                                   
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPERMSG,OPERMSGL,C'LVL1'                   
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PFKEY PRESS                                                *         
* EXIT:  CC HI = IGNORE VALIDATION OF P3 (PFKEY WAS PRESSED)          *         
***********************************************************************         
         SPACE 1                                                                
VALPFK   NTR1  ,                                                                
         CLI   PFKEY,7             CHECK SCROLL PFKEY PRESSED                   
         BL    EXITOK                                                           
         CLI   PFKEY,8                                                          
         BH    EXITOK                                                           
*                                                                               
         CLI   PFKEY,8                                                          
         BNE   VPFK04                                                           
         CLI   INCORE,YES                                                       
         BE    VPFK02                                                           
*                                                                               
         L     RF,OFFSET           SCROLL DOWN A SCREEN LESS 1 LINE             
         AHI   RF,MAXLEN-16                                                     
         CLM   RF,15,PSLEN         MAKE SURE NOT PAST END                       
         BH    VPFK10                                                           
*                                                                               
         ST    RF,OFFSET           SET NEW OFFSET                               
         A     RF,APHASE                                                        
         ST    RF,START            SET NEW START                                
*                                                                               
         MVC   LENGTH,=AL2(MAXLEN)                                              
         ICM   RF,15,PSLEN                                                      
         S     RF,OFFSET                                                        
         C     RF,=AL4(MAXLEN)                                                  
         BH    VPFK08                                                           
         STH   RF,LENGTH                                                        
         B     VPFK08                                                           
*                                                                               
VPFK02   ICM   RF,15,OFFSET        SCROLL DOWN A SCREEN LESS 1 LINE             
         AHI   RF,MAXLEN-16                                                     
         MVC   DUB(4),EN31                                                      
         CLI   OFFSET,0                                                         
         BNE   *+10                                                             
         MVC   DUB(4),EN24                                                      
         CLC   OFFSET,DUB                                                       
         BH    VPFK10                                                           
*                                                                               
         STCM  RF,15,OFFSET        SET NEW OFFSET                               
         A     RF,ACORE                                                         
         STCM  RF,15,START         SET NEW START                                
*                                                                               
         MVC   LENGTH,=AL2(MAXLEN) SET NEW DISPLAY LENGTH                       
         ICM   RF,15,DUB                                                        
         S     RF,OFFSET                                                        
         C     RF,=AL4(MAXLEN)                                                  
         BH    VPFK08                                                           
         STH   RF,LENGTH                                                        
         B     VPFK08                                                           
*                                                                               
VPFK04   CLI   INCORE,YES                                                       
         BE    VPFK06                                                           
         L     RF,OFFSET           SCROLL UP A SCREEN LESS 1 LINE               
         AHI   RF,-(MAXLEN-16)                                                  
         BP    *+6                                                              
         XR    RF,RF                                                            
*                                                                               
         STCM  RF,15,OFFSET        SET NEW OFFSET                               
         A     RF,APHASE                                                        
         STCM  RF,15,START         SET NEW START                                
*                                                                               
         MVC   LENGTH,=AL2(MAXLEN)                                              
         ICM   RF,15,PSLEN                                                      
         S     RF,OFFSET                                                        
         C     RF,=AL4(MAXLEN)                                                  
         BH    VPFK08                                                           
         STH   RF,LENGTH                                                        
         B     VPFK08                                                           
*                                                                               
VPFK06   ICM   RF,15,OFFSET         SCROLL UP A SCREEN LESS 1 LINE              
         A     RF,ACORE                                                         
         AHI   RF,-(MAXLEN-16)                                                  
*                                                                               
         ICM   R0,15,ST31                                                       
         TMH   RF,X'FF00'           RF HAS XA ADDRESS?                          
         BNZ   *+8                  YES                                         
         ICM   R0,15,ST24                                                       
         CR    RF,R0                                                            
         BH    *+6                                                              
         LR    RF,R0                                                            
         A     RF,ACORE                                                         
         STCM  RF,15,OFFSET        SET OFFSET                                   
         A     RF,ACORE                                                         
         STCM  RF,15,START         SET NEW START                                
         B     VPFK08                                                           
*                                                                               
VPFK08   MVC   SRVP2,SPACES                                                     
         LA    R2,SRVP2H                                                        
         USING FHD,R2                                                           
         LHI   R0,4                                                             
         LA    RF,OFFSET                                                        
         GOTO1 AHEXOUT,DMCB,(RF),FHDA,4,0                                       
         MVI   FHOL,8                                                           
         OI    FHOI,FHOITR                                                      
*                                                                               
         CLI   FHDA,C'0'           STRIP LEADING SPACES                         
         BNE   *+14                                                             
         MVC   FHDA(8),FHDA+1                                                   
         B     *-14                                                             
*                                                                               
         CLI   FHDA,C' '                                                        
         BH    *+8                                                              
         MVI   FHDA,C'0'                                                        
*                                                                               
VPFK10   LA    RF,SRVP3H                                                        
         ST    RF,FADRH                                                         
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* SEARCH FOR STRING STRING IN CORE/PHASE.                             *         
***********************************************************************         
         SPACE 1                                                                
SEARCH   NTR1  ,                                                                
         XR    R1,R1               R1 = STRING LENGTH                           
         IC    R1,STRLEN                                                        
         BCTR  R1,0                                                             
*                                                                               
         L     R2,START            SET UP BXLE LOOP R2/RE/RF                    
         LHI   RE,1                                                             
         LR    RF,R2                                                            
         A     RF,SRCHLEN                                                       
         BCTR  RF,0                                                             
*                                                                               
         EX    R1,SRCHCLC          TRY TO FIND STRING                           
         BE    SRCH02                                                           
         BXLE  R2,RE,*-8                                                        
*                                                                               
         MVI   SRCHIND,NO          NOT FOUND                                    
         ST    R2,START            SAVE A(LAST BYTE SEARCHED)                   
         BCTR  R2,0                                                             
         ST    R2,FNDADR           FNDADR=END OF SEARCH OFFSET                  
         B     SRCH04                                                           
*                                                                               
SRCHCLC  CLC   SRCHSTR(0),0(R2)                                                 
*                                                                               
SRCH02   MVI   SRCHIND,YES         FOUND STRING                                 
         ST    R2,FNDADR           A(MATCHING STRING)                           
         AHI   R2,-16                                                           
*                                                                               
         CLI   INCORE,YES          IS THIS A CORE SEARCH?                       
         BE    SRCH02A             YES                                          
         C     R2,APHASE                                                        
         BH    *+8                                                              
         L     R2,APHASE                                                        
         ST    R2,START            DISPLAY IN CONTEXT                           
         B     SRCH03                                                           
*                                                                               
SRCH02A  ICM   R0,15,ACORE         KEYWORD DISPLAY?                             
         BZ    SRCH02B                                                          
         CR    R2,R0                                                            
         BH    *+6                                                              
         LR    R2,R0                                                            
         ST    R2,START            DISPLAY IN CONTEXT                           
         B     SRCH03                                                           
*                                                                               
SRCH02B  ICM   R0,15,OFFSET        REGULAR CORE DISPLAY?                        
         CR    R2,R0                                                            
         BH    *+6                                                              
         LR    R2,R0                                                            
         ST    R2,START            DISPLAY IN CONTEXT                           
         B     SRCH03                                                           
*                                                                               
SRCH03   AHI   RF,1                RF = LENGTH TO END OF SEARCH                 
         CLI   INCORE,YES          IS THIS A CORE SEARCH?                       
         BNE   SRCH04              NO                                           
         ICM   RF,15,EN31          SET RF TO END OF REGION                      
         CLI   FNDADR,0                                                         
         BNE   *+8                                                              
         ICM   RF,15,EN24                                                       
*                                                                               
SRCH04   S     RF,FNDADR                                                        
         MVC   LENGTH,=AL2(MAXLEN) SET LENGTH TO DISPLAY                        
         CHI   RF,MAXLEN                                                        
         BL    *+8                                                              
         LHI   RF,MAXLEN                                                        
         STH   RF,LENGTH                                                        
*                                                                               
         ICM   R0,15,ACORE                                                      
         CLI   INCORE,YES          IS THIS A CORE SEARCH?                       
         BE    *+8                 YES                                          
         ICM   R0,15,APHASE                                                     
*                                                                               
         ICM   RF,15,START         SET OFFSET FOR DISPLAY                       
         SR    RF,R0                                                            
         ST    RF,OFFSET                                                        
*                                                                               
         ICM   RF,15,FNDADR        SHOW OFFSET FOR NEXT SEARCH                  
         AHI   RF,1                                                             
         SR    RF,R0                                                            
         ST    RF,FNDADR                                                        
*                                                                               
         MVC   SRVP2,SPACES        DISPLAY NEW OFFSET                           
         LA    R2,SRVP2H                                                        
         USING FHD,R2                                                           
         LA    RF,FNDADR                                                        
         GOTO1 AHEXOUT,DMCB,(RF),FHDA,4,0                                       
         MVI   FHOL,8                                                           
         OI    FHOI,FHOITR                                                      
*                                                                               
         CLI   FHDA,C'0'           STRIP LEADING SPACES                         
         BNE   *+14                                                             
         MVC   FHDA(8),FHDA+1                                                   
         B     *-14                                                             
*                                                                               
         CLI   FHDA,C' '           JUST IN CASE ALL ZEROS                       
         BH    *+8                                                              
         MVI   FHDA,C'0'                                                        
*                                                                               
         LA    RF,SRVP4H                                                        
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISERR   NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'E#'        SET ERROR                                  
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,FERN                                                          
         CHI   R0,255                                                           
         BNE   *+6                                                              
         XR    R0,R0                                                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         LTR   R0,R0               SPECIAL ERROR MESSAGE?                       
         BNE   DERR02              NO                                           
         XR    RF,RF                                                            
         ICM   RF,7,FERNA                                                       
         MVC   0(30,R4),0(RF)                                                   
         B     DERR04                                                           
*                                                                               
DERR02   AHI   R0,-1                                                            
         MHI   R0,EMSGL                                                         
         LARL  RF,ERRMSGS                                                       
         AR    RF,R0                                                            
         MVC   0(EMSGL,R4),0(RF)                                                
*                                                                               
DERR04   ICM   R2,15,FADRH         SET CURSOR ON BAD FIELD                      
         BZ    EXITOK                                                           
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY OK MESSAGE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOK    NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'I#'      SET OK                                       
         AHI   R4,2                                                             
*                                                                               
         CLI   HDRN,1              "OKAY" MESSAGE IS LEFT AS DEFAULT?           
         BNE   DISOK10             NO: USE THE ONE THAT WAS CHOSEN              
         CLI   INCORE,YES          LOOKING AT CORE?                             
         BNE   DISOK10             NO                                           
         MVI   HDRN,8              YES: SAY DATA IS FROM FACPAK REGION          
*                                                                               
DISOK10  DS    0H                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,HDRN                                                          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         AHI   R0,-1                                                            
         MHI   R0,IMSGL                                                         
         LARL  RF,OKMSGS                                                        
         AR    RF,R0                                                            
         MVC   0(IMSGL,R4),0(RF)                                                
*                                                                               
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         ICM   RF,15,FADRH                                                      
         BNZ   *+8                                                              
         LA    RF,SRVL1H                                                        
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* HELP DISPLAY ROUTINE FOR FILE NAME FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
P1HELP   NTR1  ,                                                                
         LA    R3,SRVP1H                                                        
         USING FHD,R3                                                           
         LA    R4,SRVXFKH          R4 = A(END OF SCREEN)                        
         XR    R5,R5               KEEP THIS CLEAR (MUST BE ODD)                
*                                                                               
         MVI   BYTE,1                                                           
         CLI   FHDA+1,C'1'         ONLY SUPPORT 2 HELP SCREENS                  
         BL    P1H02                                                            
         CLI   FHDA+1,C'2'                                                      
         BH    P1H02                                                            
         MVC   BYTE,FHDA+1                                                      
         NI    BYTE,X'0F'                                                       
*                                                                               
P1H02    LA    R0,SAVESCR          SAVE BOTTOM HALF OF FF SCREEN                
         LHI   R1,SAVESCRL                                                      
         LA    RE,SRVU1H                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'FE',SRVH1OH),0                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,SRVX1H                                                        
         MVC   FHDA(L'P1HL1),P1HL1 MOVE OUT CORE/COREX MESSAGE                  
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         AR    R3,R5                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
         LHI   R0,L'NNAME+1                                                     
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0               FIND OUT HOW MANY FIT ON A LINE              
         STH   RF,FULL+2           FULL+2(2) = NUMBER ON A LINE                 
*                                                                               
         MVC   FHDA(L'P1HLSYS),P1HLSYS                                          
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         AR    R3,R5                                                            
*                                                                               
         LARL  R2,SYSTAB           ADDRESSES FROM SYSFACS                       
         USING NAMED,R2                                                         
P1H04    BRAS  RE,DISONSCR         DISPLAY FROM TABLE                           
         BE    P1H06                                                            
         BRAS  RE,MYHELP           DID CORRECT HELP PAGE                        
         BE    P1H14               YES - DONE                                   
*                                                                               
         LA    R3,SRVX1H           REPEAT MESSAGE FOR THIS TABLE                
         MVC   FHDA(L'P1HLSYS),P1HLSYS                                          
         MVC   FHDA+L'P1HLSYS(L'CONTINUE),CONTINUE                              
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         BXH   R3,R5,P1H04         DISPLAY REMAINDER OF TABLE                   
*                                                                               
P1H06    IC    R5,FHLN             NEXT FIELD                                   
         AR    R3,R5                                                            
         CR    R3,R4               END OF SCREEN?                               
         BL    P1H07               NO - KEEP GOING                              
*                                                                               
         BRAS  RE,MYHELP           DID CORRECT HELP PAGE                        
         BE    P1H14               YES - DONE                                   
         LA    R3,SRVX1H           START FROM TOP AGAIN                         
*                                                                               
P1H07    MVC   FHDA(L'P1HLTCB),P1HLTCB                                          
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         AR    R3,R5                                                            
*                                                                               
         LARL  R2,TCBTAB           ADDRESSES FROM TCB                           
P1H08    BRAS  RE,DISONSCR                                                      
         BE    P1H10                                                            
         BRAS  RE,MYHELP           DID CORRECT HELP PAGE                        
         BE    P1H14               YES - DONE                                   
*                                                                               
         LA    R3,SRVX1H           REPEAT MESSAGE FOR THIS TABLE                
         MVC   FHDA(L'P1HLTCB),P1HLTCB                                          
         MVC   FHDA+L'P1HLTCB(L'CONTINUE),CONTINUE                              
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         BXH   R3,R5,P1H08         DISPLAY REMAINDER OF TABLE                   
*                                                                               
P1H10    IC    R5,FHLN             NEXT LINE ON SCREEN                          
         AR    R3,R5                                                            
         CR    R3,R4               END OF SCREEN?                               
         BL    P1H11               NO - KEEP GOING                              
*                                                                               
         BRAS  RE,MYHELP           DID CORRECT HELP PAGE                        
         BE    P1H14               YES - DONE                                   
         LA    R3,SRVX1H           START FROM TOP AGAIN                         
*                                                                               
P1H11    MVC   FHDA(L'P1HLCOM),P1HLCOM                                          
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         AR    R3,R5                                                            
*                                                                               
         LARL  R2,COMTAB                                                        
P1H12    BRAS  RE,DISONSCR                                                      
         BE    P1H13                                                            
         BRAS  RE,MYHELP           DID CORRECT HELP PAGE                        
         BE    P1H14               YES - DONE                                   
*                                                                               
         LA    R3,SRVX1H           REPEAT MESSAGE FOR THIS TABLE                
         MVC   FHDA(L'P1HLCOM),P1HLCOM                                          
         MVC   FHDA+L'P1HLCOM(L'CONTINUE),CONTINUE                              
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         BXH   R3,R5,P1H12         DISPLAY REMAINDER OF TABLE                   
*                                                                               
P1H13    IC    R5,FHLN             NEXT LINE ON SCREEN                          
         AR    R3,R5                                                            
         CR    R3,R4               END OF SCREEN?                               
         BL    P1H13P              NO - CONTINUE                                
*                                                                               
         BRAS  RE,MYHELP           DID CORRECT HELP PAGE                        
         BE    P1H14               YES - DONE                                   
         LA    R3,SRVX1H           START FROM TOP AGAIN                         
*                                                                               
P1H13P   MVC   FHDA(L'P1HLLNK),P1HLLNK                                          
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         AR    R3,R5                                                            
*                                                                               
         LARL  R2,LNKTAB                                                        
P1H13A   BRAS  RE,DISONSCR                                                      
         BE    P1H14               YES                                          
         BRAS  RE,MYHELP           DID CORRECT HELP PAGE                        
         BE    P1H14               YES - DONE                                   
*                                                                               
         LA    R3,SRVX1H           REPEAT MESSAGE FOR THIS TABLE                
         MVC   FHDA(L'P1HLLNK),P1HLLNK                                          
         MVC   FHDA+L'P1HLLNK(L'CONTINUE),CONTINUE                              
         OI    FHAT,FHATHI                                                      
         IC    R5,FHLN                                                          
         BXH   R3,R5,P1H13A        DISPLAY REMAINDER OF TABLE                   
*                                                                               
P1H14    CLI   PFKEY,2             'SELECT' CURSOR PRESSED                      
         BE    P1H16               YES                                          
*                                                                               
         LA    R3,SRVX1H           SET SELECT MESSAGE AND EXIT                  
         ST    R3,FADRH                                                         
         MVI   FERN,0                                                           
         MVI   HDRN,6              HELP DISPLAYED                               
         B     P1HX                                                             
*                                                                               
P1H16    LA    R3,SRVX1H           FIND FIELD FOR CURSOR                        
         CLC   SCURSOR,FHAD                                                     
         BL    FNH14                                                            
         LA    R3,SRVXFKH                                                       
         CLC   SCURSOR,FHAD                                                     
         BNL   FNH14                                                            
*                                                                               
         LA    R3,SRVX1H           SET FIRST ADDRESS                            
         ST    R3,FULL1                                                         
         LA    RF,SRVXFKH                                                       
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
FNH10    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    FNH12                                                            
         ST    R3,FULL1                                                         
         IC    RE,FHLN                                                          
         BXLE  R3,RE,FNH10                                                      
         DC    H'0'                                                             
*                                                                               
FNH12    L     R3,FULL1            CURSOR FIELD IS IN FULL1                     
         CLC   =C'>>',FHDA         INFO FIELD?                                  
         BE    FNH14               YES - NO SELECT                              
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         LH    RE,SCURSOR                                                       
         SR    RE,RF               GET DISP INTO FIELD                          
         SRDL  RE,32                                                            
         LH    R1,FULL                                                          
         DR    RE,R1                                                            
         MH    RF,FULL             GET 1ST CHARACTER OF THIS ENTRY              
         LA    RF,FHDA(RF)                                                      
*                                                                               
         AHI   R1,-3               REMEMBER THE LENGTH+2 ABOVE                  
         EX    R1,*+8                                                           
         BNH   FNH14                                                            
         CLC   0(0,RF),SPACES                                                   
         MVC   SRVP1,SPACES                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRVP1(0),0(RF)                                                   
         LA    R3,SRVP1H                                                        
         ST    R3,FADRH                                                         
         AHI   R1,1                                                             
         STC   R1,FHIL                                                          
*                                                                               
         LA    R0,SAVESCR          RESTORE BOTTOM HALF OF FF SCREEN             
         LHI   R1,SAVESCRL                                                      
         LA    RE,SRVU1H                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         B     EXITH               SHOW SELECT DONE                             
*                                                                               
FNH14    MVI   FERN,20             INVALID CURSOR POSITION                      
         B     P1HX                                                             
*                                                                               
P1HX     LA    R3,SRVP1H           SET ?(PAGE)                                  
         MVC   FHDA(3),=CL3'?  '                                                
         MVC   FHDA+1(1),BYTE                                                   
         OI    FHDA+1,X'F0'                                                     
         MVI   FHOL,3                                                           
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SIMPLE ROUTINE TO PUT OUT NAMES ONTO SCREEN                         *         
* NTRY: R2     = A(START OF NAMED BLOCK)                              *         
*       R3     = A(FIRST FIELD FOR DISPLAY)                           *         
***********************************************************************         
         SPACE 1                                                                
         USING NAMED,R2                                                         
         USING FHD,R3                                                           
DISONSCR LA    R0,SRVXFKH          MAKE SURE IT STILL FITS ON SCREEN            
         CR    R0,R3                                                            
         BH    DOS02                                                            
         CLI   *,255                                                            
         BR    RE                  SET CC FOR FULL SCREEN AND EXIT              
*                                                                               
DOS02    LH    R0,FULL+2           LOOP ROUND SETTING INFO ON LINES             
         LA    RF,FHDA                                                          
*                                                                               
DOS04    CLI   NNAME,EOT                                                        
         BER   RE                                                               
         MVC   0(L'NNAME,RF),NNAME                                              
         AHI   R2,NAMEL                                                         
         AH    RF,FULL                                                          
         BCT   R0,DOS04                                                         
*                                                                               
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         BXH   R3,RF,DISONSCR                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIGURE OUT IF WE DID OUR HELP PAGE YET                   *         
***********************************************************************         
         SPACE 1                                                                
MYHELP   NTR1  ,                                                                
         XR    R0,R0               BYTE HOLD PAGE NUMBER TO DISPLAY             
         ICM   R0,1,BYTE                                                        
         AHI   R0,-1                                                            
         BNP   EXITOK                                                           
*                                                                               
         LA    R3,SRVX1H           CLEAR OUT SCREEN                             
         USING FHD,R3                                                           
         XR    RF,RF                                                            
MYH02    LA    R0,SRVXFKH          CHECK FOR EOS                                
         CR    R0,R3                                                            
         BNH   EXITL                                                            
*                                                                               
         NI    FHAT,255-FHATHI     TURN OFF HIGLIGHTS                           
         IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RF,-(FHDAD)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),SPACES      CLEAR FIELD                                  
         IC    RF,FHLN                                                          
         BXH   R3,RF,MYH02                                                      
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS AND HANDY ROUTINES                                      *         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XMOD     MVC   TLANG,SAVELANG      RESTORE CONNECTED LANG                       
         L     RD,SAVERD                                                        
         XMOD1 ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND EQUATES                                                *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
EOT      EQU   FF                                                               
EMSGL    EQU   45                                                               
IMSGL    EQU   45                                                               
SRCHCLQ  EQU   128*1024                                                         
MAXLEN   EQU   256                 MAX DATA DISPLAY LENGTH                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
VLOADPNT DC    V(LOADPNT)                                                       
*                                                                               
ZEROS    DC    8C'0'                                                            
SPACES   DC    CL80' '                                                          
DUMMYPHS DC    X'FFFFFF'           D/A OF DUMMY C/R PHASES                      
*                                                                               
HDRHEX   DC    CL16'0.2.4.6.8.A.C.E.'                                           
         DC    CL16'0.2.4.6.8.A.C.E.'                                           
HDRDEC   DC    CL32'0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.'                           
         DC    CL32'0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.'                           
         EJECT                                                                  
***********************************************************************         
* MESSAGES FOR HELP LINES                                             *         
***********************************************************************         
         SPACE 1                                                                
P1HL1    DS    0CL78                                                            
         DC    CL50'>> You may enter a phase TSSPP00(L), or use keywor'         
         DC    CL28'ds "CORE" or "COREX"'                                       
P1HLSYS  DC    C'>> Addresses from SYSFACS '                                    
P1HLTCB  DC    C'>> Addresses from your TCB '                                   
P1HLCOM  DC    C'>> Addresses from COMFACS '                                    
P1HLLNK  DC    C'>> Addresses from LNKTAB '                                     
CONTINUE DC    C'- Continued'                                                   
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLE                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
DDTRTBL  DC    CL32'................................'  00-1F                    
         DC    CL32'................................'  20-3F                    
         DC    CL32' ..........<(+|&&.........!$*);.' 40-5F                    
         DC    CL32'-/........º,%_>?.........`.#@''="' 60-7F                    
         DC    CL32'.abcdefghi.......jklmnopqr......'  80-9F                    
         DC    CL32'.~stuvwxyz......................'  A0-BF                    
         DC    CL32'{ABCDEFGHI......}JKLMNOPQR......'  C0-DF                    
         DC    CL32'\.STUVWXYZ......0123456789......'  E0-FF                    
         EJECT                                                                  
***********************************************************************         
* TABLES - COVERED BY NAMED                                           *         
***********************************************************************         
         SPACE 1                                                                
SYSTAB   DS    0A                                                               
         DC    CL8'ADDAFT  ',AL1(6,0),AL2(VADDAFT-SYSFACD),AL4(0)               
         DC    CL8'ADRBUFF ',AL1(7,0),AL2(VADRBUFF-SYSFACD),AL4(0)              
         DC    CL8'ADRFILE ',AL1(7,0),AL2(VADRFILE-SYSFACD),AL4(0)              
         DC    CL8'ARREDIT ',AL1(7,0),AL2(VARREDIT-SYSFACD),AL4(0)              
         DC    CL8'CALLOV  ',AL1(6,0),AL2(VCALLOV-SYSFACD),AL4(0)               
         DC    CL8'CHANBLK ',AL1(7,0),AL2(VCHANBLK-SYSFACD),AL4(0)              
         DC    CL8'CHKOUT  ',AL1(6,0),AL2(VCHKOUT-SYSFACD),AL4(0)               
         DC    CL8'CHKPT1X ',AL1(7,0),AL2(VCHKPT1X-SYSFACD),AL4(0)              
         DC    CL8'CHKPT1  ',AL1(6,0),AL2(VCHKPT1-SYSFACD),AL4(0)               
         DC    CL8'CHKPT2X ',AL1(7,0),AL2(VCHKPT2X-SYSFACD),AL4(0)              
         DC    CL8'CHKPT2  ',AL1(6,0),AL2(VCHKPT2-SYSFACD),AL4(0)               
         DC    CL8'CLSESYS ',AL1(7,0),AL2(VCLSESYS-SYSFACD),AL4(0)              
         DC    CL8'CRAPBLK ',AL1(7,0),AL2(VCRAPBLK-SYSFACD),AL4(0)              
         DC    CL8'DABACK  ',AL1(6,0),AL2(VDABACK-SYSFACD),AL4(0)               
         DC    CL8'DACPUID ',AL1(7,0),AL2(VDACPUID-SYSFACD),AL4(0)              
         DC    CL8'DADDS   ',AL1(5,0),AL2(VDADDS-SYSFACD),AL4(0)                
         DC    CL8'DARPT   ',AL1(5,0),AL2(VDARPT-SYSFACD),AL4(0)                
         DC    CL8'DATRNS  ',AL1(6,0),AL2(VDATRNS-SYSFACD),AL4(0)               
         DC    CL8'DECBLST ',AL1(7,0),AL2(VDECBLST-SYSFACD),AL4(0)              
         DC    CL8'DMGR    ',AL1(4,0),AL2(VDATAMGR-SYSFACD),AL4(0)              
         DC    CL8'DMGRFLS ',AL1(7,0),AL2(VDMGRFLS-SYSFACD),AL4(0)              
         DC    CL8'DMOD000 ',AL1(7,0),AL2(VDMOD000-SYSFACD),AL4(0)              
         DC    CL8'DMPFILE ',AL1(7,0),AL2(VDMPFILE-SYSFACD),AL4(0)              
         DC    CL8'DMRCVR  ',AL1(6,0),AL2(VDMRCVR-SYSFACD),AL4(0)               
         DC    CL8'DTFIOA  ',AL1(6,0),AL2(VDTFIOA-SYSFACD),AL4(0)               
         DC    CL8'EKEY    ',AL1(4,0),AL2(VEKEY-SYSFACD),AL4(0)                 
         DC    CL8'ENQDEQ  ',AL1(6,0),AL2(VENQDEQ-SYSFACD),AL4(0)               
         DC    CL8'FINDSYS ',AL1(7,0),AL2(VFINDSYS-SYSFACD),AL4(0)              
         DC    CL8'FNDEOF  ',AL1(6,0),AL2(VFNDEOF-SYSFACD),AL4(0)               
         DC    CL8'ISCPUID ',AL1(7,0),AL2(VISCPUID-SYSFACD),AL4(0)              
         DC    CL8'LCBUFFS ',AL1(7,0),AL2(VLCBUFFS-SYSFACD),AL4(0)              
         DC    CL8'LCBUFFX ',AL1(7,0),AL2(VLCBUFFX-SYSFACD),AL4(0)              
         DC    CL8'LCM     ',AL1(3,0),AL2(VLCM-SYSFACD),AL4(0)                  
         DC    CL8'LCWRITE ',AL1(7,0),AL2(VLCWRITE-SYSFACD),AL4(0)              
         DC    CL8'LNKTAB  ',AL1(6,0),AL2(VLNKTAB-SYSFACD),AL4(0)               
         DC    CL8'LOCKER  ',AL1(6,0),AL2(VLOCKER-SYSFACD),AL4(0)               
         DC    CL8'LOCKSPC ',AL1(7,0),AL2(VLOCKSPC-SYSFACD),AL4(0)              
         DC    CL8'LOCKTAB ',AL1(7,0),AL2(VLOCKTAB-SYSFACD),AL4(0)              
         DC    CL8'LOGGER  ',AL1(6,0),AL2(VLOGGER-SYSFACD),AL4(0)               
         DC    CL8'MQIPARM ',AL1(7,0),AL2(VMQIPARM-SYSFACD),AL4(0)              
         DC    CL8'OPENSYS ',AL1(7,0),AL2(VOPENSYS-SYSFACD),AL4(0)              
         DC    CL8'OPENIS  ',AL1(6,0),AL2(VOPENIS-SYSFACD),AL4(0)               
         DC    CL8'OPEN    ',AL1(4,0),AL2(VOPEN-SYSFACD),AL4(0)                 
         DC    CL8'PHLIST  ',AL1(6,0),AL2(VPHLIST-SYSFACD),AL4(0)               
         DC    CL8'PRGMS   ',AL1(5,0),AL2(VPRGMS-SYSFACD),AL4(0)                
         DC    CL8'PRQENTS ',AL1(7,0),AL2(VPRQENTS-SYSFACD),AL4(0)              
         DC    CL8'PRQ     ',AL1(3,0),AL2(VPRQ-SYSFACD),AL4(0)                  
         DC    CL8'PRTQUE  ',AL1(6,0),AL2(VPRTQUE-SYSFACD),AL4(0)               
         DC    CL8'POWWOW  ',AL1(6,0),AL2(VPOWWOW-SYSFACD),AL4(0)               
         DC    CL8'RCTYPE  ',AL1(6,0),AL2(VRCTYPE-SYSFACD),AL4(0)               
         DC    CL8'RDID    ',AL1(4,0),AL2(VRDID-SYSFACD),AL4(0)                 
         DC    CL8'READ    ',AL1(4,0),AL2(VREAD-SYSFACD),AL4(0)                 
         DC    CL8'RKEY    ',AL1(4,0),AL2(VRKEY-SYSFACD),AL4(0)                 
         DC    CL8'RUNIT   ',AL1(5,0),AL2(VRUNIT-SYSFACD),AL4(0)                
         DC    CL8'SKIPA   ',AL1(5,0),AL2(VSKIPA-SYSFACD),AL4(0)                
         DC    CL8'SSB     ',AL1(3,0),AL2(VSSB-SYSFACD),AL4(0)                  
         DC    CL8'SELIST  ',AL1(6,0),AL2(VSELIST-SYSFACD),AL4(0)               
         DC    CL8'SHIPIT  ',AL1(6,0),AL2(VSHIPIT-SYSFACD),AL4(0)               
         DC    CL8'SYSFAC0 ',AL1(7,0),AL2(VSYSFAC0-SYSFACD),AL4(0)              
         DC    CL8'SYSFAC1 ',AL1(7,0),AL2(VSYSFAC1-SYSFACD),AL4(0)              
         DC    CL8'SYSFAC2 ',AL1(7,0),AL2(VSYSFAC2-SYSFACD),AL4(0)              
         DC    CL8'SYSFAC3 ',AL1(7,0),AL2(VSYSFAC3-SYSFACD),AL4(0)              
         DC    CL8'SYSFAC4 ',AL1(7,0),AL2(VSYSFAC4-SYSFACD),AL4(0)              
         DC    CL8'TCB     ',AL1(3,0),AL2(VTCB-SYSFACD),AL4(0)                  
         DC    CL8'TEMPEST ',AL1(7,0),AL2(VTEMPEST-SYSFACD),AL4(0)              
         DC    CL8'TEMPSTR ',AL1(7,0),AL2(VTEMPSTR-SYSFACD),AL4(0)              
         DC    CL8'TEMPTRC ',AL1(7,0),AL2(VTEMPTRC-SYSFACD),AL4(0)              
         DC    CL8'TERMBLD ',AL1(7,0),AL2(VTERMBLD-SYSFACD),AL4(0)              
         DC    CL8'TICTOCT ',AL1(7,0),AL2(VTICTOCT-SYSFACD),AL4(0)              
         DC    CL8'TICTOC  ',AL1(6,0),AL2(VTICTOC-SYSFACD),AL4(0)               
         DC    CL8'TSTTAB  ',AL1(6,0),AL2(VTSTTAB-SYSFACD),AL4(0)               
         DC    CL8'TSTRCVR ',AL1(7,0),AL2(VTSTRCVR-SYSFACD),AL4(0)              
         DC    CL8'TWASVR  ',AL1(6,0),AL2(VTWASVR-SYSFACD),AL4(0)               
         DC    CL8'UPDTAB  ',AL1(6,0),AL2(VUPDTAB-SYSFACD),AL4(0)               
         DC    CL8'VRSNTAB ',AL1(7,0),AL2(VVRSNTAB-SYSFACD),AL4(0)              
         DC    CL8'VUTL    ',AL1(3,0),AL2(VUTL-SYSFACD),AL4(0)                  
         DC    CL8'WCTYPE  ',AL1(6,0),AL2(VWCTYPE-SYSFACD),AL4(0)               
         DC    CL8'WKEY    ',AL1(4,0),AL2(VWKEY-SYSFACD),AL4(0)                 
         DC    CL8'WKFILE  ',AL1(6,0),AL2(VWKFILE-SYSFACD),AL4(0)               
         DC    CL8'WRITE   ',AL1(5,0),AL2(VWRITE-SYSFACD),AL4(0)                
         DC    CL8'WRTAFT  ',AL1(6,0),AL2(VWRTAFT-SYSFACD),AL4(0)               
         DC    CL8'WSSVR   ',AL1(5,0),AL2(VWSSVR-SYSFACD),AL4(0)                
         DC    CL8'WTID    ',AL1(4,0),AL2(VWTID-SYSFACD),AL4(0)                 
         DC    CL8'WTCKD   ',AL1(5,0),AL2(VWTCKD-SYSFACD),AL4(0)                
         DC    CL8'ZIPFAC  ',AL1(6,0),AL2(VZIPFAC-SYSFACD),AL4(0)               
         DC    AL1(EOT)                                                         
*                                                                               
TCBTAB   DS    0A                                                               
         DC    CL8'MAP     ',AL1(3,0),AL2(TCBMAP-TCBD),AL4(0)                   
         DC    CL8'TWA     ',AL1(3,0),AL2(TCBTWA-TCBD),AL4(0)                   
         DC    CL8'UTL     ',AL1(3,0),AL2(TCBUTL-TCBD),AL4(0)                   
         DC    AL1(EOT)                                                         
*                                                                               
COMTAB   DS    0H                                                               
         DC    CL8'DATAMGR ',AL1(7,0),AL2(CDATAMGR-COMFACSD),AL4(0)             
         DC    CL8'CALLOV  ',AL1(6,0),AL2(CCALLOV-COMFACSD),AL4(0)              
         DC    CL8'GETMSG  ',AL1(6,0),AL2(CGETMSG-COMFACSD),AL4(0)              
         DC    CL8'GETTXT  ',AL1(6,0),AL2(CGETTXT-COMFACSD),AL4(0)              
         DC    CL8'SWITCH  ',AL1(6,0),AL2(CSWITCH-COMFACSD),AL4(0)              
         DC    CL8'HELLO   ',AL1(5,0),AL2(CHELLO-COMFACSD),AL4(0)               
         DC    CL8'SCANNER ',AL1(7,0),AL2(CSCANNER-COMFACSD),AL4(0)             
         DC    CL8'UNSCAN  ',AL1(6,0),AL2(CUNSCAN-COMFACSD),AL4(0)              
         DC    CL8'HEXIN   ',AL1(5,0),AL2(CHEXIN-COMFACSD),AL4(0)               
         DC    CL8'HEXOUT  ',AL1(6,0),AL2(CHEXOUT-COMFACSD),AL4(0)              
         DC    CL8'CASHVAL ',AL1(7,0),AL2(CCASHVAL-COMFACSD),AL4(0)             
         DC    CL8'DATVAL  ',AL1(6,0),AL2(CDATVAL-COMFACSD),AL4(0)              
         DC    CL8'DATCON  ',AL1(6,0),AL2(CDATCON-COMFACSD),AL4(0)              
         DC    CL8'TERMVAL ',AL1(7,0),AL2(CTERMVAL-COMFACSD),AL4(0)             
         DC    CL8'SCUNKEY ',AL1(7,0),AL2(CSCUNKEY-COMFACSD),AL4(0)             
         DC    CL8'ADDAY   ',AL1(5,0),AL2(CADDAY-COMFACSD),AL4(0)               
         DC    CL8'GETDAY  ',AL1(6,0),AL2(CGETDAY-COMFACSD),AL4(0)              
         DC    CL8'GETPROF ',AL1(7,0),AL2(CGETPROF-COMFACSD),AL4(0)             
         DC    CL8'PERVERT ',AL1(7,0),AL2(CPERVERT-COMFACSD),AL4(0)             
         DC    CL8'GETFACT ',AL1(7,0),AL2(CGETFACT-COMFACSD),AL4(0)             
         DC    CL8'XSORT   ',AL1(5,0),AL2(CXSORT-COMFACSD),AL4(0)               
         DC    CL8'REQTWA  ',AL1(6,0),AL2(CREQTWA-COMFACSD),AL4(0)              
*&&US                                                                           
         DC    CL8'SOFDAT  ',AL1(6,0),AL2(CSOFDAT-COMFACSD),AL4(0)              
         DC    CL8'DEMADDR ',AL1(7,0),AL2(CDEMADDR-COMFACSD),AL4(0)             
         DC    CL8'DEMDISP ',AL1(6,0),AL2(CT00AD0-COMFACSD),AL4(0)              
         DC    CL8'DEMTABS ',AL1(6,0),AL2(CDEMTABS-COMFACSD),AL4(0)             
         DC    CL8'SPGETIUN',AL1(6,0),AL2(CSPGTIUN-COMFACSD),AL4(0)             
         DC    CL8'DEFINE  ',AL1(6,0),AL2(CDEFINE-COMFACSD),AL4(0)              
         DC    CL8'GENNEW  ',AL1(6,0),AL2(CGENNEW-COMFACSD),AL4(0)              
         DC    CL8'DEMAND1 ',AL1(7,0),AL2(CDEMAND1-COMFACSD),AL4(0)             
         DC    CL8'RUNIT   ',AL1(5,0),AL2(CRUNIT-COMFACSD),AL4(0)               
         DC    CL8'DDLINK  ',AL1(6,0),AL2(CDDLINK-COMFACSD),AL4(0)              
         DC    CL8'DEMOUT  ',AL1(6,0),AL2(CDEMOUT-COMFACSD),AL4(0)              
         DC    CL8'DEMEL   ',AL1(5,0),AL2(CDEMEL-COMFACSD),AL4(0)               
         DC    CL8'DEMAINT ',AL1(7,0),AL2(CDEMAINT-COMFACSD),AL4(0)             
         DC    CL8'DEMAND  ',AL1(6,0),AL2(CDEMAND-COMFACSD),AL4(0)              
         DC    CL8'DEMOMTH ',AL1(7,0),AL2(CDEMOMTH-COMFACSD),AL4(0)             
         DC    CL8'DEMOVAL ',AL1(7,0),AL2(CDEMOVAL-COMFACSD),AL4(0)             
         DC    CL8'DEMOCON ',AL1(7,0),AL2(CDEMOCON-COMFACSD),AL4(0)             
         DC    CL8'GENERAL ',AL1(7,0),AL2(CGENERAL-COMFACSD),AL4(0)             
         DC    CL8'PERVAL  ',AL1(6,0),AL2(CPERVAL-COMFACSD),AL4(0)              
         DC    CL8'DLFLD   ',AL1(5,0),AL2(CDLFLD-COMFACSD),AL4(0)               
         DC    CL8'SEARCH  ',AL1(6,0),AL2(CSEARCH-COMFACSD),AL4(0)              
         DC    CL8'LIMACC  ',AL1(6,0),AL2(CLIMACC-COMFACSD),AL4(0)              
         DC    CL8'SRCHCAL ',AL1(7,0),AL2(CSRCHCAL-COMFACSD),AL4(0)             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'PERVAL  ',AL1(6,0),AL2(CPERVAL-COMFACSD),AL4(0)              
         DC    CL8'GETTXT  ',AL1(6,0),AL2(CGETTXT-COMFACSD),AL4(0)              
         DC    CL8'DLFLD   ',AL1(5,0),AL2(CDLFLD-COMFACSD),AL4(0)               
         DC    CL8'GENERAL ',AL1(7,0),AL2(CGENERAL-COMFACSD),AL4(0)             
         DC    CL8'SEARCH  ',AL1(6,0),AL2(CSEARCH-COMFACSD),AL4(0)              
         DC    CL8'CONVERT ',AL1(7,0),AL2(CCONVERT-COMFACSD),AL4(0)             
         DC    CL8'PRORATA ',AL1(7,0),AL2(CPRORATA-COMFACSD),AL4(0)             
         DC    CL8'LIMACC  ',AL1(6,0),AL2(CLIMACC-COMFACSD),AL4(0)              
         DC    CL8'SRCHCAL ',AL1(7,0),AL2(CSRCHCAL-COMFACSD),AL4(0)             
         DC    CL8'MEDFACS ',AL1(7,0),AL2(CMEDFACS-COMFACSD),AL4(0)             
         DC    CL8'CASHVAL ',AL1(7,0),AL2(CCASHVA-COMFACSD),AL4(0)              
         DC    CL8'TOBACCO ',AL1(7,0),AL2(CTOBACCO-COMFACSD),AL4(0)             
         DC    CL8'SOFDAT  ',AL1(6,0),AL2(CSOFDAT-COMFACSD),AL4(0)              
         DC    CL8'DDLINK  ',AL1(6,0),AL2(CDDLINK-COMFACSD),AL4(0)              
         DC    CL8'RUNIT   ',AL1(5,0),AL2(CRUNIT-COMFACSD),AL4(0)               
*&&                                                                             
         DC    CL8'GLOBBER ',AL1(7,0),AL2(CGLOBBER-COMFACSD),AL4(0)             
         DC    CL8'MINIO   ',AL1(5,0),AL2(CMINIO-COMFACSD),AL4(0)               
         DC    CL8'PARSNIP ',AL1(7,0),AL2(CPARSNIP-COMFACSD),AL4(0)             
         DC    CL8'DICTATE ',AL1(7,0),AL2(CDICTATE-COMFACSD),AL4(0)             
         DC    CL8'EDITOR  ',AL1(6,0),AL2(CEDITOR-COMFACSD),AL4(0)              
         DC    CL8'GETHELP ',AL1(7,0),AL2(CGETHELP-COMFACSD),AL4(0)             
         DC    CL8'CUREDIT ',AL1(7,0),AL2(CCUREDIT-COMFACSD),AL4(0)             
         DC    CL8'GETRET  ',AL1(6,0),AL2(CGETRET-COMFACSD),AL4(0)              
         DC    CL8'REPORT  ',AL1(6,0),AL2(CREPORT-COMFACSD),AL4(0)              
         DC    CL8'BLDCUR  ',AL1(6,0),AL2(CBLDCUR-COMFACSD),AL4(0)              
         DC    CL8'GETCUR  ',AL1(6,0),AL2(CGETCUR-COMFACSD),AL4(0)              
         DC    CL8'GETNAR  ',AL1(6,0),AL2(CGETNAR-COMFACSD),AL4(0)              
         DC    CL8'DEJAVU  ',AL1(6,0),AL2(CDEJAVU-COMFACSD),AL4(0)              
         DC    CL8'SECRET  ',AL1(6,0),AL2(CSECRET-COMFACSD),AL4(0)              
         DC    CL8'BILLIT  ',AL1(6,0),AL2(CBILLIT-COMFACSD),AL4(0)              
         DC    CL8'LOCKET  ',AL1(6,0),AL2(CLOCKET-COMFACSD),AL4(0)              
         DC    CL8'PQPROF  ',AL1(6,0),AL2(CPQPROF-COMFACSD),AL4(0)              
         DC    CL8'SCRIPT  ',AL1(6,0),AL2(CSCRIPT-COMFACSD),AL4(0)              
         DC    CL8'DATTIM  ',AL1(6,0),AL2(CDATTIM-COMFACSD),AL4(0)              
         DC    CL8'BINSRCH ',AL1(7,0),AL2(CBINSRCH-COMFACSD),AL4(0)             
         DC    CL8'PROTON  ',AL1(6,0),AL2(CPROTON-COMFACSD),AL4(0)              
         DC    CL8'PROTOFF ',AL1(7,0),AL2(CPROTOFF-COMFACSD),AL4(0)             
         DC    CL8'HELEN   ',AL1(5,0),AL2(CHELEN-COMFACSD),AL4(0)               
         DC    CL8'MQIO    ',AL1(4,0),AL2(CMQIO-COMFACSD),AL4(0)                
         DC    CL8'EUREKA  ',AL1(6,0),AL2(CEUREKA-COMFACSD),AL4(0)              
         DC    CL8'LOCKUP  ',AL1(6,0),AL2(CLOCKUP-COMFACSD),AL4(0)              
         DC    CL8'LOCKSPC ',AL1(7,0),AL2(CLOCKSPC-COMFACSD),AL4(0)             
         DC    CL8'WSSVR   ',AL1(5,0),AL2(CWSSVR-COMFACSD),AL4(0)               
         DC    AL1(EOT)                                                         
*                                                                               
LNKTAB   DS    0H                                                               
         DC    CL8'CARDS   ',AL1(5,0),AL2(LCARDS-LNKTABD),AL4(0)                
         DC    CL8'DS00900 ',AL1(7,0),AL2(LDS00900-LNKTABD),AL4(0)              
         DC    CL8'DS00901 ',AL1(7,0),AL2(LDS00901-LNKTABD),AL4(0)              
         DC    CL8'ACCEMU  ',AL1(6,0),AL2(LACCEMU-LNKTABD),AL4(0)               
         DC    CL8'DALINK  ',AL1(6,0),AL2(LDALINK-LNKTABD),AL4(0)               
         DC    CL8'DANDX   ',AL1(5,0),AL2(LDANDX-LNKTABD),AL4(0)                
         DC    CL8'DAPTRS  ',AL1(6,0),AL2(LDAPTRS-LNKTABD),AL4(0)               
         DC    CL8'DTFIOA  ',AL1(6,0),AL2(LDTFIOA-LNKTABD),AL4(0)               
         DC    CL8'DTFS    ',AL1(4,0),AL2(LDTFS-LNKTABD),AL4(0)                 
         DC    CL8'DYNDD   ',AL1(5,0),AL2(LDYNDD-LNKTABD),AL4(0)                
         DC    CL8'ENQCTL  ',AL1(6,0),AL2(LENQCTL-LNKTABD),AL4(0)               
         DC    CL8'ISDDS   ',AL1(5,0),AL2(LISDDS-LNKTABD),AL4(0)                
         DC    CL8'LOCKER  ',AL1(6,0),AL2(LLOCKER-LNKTABD),AL4(0)               
         DC    CL8'DMPRTQ  ',AL1(6,0),AL2(LDMPRTQ-LNKTABD),AL4(0)               
         DC    CL8'DMPRTQO ',AL1(7,0),AL2(LDMPRTQO-LNKTABD),AL4(0)              
         DC    CL8'DMRCVR  ',AL1(6,0),AL2(LDMRCVR-LNKTABD),AL4(0)               
         DC    CL8'DMWRKF  ',AL1(6,0),AL2(LDMWRKF-LNKTABD),AL4(0)               
         DC    CL8'DMWRKR  ',AL1(6,0),AL2(LDMWRKR-LNKTABD),AL4(0)               
         DC    CL8'DTCNV   ',AL1(5,0),AL2(LDTCNV-LNKTABD),AL4(0)                
         DC    CL8'ABEND   ',AL1(5,0),AL2(LABEND-LNKTABD),AL4(0)                
         DC    CL8'EMUMSG  ',AL1(6,0),AL2(LEMUMSG-LNKTABD),AL4(0)               
         DC    CL8'FALOAD  ',AL1(6,0),AL2(LFALOAD-LNKTABD),AL4(0)               
         DC    CL8'FAMNTR  ',AL1(6,0),AL2(LFAMNTR-LNKTABD),AL4(0)               
         DC    CL8'MSGQIN  ',AL1(6,0),AL2(LMSGQIN-LNKTABD),AL4(0)               
         DC    CL8'SCRNCH  ',AL1(6,0),AL2(LSCRNCH-LNKTABD),AL4(0)               
         DC    CL8'SECRET  ',AL1(6,0),AL2(LSECRET-LNKTABD),AL4(0)               
         DC    CL8'FATAB   ',AL1(5,0),AL2(LFATAB-LNKTABD),AL4(0)                
         DC    CL8'TASKER  ',AL1(6,0),AL2(LTASKER-LNKTABD),AL4(0)               
         DC    CL8'TIADDS  ',AL1(6,0),AL2(LTIADDS-LNKTABD),AL4(0)               
         DC    CL8'FATISTR ',AL1(7,0),AL2(LFATISTR-LNKTABD),AL4(0)              
         DC    CL8'TI3270  ',AL1(6,0),AL2(LTI3270-LNKTABD),AL4(0)               
         DC    CL8'TOADDS  ',AL1(6,0),AL2(LTOADDS-LNKTABD),AL4(0)               
         DC    CL8'TOSCRP  ',AL1(6,0),AL2(LTOSCRP-LNKTABD),AL4(0)               
         DC    CL8'TOSTR   ',AL1(5,0),AL2(LTOSTR-LNKTABD),AL4(0)                
         DC    CL8'TO3270  ',AL1(6,0),AL2(LTO3270-LNKTABD),AL4(0)               
         DC    CL8'TWASVR  ',AL1(6,0),AL2(LTWASVR-LNKTABD),AL4(0)               
         DC    CL8'HELEN   ',AL1(5,0),AL2(LHELEN-LNKTABD),AL4(0)                
         DC    CL8'LOADER  ',AL1(6,0),AL2(LLOADER-LNKTABD),AL4(0)               
         DC    CL8'RECUP   ',AL1(5,0),AL2(LRECUP-LNKTABD),AL4(0)                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OK MESSAGES                                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
OKMSGS   DS    0CL(IMSGL)                                                       
OK01     DC    CL(IMSGL)'Information displayed - as requested'                  
OK02     DC    CL(IMSGL)'Data displayed - enter patch'                          
OK03     DC    CL(IMSGL)'Search successful - data string displayed'             
OK04     DC    CL(IMSGL)'Search not successful - String not found'              
OK05     DC    CL(IMSGL)'Search not successful - Hit enter to continue'         
OK06     DC    CL(IMSGL)'Help displayed. Select with PFK or type name'          
OK07     DC    CL(IMSGL)'Data patched - enter next request'                     
OK08     DC    CL(IMSGL)'Data displayed is within Facpak region'                
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'Missing input field'                                   
ERR02    DC    CL(EMSGL)'Must be Keyword or TSPPOO(L) Phase or <CORE>'          
ERR03    DC    CL(EMSGL)'Too much input in field'                               
ERR04    DC    CL(EMSGL)'Not enough input in field'                             
ERR05    DC    CL(EMSGL)'Level must be <live>, A, B or C'                       
ERR06    DC    CL(EMSGL)'Language code is not known - see FALANG'               
ERR07    DC    CL(EMSGL)'The next 5 bytes must be valid HEX'                    
ERR08    DC    CL(EMSGL)'Phase name is not in phase list'                       
ERR09    DC    CL(EMSGL)'Option is not valid when P1 is <CORE>'                 
ERR10    DC    CL(EMSGL)'Input is not valid numeric (Range 0 - 9)'              
ERR11    DC    CL(EMSGL)'''LEVEL='' search string not found'                    
ERR12    DC    CL(EMSGL)'Input is not valid hex (Range 0-9,A-F)'                
ERR13    DC    CL(EMSGL)'Offset is outside of permitted range'                  
ERR14    DC    CL(EMSGL)'You have input an invalid search string'               
ERR15    DC    CL(EMSGL)'Your search string has no '' at the end'               
ERR16    DC    CL(EMSGL)'You need an even number of valid hex chars'            
ERR17    DC    CL(EMSGL)'.............tring has no '' at the end'               
ERR18    DC    CL(EMSGL)'''*LNKSTAMP*'' search string not found'                
ERR19    DC    CL(EMSGL)'''BOOK='' search string not found'                     
ERR20    DC    CL(EMSGL)'Invalid position for cursor selection'                 
ERR21    DC    CL(EMSGL)'Opening chevron is unpaired'                           
ERR22    DC    CL(EMSGL)'Closing chevron is unpaired'                           
ERR23    DC    CL(EMSGL)'Closing chevron is before the opening one'             
ERR24    DC    CL(EMSGL)'Chevron encloses data not at a byte boundary'          
ERR25    DC    CL(EMSGL)'Chevrons are too close together'                       
ERR26    DC    CL(EMSGL)'Production patch requires id'                          
ERR27    DC    CL(EMSGL)'This field is in an invalid format'                    
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
RELO     DS    A                                                                
SAVERD   DS    A                                                                
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
APHASE   DS    A                   A(PHASE)                                     
ACORE    DS    A                   A(CORE)                                      
*                                                                               
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
FULL     DS    F                                                                
FULL1    DS    F                                                                
*                                                                               
AHEXIN   DS    A                                                                
AHEXOUT  DS    A                                                                
ASCANNER DS    A                                                                
ATICTOC  DS    A                                                                
ACALLOV  DS    A                                                                
ALOGGER  DS    A                                                                
ADMOD000 DS    A                                                                
AWCTYPE  DS    A                                                                
ASSB     DS    A                                                                
ATCB     DS    A                                                                
ALNKTAB  DS    A                                                                
MYTCB    DS    A                                                                
*                                                                               
PLIST    DS    6F                                                               
DMCB     DS    6F                                                               
ERROR    DS    F                                                                
START    DS    A                                                                
*                                                                               
APERSON  DS    A                                                                
ALANGTAB DS    A                                                                
OFFSET   DS    F                                                                
FNDADR   DS    F                                                                
DDOSET   DS    F                                                                
PHSLEN   DS    F                                                                
SRCHLEN  DS    F                                                                
*                                                                               
ST24     DS    A                                                                
ST31     DS    A                                                                
EN24     DS    A                                                                
EN31     DS    A                                                                
*                                                                               
HALF     DS    H                                                                
LENGTH   DS    H                                                                
DDREM    DS    H                                                                
DDALEN   DS    H                                                                
SAVETEST DS    X                                                                
SAVETST1 DS    X                                                                
PHLEVEL  DS    C                                                                
PATCHIND DS    C                                                                
PFKEY    DS    C                                                                
PFKIND   DS    C                                                                
SRCHIND  DS    C                                                                
ERRIND   DS    C                                                                
BYTE     DS    X                                                                
WPATCH   DS    X                                                                
WSRCH    DS    X                                                                
INHELP   DS    X                                                                
SAVELANG DS    X                                                                
DDSLANG  DS    C                                                                
DDSLVL   DS    C                                                                
IBMLANG  DS    C                                                                
SYSCH    DS    C                                                                
STRLEN   DS    C                                                                
SRCHSTR  DS    CL80                                                             
SCURSOR  DS    H                                                                
SYSID    DS    X                                                                
INXA     DS    X                                                                
INCORE   DS    X                                                                
INLVL    DS    X                                                                
PRODSYS  DS    X                                                                
PROFSID  DS    CL8                                                              
PASSWORD DS    CL8                                                              
SYSNAME  DS    CL4                                                              
SYSSHRT  DS    CL3                                                              
SAVETIME DS    PL4                                                              
MSG      DS    CL60                                                             
WORK     DS    XL64                                                             
*                                                                               
PSREC    DS    XL(PROGSPCL)                                                     
SPSREC   DS    XL(PROGSPCL)                                                     
*                                                                               
OPERMSG  DS    0CL53'+PATCH+  P1...... P2...... P3...... P4...... (F'           
OPERID   DS    CL8                                                              
         DS    CL1                                                              
OPERP1   DS    CL8                                                              
         DS    CL1                                                              
OPERP2   DS    CL8                                                              
         DS    CL1                                                              
OPERP3   DS    CL8                                                              
         DS    CL1                                                              
OPERP4   DS    CL8                                                              
         DS    CL1                                                              
OPERFAC  DS    CL8                                                              
OPERMSGL EQU   *-OPERMSG                                                        
*                                                                               
DDDSP    DS    X                                                                
DDHEX    DS    X                                                                
DDALF    DS    X                                                                
*                                                                               
LOGREC   DS    0CL38                                                            
LOGID    DS    CL4                 $PA.                                         
LOGLUID  DS    CL8                 LUID                                         
LOGTIME  DS    PL4                 0HHMMSS+                                     
LOGPHS   DS    CL8                 TSPPNNL-PHASE                                
LOGOFFS  DS    CL8                 OFFSET                                       
LOGUSER  DS    CL8                 USER                                         
*                                                                               
DATAC    DS    XL(MAXLEN*2)                                                     
DATACL   EQU   *-DATAC                                                          
DATA     DS    XL(MAXLEN+4)                                                     
DATAL    EQU   *-DATA                                                           
*                                                                               
SAVESCR  DS    XL(SRVWORK-SRVU1H)                                               
SAVESCRL EQU   *-SAVESCR                                                        
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER NAMED AREAS OF W/S                                   *         
***********************************************************************         
         SPACE 1                                                                
NAMED    DSECT                                                                  
NNAME    DS    CL8                                                              
NLEN     DS    AL1                                                              
         DS    AL1                                                              
NDISP    DS    AL2                                                              
         DS    AL4                                                              
NAMEL    EQU   *-NAMED                                                          
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECTS                                                       *         
***********************************************************************         
         SPACE 1                                                                
SRDISFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRDISFFD                                                       
         ORG   SRVH1OH                                                          
       ++INCLUDE SRDISFED                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDPERSOND                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERSOND                                                      
         PRINT ON                                                               
* FAPROGSPCD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAPROGSPCD                                                     
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FALNKTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FALNKTAB                                                       
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* PHASES LOADED HERE                                                  *         
***********************************************************************         
         SPACE 1                                                                
         ORG                                                                    
LOADPNT  CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRDIS00   09/19/11'                                      
         END                                                                    
