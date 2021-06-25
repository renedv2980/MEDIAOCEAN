*          DATA SET NELNK10    AT LEVEL 029 AS OF 12/07/20                      
*PROCESS USING(WARN(15))           ENABLE ALL USING STATEMENT WARNINGS          
*PHASE T30210B                                                                  
NELNK10  TITLE '- INITIAL DOWNLOAD FOR CONSTRUCTOR AND DEMO ENGINE -'           
         PRINT GEN                                                              
SVRDEF   CSECT                                                                  
         LKSVR REQUEST=*,CODE=CODE,SYSTEM=NETSYSQ,TYPE=D,              *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#NETTB,NEBRDNMD,   *        
               B#SYNTB,NESYNNMD,B#CBLTB,NECBNAMD,B#NTIQ,NTIQD,         *        
               B#NTIPTY,NTIPTYD,B#NADPTY,NADPTYD,B#SPTY,SPTYD,         *        
               B#DAYP,DAYPD,B#CONT,CONTD,B#AIRN,AIRND,B#NADCAT,        *        
               NADCATD,B#DEM,NTIDTD,B#STA,STARECD),                    *        
               AUTOCLEAR=Y                                                      
                                                                                
***********************************************************************         
* NETWORK CONSTRUCTOR INITIAL DOWNLOADS                               *         
***********************************************************************         
         DS    0F                                                               
CODE     NMOD1 0,**NL10*,CLEAR=YES,RR=RE                                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   RUNREQ                                                           
                                                                                
         LA    R0,SAVECLR          CLEAR DOWN SAVE AREA                         
         LHI   R1,SAVECLRL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         BASR  RE,0                MOVE LITERALS TO SAVED STORAGE               
         AHI   RE,LVALUES-*                                                     
         LA    R0,WVALUES                                                       
         LHI   R1,WVALUEL                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         L     RF,RELO                                                          
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         AR    R2,RF                                                            
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
                                                                                
B#NETTB  EQU   3                   BROADCAST NETWORKS TABLE                     
         ICM   RF,15,ACOMFACS      GET ADDRESS FROM DEMTABS                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,NEBROD                                                 
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  RE,15,LP_BLKS+((B#NETTB-1)*L'LP_BLKS)                            
                                                                                
B#SYNTB  EQU   4                   SYNDICATION NETWORKS TABLE                   
         ICM   RF,15,ACOMFACS      GET ADDRESS FROM DEMTABS                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,NESYND                                                 
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  RE,15,LP_BLKS+((B#SYNTB-1)*L'LP_BLKS)                            
                                                                                
B#CBLTB  EQU   5                   CABLE NETWORKS TABLE                         
         ICM   RF,15,ACOMFACS      GET ADDRESS FROM DEMTABS                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,NECABNAM                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  RE,15,LP_BLKS+((B#CBLTB-1)*L'LP_BLKS)                            
                                                                                
B#NTIQ   EQU   6                   NTI QUARTERS TABLE                           
         ICM   RF,15,ACOMFACS      GET ADDRESS FROM DEMTABS                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,NENTIQRT                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  RE,15,LP_BLKS+((B#NTIQ-1)*L'LP_BLKS)                             
                                                                                
B#NTIPTY EQU   7                   NTI PROGRAM TYPES TABLE                      
         BASR  RE,0                                                             
         AHI   RE,NTIPTYP-*                                                     
         STCM  RE,15,LP_BLKS+((B#NTIPTY-1)*L'LP_BLKS)                           
                                                                                
B#NADPTY EQU   8                   NAD PROGRAM TYPES TABLE                      
         BASR  RE,0                                                             
         AHI   RE,NADPTYP-*                                                     
         STCM  RE,15,LP_BLKS+((B#NADPTY-1)*L'LP_BLKS)                           
                                                                                
B#SPTY   EQU   9                   SUB-PROGRAM TYPES TABLE                      
         BASR  RE,0                                                             
         AHI   RE,SUBPTYP-*                                                     
         STCM  RE,15,LP_BLKS+((B#SPTY-1)*L'LP_BLKS)                             
                                                                                
B#DAYP   EQU   10                  DAYPART TABLE                                
         BASR  RE,0                                                             
         AHI   RE,DAYPARTT-*                                                    
         STCM  RE,15,LP_BLKS+((B#DAYP-1)*L'LP_BLKS)                             
                                                                                
B#CONT   EQU   11                  CONTENT TABLE                                
         L     RE,ACONTTAB                                                      
         STCM  RE,15,LP_BLKS+((B#CONT-1)*L'LP_BLKS)                             
                                                                                
B#AIRN   EQU   12                  AIRING TYPE TABLE                            
         L     RE,AAIRNTAB                                                      
         STCM  RE,15,LP_BLKS+((B#AIRN-1)*L'LP_BLKS)                             
                                                                                
B#NADCAT EQU   13                  NAD CATEGORIES TABLE                         
         L     RE,APFXTAB                                                       
         STCM  RE,15,LP_BLKS+((B#NADCAT-1)*L'LP_BLKS)                           
                                                                                
B#DEM    EQU   14                  NTI DEMO CATEGORIES                          
         L     RE,ANTIDEMS                                                      
         STCM  RE,15,LP_BLKS+((B#DEM-1)*L'LP_BLKS)                              
                                                                                
B#STA    EQU   15                  STATION RECORD AREA                          
         MVC   LP_BLKS+((B#STA-1)*L'LP_BLKS),AIO4                               
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         CLC   LP_QMAPN,INIDWN#                                                 
         JE    RUNINIT                                                          
         CLC   LP_QMAPN,LTSTBK#                                                 
         JE    RUNLTBK                                                          
         CLC   LP_QMAPN,SMASTR#                                                 
         JE    RUNSMAST                                                         
         CLC   LP_QMAPN,DDEF#                                                   
         JE    RUNDDEF                                                          
         CLC   LP_QMAPN,DDSTM#                                                  
         JE    RUNDDSTM                                                         
         CLC   LP_QMAPN,DEINFO#                                                 
         JE    RUNDEIN                                                          
         J     EXITY                                                            
                                                                                
RUNINIT  GOTOR INIDOWN                                                          
         J     EXITY                                                            
RUNLTBK  GOTOR LTBKDOWN                                                         
         J     EXITY                                                            
RUNSMAST GOTOR SMSTDOWN                                                         
         J     EXITY                                                            
RUNDDEF  GOTOR DDEFDOWN                                                         
         J     EXITY                                                            
RUNDDSTM GOTOR DDSTDOWN                                                         
         J     EXITY                                                            
RUNDEIN  GOTOR DEINDOWN                                                         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* BUMP THROUGH NET MASTER STATION RECORDS                             *         
***********************************************************************         
                                                                                
NXTSMAS  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',STAKEYT),('B#STA',0),    *        
               ('STAFILQ',SAVED),0,0                                            
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
*        USING STAREC,R2           R2=A(STATION RECORD)                         
*        CLI   STYPE,C'N'          MEDIA=NETWORK                                
*        BE    NXTSMA04                                                         
*        CLI   STYPE,C'S'          MEDIA=SYNDICATION                            
*        BE    NXTSMA04                                                         
*        CLI   STYPE,C'C'          MEDIA=CABLE                                  
*        BE    NXTSMA04                                                         
*        B     NXTSMAS             DON'T INCLUDE OTHER MEDIAS                   
                                                                                
NXTSMA04 MVC   KEYSAVE,IOKEY       SAVE STATION KEY                             
         XC    NETNAME,NETNAME                                                  
                                                                                
         XC    MKTKEY,MKTKEY       READ MARKET RECORD FOR NETWORK NAME          
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVI   MKTKMED,NETMEDQ                                                  
         MVC   MKTKMKT,SMKT-STAREC(R2)                                          
         MVC   MKTKAGY,AGENCY                                                   
         MVI   MKTKFILL,C'0'                                                    
         MVC   MKTKFILL+1(L'MKTKFILL-1),MKTKFILL                                
         MVC   IOKEY(MKTKEYLQ),MKTREC                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+#STAREC'                        
                                                                                
         MVC   IOKEY,KEYSAVE       RESTORE STATION RECORD KEY                   
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         MVC   NETNAME,MKTNAME-MKTREC(R2)                                       
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUMP THROUGH DEMDEF RECORDS                                         *         
***********************************************************************         
                                                                                
NXTDDEF  LA    R0,ODDCOD                                                        
         ST    R0,LP_ADATA                                                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   NXTD20                                                           
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NNDRECD,R2                                                       
         MVC   NNDKTYP,=X'0D16'                                                 
                                                                                
         L     R4,AAGYREC          GET AGENCY/MEDIA CODE                        
         MVI   ELCODE,AGYMEDEQ                                                  
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
NXTD10   BRAS  RE,NEXTEL                                                        
         JNE   NOMORE                                                           
         USING AGYMEDEL,R4                                                      
         CLI   AGYMEDCD,NETMEDQ   MEDIA 'N'                                     
         BNE   NXTD10                                                           
         MVC   NNDKAM,AGYMEDBT                                                  
         DROP  R4                                                               
                                                                                
         MVC   NNDCODE,DDCODE     IF DEMDEF CODE SPECIFIED, USE IT              
         OC    DDCODE,DDCODE                                                    
         BZ    NXTD18                                                           
         LA    RE,NNDCODE+L'NNDCODE-1                                           
NXTD16   CLI   0(RE),C' '                                                       
         BNE   NXTD18                                                           
         MVI   0(RE),0                                                          
         SHI   RE,1                                                             
         B     NXTD16                                                           
                                                                                
NXTD18   GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR'    READ HIGH                   
         B     NXTD30                                                           
                                                                                
NXTD20   OC    DDCODE,DDCODE       IF DEMDEF CODE SPECIFIED, STOP               
         JNZ   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR'    READ SEQUENTIAL             
         B     NXTD30                                                           
                                                                                
NXTD30   JNE   NOMORE                                                           
         CLC   IOKEY(3),IOKEYSAV                                                
         JNE   NOMORE                                                           
                                                                                
         OC    DDCODE,DDCODE       MAKE SURE IT'S SPECIFIED DEMDEF              
         BZ    NXTD32                                                           
         CLC   IOKEY(3+L'NNDCODE),IOKEYSAV                                      
         JNE   NOMORE                                                           
                                                                                
NXTD32   L     R0,AIO4             GET DEMDEF RECORD FROM FILE                  
         STCM  R0,15,IOADDR                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL'                               
         JNE   NOMORE                                                           
                                                                                
         L     R2,IOADDR                                                        
         MVC   ODDCOD,NNDCODE                                                   
         GOTOR DATCON,DMCB,(3,NNDACTD),(0,ODDDATE)                              
                                                                                
         XC    ODDTIMS,ODDTIMS                                                  
         L     R4,IOADDR                                                        
         MVI   ELCODE,NND0ACDQ     EXTENDED ACTIVITY ELEMENT                    
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BNE   NXTD35                                                           
         USING NNDEL0A,R4                                                       
         GOTOR DATCON,DMCB,(3,NND0ALDT),(0,ODDDATE)                             
         MVC   ODDTIMS,NND0ATIM                                                 
         DROP  R4                                                               
                                                                                
NXTD35   L     R4,IOADDR           DEMDEF RECORD                                
         MVI   ELCODE,NNDELQ       DEMO DEFINITION ELEMENT                      
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         LA    R3,ELEM                                                          
NXTD40   BNE   NXTD50                                                           
         USING NNDELDD,R4                                                       
         CLI   NNDMOD,C'V'                                                      
         BE    NXTD42                                                           
         CLI   NNDMOD,C'I'                                                      
         BNE   NXTD45              INCLUDE ONLY VPH'S                           
NXTD42   OC    NNDCAT,NNDCAT                                                    
         BZ    NXTD45              AND ONLY IF NADS                             
         ZIC   R1,1(R4)                                                         
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       MOVE 'DD' ELEMENTS TO ELEM                   
         AR    R3,RF                                                            
NXTD45   BRAS  RE,NEXTEL                                                        
         B     NXTD40                                                           
                                                                                
NXTD50   MVI   0(R3),0             EOR                                          
                                                                                
         CLI   ELEM,0              NO NAD VPH'S FOUND?                          
         BE    NXTD20              DON'T SEND. READ NEXT RECORD                 
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUMP THROUGH COMDEF RECORDS                                         *         
***********************************************************************         
                                                                                
NXTCDEF  LA    R0,ODDCOD                                                        
         ST    R0,LP_ADATA                                                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   NXTCD20                                                          
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NNDRECD,R2                                                       
         MVC   NNDKTYP,=X'0D16'                                                 
                                                                                
         L     R4,AAGYREC          GET AGENCY/MEDIA CODE                        
         MVI   ELCODE,AGYMEDEQ                                                  
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
NXTCD10  BRAS  RE,NEXTEL                                                        
         JNE   NOMORE                                                           
         USING AGYMEDEL,R4                                                      
         CLI   AGYMEDCD,NETMEDQ   MEDIA 'N'                                     
         BNE   NXTCD10                                                          
         MVC   NNDKAM,AGYMEDBT                                                  
         DROP  R4                                                               
                                                                                
         MVC   NNDCODE,DDCODE     IF COMDEF CODE SPECIFIED, USE IT              
         OC    DDCODE,DDCODE                                                    
         BZ    NXTCD18                                                          
         LA    RE,NNDCODE+L'NNDCODE-1                                           
NXTCD16  CLI   0(RE),C' '                                                       
         BNE   NXTCD18                                                          
         MVI   0(RE),0                                                          
         SHI   RE,1                                                             
         B     NXTCD16                                                          
                                                                                
NXTCD18  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR'    READ HIGH                   
         B     NXTCD30                                                          
                                                                                
NXTCD20  OC    DDCODE,DDCODE       IF DEMDEF CODE SPECIFIED, STOP               
         JNZ   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR'    READ SEQUENTIAL             
         B     NXTCD30                                                          
                                                                                
NXTCD30  JNE   NOMORE                                                           
         CLC   IOKEY(3),IOKEYSAV                                                
         JNE   NOMORE                                                           
         CLI   IOKEY+9,NNDKRSCQ    COMDEF?                                      
         JNE   NXTCD20                                                          
                                                                                
         OC    DDCODE,DDCODE       MAKE SURE IT'S SPECIFIED DEMDEF              
         BZ    NXTCD32                                                          
         CLC   IOKEY(3+L'NNDCODE),IOKEYSAV                                      
         JNE   NOMORE                                                           
                                                                                
NXTCD32  L     R0,AIO4             GET DEMDEF RECORD FROM FILE                  
         STCM  R0,15,IOADDR                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL'                               
         JNE   NOMORE                                                           
                                                                                
         L     R2,IOADDR                                                        
         MVC   ODDCOD,NNDCODE                                                   
         GOTOR DATCON,DMCB,(3,NNDACTD),(0,ODDDATE)                              
                                                                                
         XC    ODDTIMS,ODDTIMS                                                  
         L     R4,IOADDR                                                        
         MVI   ELCODE,NND0ACDQ     EXTENDED ACTIVITY ELEMENT                    
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BNE   NXTCD35                                                          
         USING NNDEL0A,R4                                                       
         GOTOR DATCON,DMCB,(3,NND0ALDT),(0,ODDDATE)                             
         MVC   ODDTIMS,NND0ATIM                                                 
         DROP  R4                                                               
                                                                                
NXTCD35  L     R4,IOADDR           COMDEF RECORD                                
         MVI   ELCODE,NNDELQ       DEMO DEFINITION ELEMENT                      
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         LA    R3,ELEM                                                          
NXTCD40  BNE   NXTCD50                                                          
         USING NNDELDD,R4                                                       
NXTCD42  OC    NNDCDEMO,NNDCDEMO                                                
         BZ    NXTCD45             AND ONLY IF COMSCORE DEMO                    
         ZIC   R1,1(R4)                                                         
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       MOVE 'DD' ELEMENTS TO ELEM                   
         AR    R3,RF                                                            
NXTCD45  BRAS  RE,NEXTEL                                                        
         B     NXTCD40                                                          
                                                                                
NXTCD50  MVI   0(R3),0             EOR                                          
                                                                                
         CLI   ELEM,0              NO NAD VPH'S FOUND?                          
         BE    NXTCD20             DON'T SEND. READ NEXT RECORD                 
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
STAFILQ  EQU   C'X'                STATION FILE                                 
NETMEDQ  EQU   C'N'                NETWORK MEDIA LETTER                         
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* EDIT DEMDEF MODIFIER                                                *         
***********************************************************************         
                                                                                
EDTMOD   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),C'V'          SEND MODIFIER                                
         CLI   0(R2),C'I'          'V' INSTEAD OF 'I'                           
         BE    *+10                                                             
         MVC   0(1,R4),0(R2)                                                    
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONVERT TYPE-3 TO TYPE-4 DEMO NUMBERS                               *         
***********************************************************************         
                                                                                
GETYP4D  L     R2,LP_AINP                                                       
         L     R4,LP_AOUT                                                       
                                                                                
         GOTOR (#TYP3TO4,ATYP3TO4),DMCB,0(R2),HALF1                             
         EDIT  HALF1,(5,(R4)),ALIGN=LEFT                                        
                                                                                
         SR    R0,R0               GET LENGTH OF OUTPUT                         
         LR    R2,R4                                                            
GET4_10  CLI   0(R2),C' '                                                       
         BE    GET4_20                                                          
         AHI   R0,1                                                             
         LA    R2,1(R2)                                                         
         B     GET4_10                                                          
GET4_20  STCM  R0,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXCLUDE BROADCAST DUPLICATE STATIONS                                *         
***********************************************************************         
                                                                                
NBDUPS   L     R2,LP_AINP                                                       
                                                                                
         USING NEBRDNMD,R2                                                      
         TM    NEBRDFLG,NEBRDFLG_NOT_UNIQUE                                     
         JO    EXITN            EXCLUDE DUPLICATE STATIONS                      
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIAL DOWNLOAD                                                    *         
***********************************************************************         
                                                                                
INIDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DMINDUR,DEFMINQ     DEFAULT MINIMUM DURATION                     
         MVI   MAXDYTM,MAXDTN      MAXIMUM NO OF DAYS/TIMES IN A DAYPRT         
*        MVI   NULLCHAR,NULLCQ     CHARACTER USED TO INDICATE NULLS             
         MVC   NULLIND,NULLCHRS    N/A -INDICATOR FOR DATA NOT AVALABLE         
         MVI   OPGTID,0            TEMP, UNTIL NEXT BUILD                       
                                                                                
         L     R2,AAGYREC                                                       
         USING AGYHDR,R2                                                        
         MVI   DPRECSN,C'N'                                                     
         TM    AGYFLAG2,AGYFLAG2_2DP                                            
         BNO   *+8                                                              
         MVI   DPRECSN,C'Y'                                                     
         TM    AGYFLAG2,AGYFLAG2_BDP                                            
         BNO   *+8                                                              
         MVI   DPRECSN,C'B'                                                     
         DROP  R2                                                               
                                                                                
         MVI   BMEDIA,MEDNET                                                    
         MVI   SMEDIA,MEDSYN                                                    
         MVI   CMEDIA,MEDCAB                                                    
                                                                                
         XC    IOKEY,IOKEY         GET THE N2 PROFILE                           
         MVC   IOKEY(4),=C'S0N2'                                                
         MVC   IOKEY+4(2),LP_AGY                                                
         MVI   IOKEY+6,C'N'                                                     
         GOTOR GETPROF,DMCB,IOKEY,(C'M',N2PROF),DATAMGR                         
         MVC   GAAPROF,N2PROF+4                                                 
                                                                                
         BRAS  RE,GETTOK           GET COMSCORE TOKEN RECORD                    
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
INIDOWNX J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMSCORE TOKEN RECORD                                               *         
***********************************************************************         
                                                                                
GETTOK   NTR1  BASE=*,LABEL=*                                                   
         XC    CSTOKEN,CSTOKEN                                                  
         XC    CSRSCD,CSRSCD                                                    
         XC    CSRSME,CSRSME                                                    
         XC    CSMASH,CSMASH                                                    
         XC    CSDAYS,CSDAYS                                                    
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
*                                                                               
         LA    R2,IOKEY                                                         
         USING TOKKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   TOKKMIN,TOKKMINQ      C'K'                                       
         MVI   TOKKTYP,TOKKRTRK      X'01' - RENTRAK RECORD                     
         MVC   TOKKAAGY,LP_AGY       AGENCY ALPHA CODE                          
         MVC   TOKKSAGY,FATAGYSC     SECURITY AGENCY CODE                       
         MVI   TOKKSYS,X'03'         NET SYSTEM                                 
         DROP  R1                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR'    READ HIGH                   
         BNE   GETTOKX                                                          
         CLC   IOKEY(L'TOKKEY),IOKEYSAV                                         
         BNE   GETTOKX                                                          
*                                                                               
         L     R0,AIO4             GET TOKEN RECORD FROM FILE                   
         STCM  R0,15,IOADDR                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL'                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,IOADDR                                                        
         AHI   R2,TOKFIRST             R4=A(1ST ELEMENT)                        
TOKEN10  CLI   0(R2),0                 ANY ELEMENTS?                            
         JE    TOKEN30                                                          
         CLI   0(R2),RTAUTELQ          X'0A' - RENTRAK AUTHOR ELEM?             
         JE    TOKEN20                                                          
         LLC   R0,1(R2)                CHECK THE NEXT ELEMENT                   
         AR    R2,R0                                                            
         J     TOKEN10                                                          
*                                                                               
         USING RTAUTHD,R2                                                       
TOKEN20  CLC   RTAUTID,SPACES          LICENSE ID BETTER BE > SPACES            
         JNH   TOKEN30                                                          
         LA    RF,CSTOKEN                                                       
         MVC   0(4,RF),=C'COM|'            comScore                             
         AHI   RF,4                                                             
         MVC   0(L'RTAUTID,RF),RTAUTID     LICENSE ID                           
         AHI   RF,L'RTAUTID                                                     
         MVI   0(RF),C'|'                                                       
         AHI   RF,1                                                             
         MVC   0(L'RTAUTSEC,RF),RTAUTSEC   SECURITY ID                          
         DROP  R2                                                               
*                                                                               
TOKEN30  GOTO1 GETFACT,DMCB,(X'80',0),F#SSBD                                    
         L     R1,0(R1)                                                         
         MVC   BYTE1,F@SSYSFL-F@SSBD(R1)   SYSTEM                               
*                                                                               
         LA    R2,IOKEY                                                         
         USING SDRRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   SDRKMIN,SDRKMINQ    X'5D'                                        
         MVC   SDRKFFL,BYTE1                                                    
         MVI   SDRKSYS,X'03'       NET                                          
         MVC   SDRKAPP,=X'0034'    AUDIENCE ESTIMATOR (FAXPEQUS)                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR'    READ HIGH                   
         B     TOKEN45                                                          
*                                                                               
TOKEN40  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOGENDIR'                                
TOKEN45  CLC   IOKEY(L'SDRKEY),IOKEYSAV                                         
         BNE   GETTOKX                                                          
*                                                                               
         L     R0,AIO4             GET TOKEN RECORD FROM FILE                   
         STCM  R0,15,IOADDR                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL'                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SDRRECD,R2                                                       
         L     R2,IOADDR                                                        
         LA    R2,SDRRFRST                                                      
TOKEN50  CLI   0(R2),0                                                          
         BE    GETTOKX                                                          
*                                                                               
         USING SDELD,R2                                                         
         CLI   SDEEL,X'02'         RATING SOURCE CONNECTION DATA                
         JNE   TOKEN55                                                          
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSRSCD(0),SDEDATA                                                
         J     TOKEN70                                                          
*                                                                               
TOKEN55  CLI   SDEEL,X'03'         RATING SOURCE METHOD ENDPOINTS               
         JNE   TOKEN60                                                          
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSRSME(0),SDEDATA                                                
         J     TOKEN70                                                          
*                                                                               
TOKEN60  CLI   SDEEL,X'04'         MASHERY KEY FOR RENTRAK API                  
         JNE   TOKEN65                                                          
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSMASH(0),SDEDATA                                                
         J     TOKEN70                                                          
*                                                                               
TOKEN65  CLI   SDEEL,X'05'         NUMBER OF DAYS                               
         JNE   TOKEN70                                                          
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSDAYS(0),SDEDATA                                                
         J     TOKEN70                                                          
*                                                                               
TOKEN70  ZIC   RF,SDELEN                                                        
         AR    R2,RF                                                            
         B     TOKEN50                                                          
*                                                                               
GETTOKX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LATEST BOOK DOWNLOAD                                                *         
***********************************************************************         
                                                                                
LTBKDOWN NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R4,AMEDIAT                                                       
         USING MEDIATD,R4                                                       
LTBK10   CLI   0(R4),FF                                                         
         BE    LTBKDWNX                                                         
                                                                                
         MVC   DBSELSRC,MDSELSRC                                                
         MVC   DBSELMED,MDSELMED                                                
         MVC   DBFILE,MDFILE                                                    
         MVC   DBSELSTA+4(1),MDSELST4                                           
         MVC   DBBTYPE,MDBTYPE                                                  
         MVI   DBFUNCT,DBGTNTLB                                                 
                                                                                
         MVC   DBAREC,AIO1                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,LP_AGY                                                  
                                                                                
         GOTOR DEMAND,DMCB,DBLOCK,LBHOOK                                        
                                                                                
         LA    R4,MEDIATL(R4)                                                   
         B     LTBK10                                                           
                                                                                
LBHOOK   L     RE,DBAREC                                                        
         MVC   HALF1,0(RE)                                                      
         GOTOR NETUNBK,DMCB,(C'W',HALF1),LBOOK,GETDAY,ADDAY,           *        
               GETBROAD                                                         
         MVC   LMEDIA,MDREQMED                                                  
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
LTBKDWNX J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* NET STATION MASTER RECORDS DOWNLOAD                                 *         
***********************************************************************         
                                                                                
SMSTDOWN NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   AGENCY,LP_AGY                                                    
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
SMSTDWNX J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMDEF RECORDS DOWNLOAD                                             *         
***********************************************************************         
                                                                                
DDEFDOWN NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
DDEFDWNX J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMDEF STAMP DOWNLOAD                                               *         
***********************************************************************         
                                                                                
DDSTDOWN NTR1  BASE=*,LABEL=*                                                   
                                                                                
         SR    R3,R3               GET DATE/TIME STAMP FOR ONE DEMDEF           
         ICM   R3,7,ADDCD          AT A TIME                                    
         BZ    DDSTDWNX                                                         
         SR    R1,R1                                                            
         ICM   R1,3,LW_NUMN-LW_D(R3)                                            
         LA    R3,LW_DATA2-LW_D(R3)                                             
                                                                                
DDST05   ST    R1,FULL1                                                         
         XC    IOKEY,IOKEY         BUILD DEMDEF KEY                             
         LA    R2,IOKEY                                                         
         USING NNDRECD,R2                                                       
         MVC   NNDKTYP,=X'0D16'                                                 
         MVC   NNDCODE,0(R3)                                                    
         LA    RE,NNDCODE+L'NNDCODE-1                                           
DDST07   CLI   0(RE),C' '          RIGHT PAD CODE WITH ZEROES                   
         BNE   DDST08                                                           
         MVI   0(RE),0                                                          
         SHI   RE,1                                                             
         B     DDST07                                                           
                                                                                
DDST08   L     R4,AAGYREC          GET AGENCY/MEDIA CODE                        
         MVI   ELCODE,AGYMEDEQ                                                  
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DDST10   BRAS  RE,NEXTEL                                                        
         BNE   DDSTDWNX                                                         
         USING AGYMEDEL,R4                                                      
         CLI   AGYMEDCD,NETMEDQ   MEDIA 'N'                                     
         BNE   DDST10                                                           
         MVC   NNDKAM,AGYMEDBT                                                  
         DROP  R4                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR'    READ HIGH                   
         BNE   DDST60                                                           
         CLC   IOKEY(NNDRLEN-NNDRECD),IOKEYSAV                                  
         BNE   DDST60                                                           
                                                                                
         L     R0,AIO4             GET DEMDEF RECORD FROM FILE                  
         STCM  R0,15,IOADDR                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL'                               
         BNE   DDST60                                                           
                                                                                
         L     R2,IOADDR           DEMDEF RECORD                                
         MVC   ODDCOD,NNDCODE                                                   
         GOTOR DATCON,DMCB,(3,NNDACTD),(0,ODDDATE)                              
                                                                                
         XC    ODDTIMS,ODDTIMS                                                  
         L     R4,IOADDR                                                        
         MVI   ELCODE,NND0ACDQ     EXTENDED ACTIVITY ELEMENT                    
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BNE   DDST30                                                           
         USING NNDEL0A,R4                                                       
         GOTOR DATCON,DMCB,(3,NND0ALDT),(0,ODDDATE)                             
         MVC   ODDTIMS,NND0ATIM                                                 
         DROP  R4                                                               
                                                                                
DDST30   GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
DDST60   LA    R3,DDCDLN(R3)       GET NEXT DEMDEF RECORD                       
         L     R1,FULL1                                                         
         BCT   R1,DDST05                                                        
                                                                                
DDSTDWNX J     EXITY                                                            
                                                                                
NNACTQ   EQU   X'01'               DEMDEF ACTIVITY ELEMENT                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMO ENGINE DOWNLOAD - DEMOGRAPHIC INFO                             *         
***********************************************************************         
                                                                                
DEINDOWN NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
DEINDWNX J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
         J     EXITY                                                            
                                                                                
EXITY    LHI   RE,1                CC=EQUAL FOR YES                             
         J     EXITCC                                                           
EXITN    LHI   RE,0                CC=NOT EQUAL FOR NO                          
EXITCC   CHI   RE,1                SET CONDITION CODE                           
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
GETEL    AH    R4,DATADISP                                                      
         J     NEXTEL2                                                          
NEXTEL   CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         ZIC   R0,1(R4)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
NEXTEL2  CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCODE,0(R4)                                                     
         JNE   NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
                                                                                
INIDWN#  DC    AL2(M#INIT)         INITIAL DOWNLOAD                             
LTSTBK#  DC    AL2(M#LTBK)         LATEST BOOK DOWNLOAD                         
SMASTR#  DC    AL2(M#SMAST)        STATION MASTER RECORDS                       
DDEF#    DC    AL2(M#DDEF)         DEMDEF RECORDS DOWNLOAD                      
DDSTM#   DC    AL2(M#DDSTM)        DEMDEF STAMP DOWNLOAD                        
DEINFO#  DC    AL2(M#DEINFO)       DEMO ENGINE DOWNLOAD - DEMO INFO             
         EJECT                                                                  
                                                                                
         EJECT                                                                  
LVALUES  DS    0D                                                               
         DC    A(PFXTAB)                                                        
         DC    A(MODIFTAB)                                                      
         DC    A(MIRTAB)                                                        
         DC    A(CONTAB)                                                        
         DC    A(CSSTAB)                                                        
         DC    A(CSVTAB)                                                        
                                                                                
         DC    C'NTI'                                                           
         DC    X'0000000001FFFFFFFFFF'                                          
LVALUESX DS    0X                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* KEY DRIVER TABLES                                                   *         
***********************************************************************         
STAKEYT  LKKEY H,STAKEY,SAVED      ** STATION KEY DRIVER TABLE **               
         LKKEY LIT,STAKTYPE,STAKTYPQ                                            
         LKKEY LIT,STAKMED,NETMEDQ                                              
         LKKEY RNG,STAKCALL,STARNG                                              
         LKKEY SIN,STAKAGY,AGENCY                                               
         LKKEY LIT,STAKCLT,C'0',L'STAKCLT+L'STAKFILL                            
         LKKEY E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS                                                        *         
***********************************************************************         
                                                                                
REQINIT  LKREQ *,M#INIT,OUTINIT    ** INITIAL DOWNLOAD **                       
                                                                                
REQLTBK  LKREQ *,M#LTBK,OUTLTBK    ** LATEST BOOK DOWNLOAD **                   
                                                                                
REQSMST  LKREQ *,M#SMAST,OUTSMAST  ** STATION MASTER RECDS DOWNLOAD **          
                                                                                
REQDEIN  LKREQ *,M#DEINFO,OUTDEIN  ** DEMO ENGINE DWNLD - INFO **               
                                                                                
REQDDEF  LKREQ H,M#DDEF,OUTDDEF    ** DEMDEF RECORDS DOWNLOAD **                
DRCd     LKREQ F,1,(D,B#SAVED,DDCODE),CHAR,COL=*,TEXT=NE#DDCD                   
         LKREQ E                                                                
                                                                                
REQDDST  LKREQ H,M#DDSTM,OUTDDSTM  ** DEMDEF STAMP DOWNLOAD **                  
DDCd     LKREQ F,1,(I,B#SAVED,DDCDIND),CHAR,COL=*,OLEN=DDCDLN,LIST=F,  *        
               MAXLEN=DDCDLN,TEXT=NE#DDCD                                       
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
OUTINIT  LKOUT H                   ** INITIAL DOWNLOAD **                       
                                                                                
MEDT     LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYMEDT)     MEDIA TYPES                                  
         LKOUT E                                                                
                                                                                
NETB     LKOUT R,2                                                              
BMed     LKOUT C,1,(D,B#SAVED,BMEDIA),CHAR                                      
Array    LKOUT C,2,(A,ARYBNET)     BROADCAST NETWORKS                           
         LKOUT E                                                                
                                                                                
NETS     LKOUT R,2                                                              
SMed     LKOUT C,1,(D,B#SAVED,SMEDIA),CHAR                                      
Array    LKOUT C,2,(A,ARYSNET)     SYNDICATION NETWORKS                         
         LKOUT E                                                                
                                                                                
NETC     LKOUT R,2                                                              
CMed     LKOUT C,1,(D,B#SAVED,CMEDIA),CHAR                                      
Array    LKOUT C,2,(A,ARYCNET)     CABLE NETWORKS                               
         LKOUT E                                                                
                                                                                
QRTS     LKOUT R,4                                                              
Array    LKOUT C,1,(A,ARYQRT)      NTI QUARTERS                                 
         LKOUT E                                                                
                                                                                
APTY     LKOUT R,5                                                              
Array    LKOUT C,1,(A,ARYNPTY)     NTI PROGRAM TYPES                            
         LKOUT E                                                                
                                                                                
ADPT     LKOUT R,6                                                              
Array    LKOUT C,1,(A,ARYNADPT)    NAD PROGRAM TYPES                            
         LKOUT E                                                                
                                                                                
SPTY     LKOUT R,7                                                              
Array    LKOUT C,1,(A,ARYSPTY)     SUB-PROGRAM TYPES                            
         LKOUT E                                                                
                                                                                
DAYP     LKOUT R,8                                                              
Array    LKOUT C,1,(A,ARYDAYP)     DAYPARTS                                     
         LKOUT E                                                                
                                                                                
CONT     LKOUT R,9                                                              
Array    LKOUT C,1,(A,ARYCONT)     CONTENT TYPES                                
         LKOUT E                                                                
                                                                                
AIRN     LKOUT R,10                                                             
Array    LKOUT C,1,(A,ARYAIRN)     AIRING TYPES                                 
         LKOUT E                                                                
                                                                                
NADC     LKOUT R,11                                                             
Array    LKOUT C,1,(A,ARYNADC)     NAD CATEGORIES                               
         LKOUT E                                                                
                                                                                
PDEM     LKOUT R,12                                                             
Optd     LKOUT C,1,(D,B#SAVED,OPGTID),CHAR    REMOVE AFTER NEXT BUILD           
Array    LKOUT C,3,(A,ARYPDEMS)    PROGRAM RECORD DEMO TABLES                   
         LKOUT E                                                                
                                                                                
MIRR     LKOUT R,13                                                             
Array    LKOUT C,1,(A,ARYMIR)      PROGRAM RECORD MIRROR CODES                  
         LKOUT E                                                                
                                                                                
CTCD     LKOUT R,14                                                             
Array    LKOUT C,1,(A,ARYCCD)      PROGRAM RECORD CONTENT CODES                 
         LKOUT E                                                                
                                                                                
CSS      LKOUT R,15                                                             
Array    LKOUT C,1,(A,ARYCSS)      comScore SOURCES                             
         LKOUT E                                                                
                                                                                
CSV      LKOUT R,16                                                             
Array    LKOUT C,1,(A,ARYCSV)      comScore VIEWING TYPES                       
         LKOUT E                                                                
                                                                                
CSTOK    LKOUT R,17                comScore TOKEN INFO                          
Token    LKOUT C,1,(D,B#SAVED,CSTOKEN),CHAR                                     
RtgSrce  LKOUT C,2,(D,B#SAVED,CSRSCD),CHAR                                      
RtgSEnd  LKOUT C,3,(D,B#SAVED,CSRSME),CHAR                                      
Mashery  LKOUT C,4,(D,B#SAVED,CSMASH),CHAR                                      
NumDays  LKOUT C,5,(D,B#SAVED,CSDAYS),UBIN                                      
         LKOUT E                                                                
                                                                                
MIND     LKOUT R,100               DEFAULT MINIMUM DURATION                     
DfMnD    LKOUT C,1,(D,B#SAVED,DMINDUR),UBIN,ND=Y                                
         LKOUT E                                                                
                                                                                
DPRE     LKOUT R,101               DECIMAL PRECISION                            
DePre    LKOUT C,1,(D,B#SAVED,DPRECSN),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
MAXD     LKOUT R,102               MAXIMUM NO OF D/T IN A DAYPART               
MaxDT    LKOUT C,1,(D,B#SAVED,MAXDYTM),UBIN,ND=Y                                
         LKOUT E                                                                
                                                                                
NULLC    LKOUT R,103               NULL CHARACTER INDICATES NO DATA             
NullIn   LKOUT C,1,(D,B#SAVED,NULLIND),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
GAAFL    LKOUT R,104               NULL CHARACTER INDICATES NO DATA             
GAA      LKOUT C,1,(D,B#SAVED,GAAPROF),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTLTBK  LKOUT H                   ** LATEST BOOK DOWNLOAD **                   
                                                                                
LTBK     LKOUT R,1                                                              
LMed     LKOUT C,1,(D,B#SAVED,LMEDIA),CHAR                                      
LBook    LKOUT C,2,(D,B#SAVED,LBOOK),EDAT                                       
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTSMAST LKOUT H                   ** STATION MASTER DOWNLOAD **                
                                                                                
         LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYMAS)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTDEIN  LKOUT H                   ** DEMO ENGINE DWNLD - DEMO INFO **          
                                                                                
NDEM     LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYNDEM)     DEMO CATEGORIES                              
         LKOUT E                                                                
                                                                                
NMOD     LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYMODF)     DEMO MODIFIERS                               
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTDDEF  LKOUT H                   ** DEMDEF RECORDS DOWNLOAD **                
                                                                                
DDEF     LKOUT R,1                                                              
         LKOUT C,1,(A,ARYDDEF)                                                  
         LKOUT E                                                                
                                                                                
CDEF     LKOUT R,2                                                              
         LKOUT C,1,(A,ARYCDEF)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTDDSTM LKOUT H                   ** DEMDEF STAMP DOWNLOAD **                  
                                                                                
DDST     LKOUT R,1                                                              
         LKOUT C,1,(D,B#SAVED,ODDCOD),CHAR,ND=Y                                 
         LKOUT C,2,(D,B#SAVED,ODDDATE),EDAT,ND=Y                                
         LKOUT C,3,(D,B#SAVED,ODDTIMS),LBIN                                     
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR BROADCAST NETWORKS DOWNLOAD                    *         
***********************************************************************         
                                                                                
ARYBNET  LKOUT A,(D,B#NETTB,NEBRDNMD),ROWWIDTH=NEBRDNLQ,EOT=FF                  
NBSKIP   EQU   *                                                                
NetCd    LKOUT C,2,(D,,NEBRDNET),CHAR,FILTROUT=NBDUPS,SKIPCOLS=NBSKIPS          
NetNm    LKOUT C,3,(D,,NEBRDNAM),CHAR                                           
NBSKIPS  EQU   (*-NBSKIP)/LX_COLSL                                              
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SYNDICATION NETWORKS DOWNLOAD                  *         
***********************************************************************         
                                                                                
ARYSNET  LKOUT A,(D,B#SYNTB,NESYNNMD),ROWWIDTH=NESYNNLQ,EOT=FF                  
SynCd    LKOUT C,2,(D,,NESYNNET),CHAR                                           
SynNm    LKOUT C,3,(D,,NESYNNAM),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CABLE NETWORKS DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYCNET  LKOUT A,(D,B#CBLTB,NECBNAMD),ROWWIDTH=NECBNMLQ,EOT=FF                  
CblNt    LKOUT C,2,(D,,NECBNALP),CHAR                                           
CblNm    LKOUT C,3,(D,,NECBNNAM),CHAR                                           
CblCd    LKOUT C,4,(D,,NECBNNML),UBIN                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MEDIA TYPES DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYMEDT  LKOUT A,(I,B#WORKD,AMEDIAT),ROWNAME=MEDIATD,                  *        
               ROWWIDTH=MEDIATL,EOT=FF                                          
MedCd    LKOUT C,1,(D,,MDREQMED),CHAR                                           
MedNm    LKOUT C,2,(D,,MDMNAME),CHAR                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NTI QUARTERS DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYQRT   LKOUT A,(D,B#NTIQ,NTIQD),ROWWIDTH=NTIQL,EOT=FF                         
QYear    LKOUT C,1,(D,,NTIQYEAR),CHAR                                           
QQrt     LKOUT C,2,(D,,NTIQQRT),CHAR                                            
QSdat    LKOUT C,3,(D,,NTIQSDAT),EDAT                                           
QEdat    LKOUT C,4,(D,,NTIQEDAT),EDAT                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NTI PROGRAM TYPES                              *         
***********************************************************************         
                                                                                
ARYNPTY  LKOUT A,(D,B#NTIPTY,NTIPTYD),ROWWIDTH=NTIPTYL,EOT=FF                   
NPtCd    LKOUT C,1,(D,,NPTYCODE),CHAR                                           
NPtNm    LKOUT C,2,(D,,NPTYNAME),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NAD PROGRAM TYPES                              *         
***********************************************************************         
                                                                                
ARYNADPT LKOUT A,(D,B#NADPTY,NADPTYD),ROWWIDTH=NADPTYL,EOT=FF                   
NdPtC    LKOUT C,1,(D,,NADPTYCD),CHAR                                           
NdPtN    LKOUT C,2,(D,,NADPTYNM),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SUB-PROGRAM TYPES                              *         
***********************************************************************         
                                                                                
ARYSPTY  LKOUT A,(D,B#SPTY,SPTYD),ROWWIDTH=SPTYL,EOT=FF                         
SPTCd    LKOUT C,1,(D,,SPTYCODE),CHAR                                           
SPTNm    LKOUT C,2,(D,,SPTYNAME),CHAR                                           
SPTP     LKOUT C,3,(D,,SPTYPROG),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DAYPARTS                                       *         
***********************************************************************         
                                                                                
ARYDAYP  LKOUT A,(D,B#DAYP,DAYPD),ROWWIDTH=DAYPL,EOT=FF                         
DptNa    LKOUT C,1,(D,,DAYPNAME),CHAR                                           
DptDe    LKOUT C,2,(D,,DAYPDEF),CHAR                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CONTENT TYPES                                  *         
***********************************************************************         
                                                                                
ARYCONT  LKOUT A,(D,B#CONT,CONTD),ROWWIDTH=CONTTABL,EOT=FF                      
CnCod    LKOUT C,1,(D,,CONTCODE),CHAR                                           
CnDes    LKOUT C,2,(D,,CONTDESC),CHAR                                           
CnMed    LKOUT C,3,(D,,CONTMEDS),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AIRING TYPES                                   *         
***********************************************************************         
                                                                                
ARYAIRN  LKOUT A,(D,B#AIRN,AIRND),ROWWIDTH=AIRNTABL,EOT=FF                      
AiCod    LKOUT C,1,(D,,AIRNCODE),CHAR                                           
AiDes    LKOUT C,2,(D,,AIRNDESC),CHAR                                           
AiMed    LKOUT C,3,(D,,AIRNMEDS),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NAD CATEGORIES                                 *         
***********************************************************************         
                                                                                
ARYNADC  LKOUT A,(D,B#NADCAT,NADCATD),ROWWIDTH=NADCATL,NROWS=PREFIXES           
NadNo    LKOUT C,1,(D,,NADNUM),UBIN                                             
NadAl    LKOUT C,2,(D,,NADALPH),CHAR                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR A LIST OF PROGRAM RECORD DEMOS                 *         
***********************************************************************         
                                                                                
ARYPDEMS LKOUT A,(I,B#WORKD,APRGDTAB),ROWNAME=DEMOPARD,                *        
               ROWWIDTH=DEMOPARL,EOT=FF                                         
PDems    LKOUT C,3,(D,,DEMPDEMO),(R,GETYP4D)                                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NTI DEMO CATEGORIES                            *         
***********************************************************************         
                                                                                
ARYNDEM  LKOUT A,(D,B#DEM,NTIDTD),ROWWIDTH=NTIDTL,EOT=FF                        
DName    LKOUT C,1,(D,,NTDNAME),CHAR                                            
DSex     LKOUT C,2,(D,,NTDSEX),CHAR                                             
DAge     LKOUT C,3,(D,,NTDAGE),CHAR                                             
DTyp4    LKOUT C,4,(D,,NTDTYP4),UBIN                                            
DTyp3    LKOUT C,5,(D,,NTDTYP3),UBIN,ND=Y                                       
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMO MODIFIERS                                 *         
***********************************************************************         
                                                                                
ARYMODF  LKOUT A,(I,B#SAVED,AMODIFT),ROWNAME=MODIFD,ROWWIDTH=MODIFL,   *        
               EOT=FF                                                           
Modif    LKOUT C,1,(D,,MODCOD),CHAR                                             
ModDe    LKOUT C,2,(D,,MODDESC),CHAR                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION MASTER RECORDS                         *         
***********************************************************************         
                                                                                
ARYMAS   LKOUT A,(R,NXTSMAS),MULTIROW=Y,ROWNAME=STAREC                          
MMedT    LKOUT C,1,(D,,STYPE),CHAR                                              
MPosT    LKOUT C,2,(D,,SPTYPE),CHAR                                             
MNetw    LKOUT C,3,(D,,STAKCALL),CHAR,LEN=L'STAKCALL-1                          
MNti     LKOUT C,4,(D,,SNTISTA),CHAR,ND=Y                                       
MMkNo    LKOUT C,5,(D,,SMKT),CHAR,ND=Y                                          
MMkNa    LKOUT C,6,(D,B#SAVED,NETNAME),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MIRROR CODES ON THE PROGRAM RECORD             *         
***********************************************************************         
                                                                                
ARYMIR   LKOUT A,(I,B#SAVED,AMIRTAB),ROWNAME=MIRTABD,ROWWIDTH=MIRTABL, *        
               EOT=FF                                                           
MirCd    LKOUT C,1,(D,,MIRCODE),CHAR                                            
MirNa    LKOUT C,2,(D,,MIRNAME),CHAR                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CONTENT CODES ON THE PROGRAM RECORD            *         
***********************************************************************         
                                                                                
ARYCCD   LKOUT A,(I,B#SAVED,ACCDTAB),ROWNAME=CONTABD,ROWWIDTH=CONTABL, *        
               EOT=FF                                                           
CcdCd    LKOUT C,1,(D,,CONTCD),CHAR                                             
CcdNm    LKOUT C,2,(D,,CONTNM),CHAR                                             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMDEF RECORDS FOR THIS AGENCY                 *         
***********************************************************************         
                                                                                
ARYDDEF  LKOUT A,(R,NXTDDEF),MULTIROW=Y                                         
DCod     LKOUT C,1,(D,B#SAVED,ODDCOD),CHAR,ND=Y                                 
DDat     LKOUT C,2,(D,B#SAVED,ODDDATE),EDAT,ND=Y                                
DTim     LKOUT C,3,(D,B#SAVED,ODDTIMS),LBIN                                     
Array    LKOUT C,11,(A,ARYDFDEM)                                                
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMOS ON THE DEMDEF RECORD                     *         
***********************************************************************         
                                                                                
ARYDFDEM LKOUT A,(D,B#WORKD,ELEM),EOT=0,                               *        
               ROWID=(NNDELDD,NNDELQ),ROWWIDTH=(V,NNDLEN)                       
DDMod    LKOUT C,11,(D,,NNDMOD),(R,EDTMOD),LEN=L'NNDMOD                         
DDNad    LKOUT C,12,(D,,NNDCAT),UBIN,ND=Y                                       
DDDem    LKOUT C,13,(D,,NNDDEMO),(R,GETYP4D),LEN=L'NNDDEMO                      
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMDEF RECORDS FOR THIS AGENCY                 *         
***********************************************************************         
                                                                                
ARYCDEF  LKOUT A,(R,NXTCDEF),MULTIROW=Y                                         
DCod     LKOUT C,1,(D,B#SAVED,ODDCOD),CHAR,ND=Y                                 
DDat     LKOUT C,2,(D,B#SAVED,ODDDATE),EDAT,ND=Y                                
DTim     LKOUT C,3,(D,B#SAVED,ODDTIMS),LBIN                                     
Array    LKOUT C,11,(A,ARYCFDEM)                                                
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMOS ON THE COMDEF RECORD                     *         
***********************************************************************         
                                                                                
ARYCFDEM LKOUT A,(D,B#WORKD,ELEM),EOT=0,                               *        
               ROWID=(NNDELDD,NNDELQ),ROWWIDTH=(V,NNDLEN)                       
DDMod    LKOUT C,11,(D,,NNDMOD),(R,EDTMOD),LEN=L'NNDMOD                         
DDDem    LKOUT C,12,(D,,NNDCDEMO),CHAR,LEN=L'NNDCDEMO                           
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR comScore SOURCES                               *         
***********************************************************************         
                                                                                
ARYCSS   LKOUT A,(I,B#SAVED,ACSST),ROWNAME=CSSTABD,ROWWIDTH=CSSTABL,   *        
               EOT=FF                                                           
Source   LKOUT C,1,(D,,CSSRCE),CHAR                                             
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR comScore VIEWING TYPES                         *         
***********************************************************************         
                                                                                
ARYCSV   LKOUT A,(I,B#SAVED,ACSVT),ROWNAME=CSVTABD,ROWWIDTH=CSVTABL,   *        
               EOT=FF                                                           
VType    LKOUT C,1,(D,,CSVTYPE),CHAR                                            
Desc     LKOUT C,2,(D,,CSVDESC),CHAR                                            
VType    LKOUT C,3,(D,,CSVTYPEC),CHAR                                           
         LKOUT E                                                                
                                                                                
         LKARY T                                                                
         EJECT                                                                  
DAYPARTT DS    0CL(DAYPL)                                                       
         DC    CL15'Early',CL30'Mon-Fri/7A-10A'                                 
         DC    CL15'Day',CL30'Mon-Fri/10A-430P'                                 
         DC    CL15'Prime',CL30'Mon-Sat/8P-11P+Sun/7P-11P'                      
         DC    CL15'Late',CL30'Mon-Fri/1130P-3A'                                
         DC    CL15'Saturday AM',CL30'Sat/8A-1P'                                
         DC    CL15'Weekend',CL30'Sat-Sun/7A-7P'                                
         DC    CL15'News',CL30'Mon-Sat/6P-7P+Sun/6P-7P'                         
DAYPARTX DC    X'FF'                                                            
                                                                                
MODIFTAB DS    0CL(MODIFL)                                                      
         DC    CL1'Y',CL25'Raw Impressions'                                     
         DC    CL1'I',CL25'Impressions'                                         
         DC    CL1'U',CL25'Universes'                                           
         DC    CL1'R',CL25'Ratings'                                             
         DC    CL1'V',CL25'VPH''s'                                              
         DC    CL1'B',CL25'GAA Raw Impressions'                                 
         DC    CL1'N',CL25'GAA Impressions'                                     
         DC    CL1'L',CL25'GAA Ratings'                                         
         DC    CL1'M',CL25'GAA VPH''s'                                          
         DC    CL1'Z',CL25'Program PUTS'                                        
         DC    CL1'S',CL25'Shares'                                              
         DC    X'FF'                                                            
                                                                                
CSSTAB   DS    0CL(CSSTABL)                                                     
         DC    CL3'PAV'                                                         
         DC    X'FF'                                                            
                                                                                
CSVTAB   DS    0CL(CSVTABL)                                                     
         DC    CL2'RL',CL15'Live',CL8'live'                                     
         DC    CL2'RC',CL15'Live Commercial',CL8'ad_live'                       
         DC    CL2'R3',CL15'Live + 3',CL8'c3'                                   
         DC    CL2'R7',CL15'Live + 7',CL8'c7'                                   
         DC    X'FF'                                                            
                                                                                
       ++INCLUDE DEPTYPS                                                        
                                                                                
       ++INCLUDE DESPTYPS                                                       
                                                                                
       ++INCLUDE DENADCATS                                                      
                                                                                
       ++INCLUDE NEPROGTABS                                                     
         EJECT                                                                  
                                                                                
NTIPTYD  DSECT                                                                  
NPTYCODE DS    CL2                                                              
NPTYNAME DS    CL40                                                             
NTIPTYL  EQU   *-NTIPTYD                                                        
                                                                                
NADPTYD  DSECT                                                                  
NADPTYCD DS    CL2                                                              
NADPTYNM DS    CL40                                                             
NADPTYL  EQU   *-NADPTYD                                                        
                                                                                
SPTYD    DSECT                                                                  
SPTYPROG DS    CL2                 PROGRAM TPE IT BELONGS TO                    
SPTYCODE DS    CL4                 SUBPROGRAM TYPE CODE                         
SPTYNAME DS    CL40                SUBPROGRAM TYPE NAME                         
SPTYL    EQU   *-SPTYD                                                          
                                                                                
DAYPD    DSECT                                                                  
DAYPNAME DS    CL15                                                             
DAYPDEF  DS    CL30                                                             
DAYPL    EQU   *-DAYPD                                                          
                                                                                
NADCATD  DSECT                                                                  
NADNUM   DS    AL1                                                              
NADALPH  DS    CL7                                                              
NADCATL  EQU   *-NADCATD                                                        
                                                                                
MODIFD   DSECT                                                                  
MODCOD   DS    CL1                                                              
MODDESC  DS    CL25                                                             
MODIFL   EQU   *-MODIFD                                                         
                                                                                
CSSTABD  DSECT                                                                  
CSSRCE   DS    CL3                                                              
CSSTABL  EQU   *-CSSTABD                                                        
                                                                                
CSVTABD  DSECT                                                                  
CSVTYPE  DS    CL2                                                              
CSVDESC  DS    CL15                                                             
CSVTYPEC DS    CL8                                                              
CSVTABL  EQU   *-CSVTABD                                                        
                                                                                
         EJECT                                                                  
         PRINT NOGEN                                                            
                                                                                
SAVED    DSECT                     ** SERVER SAVED W/S **                       
                                                                                
WVALUES  DS    0X                  ** LOCAL LITERAL VALUES **                   
                                                                                
ADCONS   DS    0A                                                               
APFXTAB  DS    A                                                                
AMODIFT  DS    A                                                                
AMIRTAB  DS    A                                                                
ACCDTAB  DS    A                                                                
ACSST    DS    A                                                                
ACSVT    DS    A                                                                
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
CNTI     DS    CL3                                                              
STARNG   DS    XL10                X'0000000001'-X'FFFFFFFFFF'                  
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
RELO     DS    A                   RELOCATION FACTOR                            
                                                                                
KEYSAVE  DS    XL(L'IOKEY)         LOCAL KEY SAVE AREA                          
                                                                                
ALASTPGE DS    A                   A(LAST ENTRY IN PROG DEMOS TABLE)            
                                                                                
SAVECLR  DS    0X                  ** START OF CLEARED AREA **                  
                                                                                
REQVALS  DS    0F                                                               
DDCDIND  DS    X                   DEDEF CODES INDICATOR                        
ADDCD    DS    AL3                 A(DEMDEF CODES)                              
IMEDIA   DS    C                                                                
DDCODE   DS    CL(DDCDLN)                                                       
REQVALSL EQU   *-REQVALS                                                        
                                                                                
OUTVALS  DS    0X                                                               
                                                                                
DMINDUR  DS    X                   DEFAULT MINIMUM DURATION                     
DPRECSN  DS    C                   DECIMAL PRECISION                            
MAXDYTM  DS    X                   MAX NO OF D/T IN A DAYPART                   
NULLCHAR DS    C                   CHARACTER TO INDICATE NULL DATA              
NULLIND  DS    CL(L'NULLCHRS)      CHARACTERS TO INDICATE NULL DATA             
LMEDIA   DS    C                   LATEST BOOK MEDIA                            
LBOOK    DS    CL6                 LATEST BOOK YYMMDD                           
BMEDIA   DS    C                   BROADCAST MEDIA                              
SMEDIA   DS    C                   SYNDICATION MEDIA                            
CMEDIA   DS    C                   CABLE MEDIA                                  
OPGTID   DS    CL1                 PROGRAM RECD DEMOS TABLE ID                  
PTYP4DEM DS    HL2                 TYPE-4 DEMO NUMBER                           
NETNAME  DS    CL(L'MKTNAME)       MARKET (NETWORK) NAME                        
ODDCOD   DS    CL(DDCDLN)          DEMDEF CODE FOR STAMP DOWNLOAD               
ODDDATE  DS    CL6                 DATE STAMP                                   
ODDTIMS  DS    HL4                 TIME STAMP IN 1/38400 SECS                   
GAAPROF  DS    C                   GAA FLAG FROM N2 PROFILE                     
                                                                                
CSTOKEN  DS    CL200               comScore TOKEN                               
CSRSCD   DS    CL200               RATING SOURCE CONNECTION DATA                
CSRSME   DS    CL200               RATING SOURCE METHOD ENDPOINTS               
CSMASH   DS    CL200               MASHERY KEY FOR API                          
CSDAYS   DS    CL1                 DAYS                                         
                                                                                
OUTVALSL EQU   *-OUTVALS                                                        
                                                                                
AGENCY   DS    CL(L'LP_AGY)                                                     
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
N2PROF   DS    CL16                                                             
                                                                                
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
SAVECLRL EQU   *-SAVECLR                                                        
                                                                                
FORMPTR  DS    A                                                                
         EJECT                                                                  
                                                                                
       ++INCLUDE NEPROGTABD                                                     
       ++INCLUDE SPGENNAD                                                       
       ++INCLUDE NELNKWRK                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE GEGENSDR                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029NELNK10   12/07/20'                                      
         END                                                                    
