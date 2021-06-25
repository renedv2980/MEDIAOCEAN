*          DATA SET FATABSDSPS AT LEVEL 061 AS OF 05/01/02                      
*PHASE TABSDSPA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'FATABSDSP - HANDLE TABLES DATA SPACE '                          
         PRINT NOGEN                                                            
TABSDSP  CSECT                                                                  
         NBASE WORKX-WORKD,*TABSDSP,=A(WORKAREA),RA,R9                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         BAS   RE,INIT             READ CARDS ETC                               
*                                                                               
         CLI   MODE,C'I'           INITIALISE                                   
         BNE   TDSP04                                                           
*                                                                               
         BAS   RE,MAIN             MAIN LOOP FOR MODE=INIT                      
         BAS   RE,MESSAGE          OUTPUT INITIALISED MESSAGE                   
*                                                                               
         CLI   TEST,C'Y'           TEST RUN?                                    
         BNE   TDSP02                                                           
         BAS   RE,RESULTS                                                       
         CLI   KILL,C'Y'                                                        
         BNE   TDSP04                                                           
         DC    H'0',C'KILL PROCESS'                                             
*                                                                               
TDSP02   BAS   RE,SETWAIT                                                       
         B     XBASE                                                            
*                                                                               
TDSP04   B     XBASE                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVC   DNUTL,DTNUTL        SET UP EYECATCHERS                           
         MVC   DNTBUFF,DTNTBUFF                                                 
         MVC   DNTPRNT,DTNTPRNT                                                 
         MVC   DNLOCK,DTNLOCK                                                   
         MVC   DNJOB,DTNJOB                                                     
         MVC   DNIAM,DTNIAM                                                     
         MVC   DNTST,DTNTST                                                     
         MVC   DNVRSN,DTNVRSN                                                   
         MVC   DNBRD,DTNBRD                                                     
         MVC   DNDARE,DTNDARE                                                   
         MVC   DNASS,DTNASS                                                     
         MVC   DNUSR1,DTNUSR1                                                   
         MVC   DNUSR2,DTNUSR2                                                   
         MVC   DNUSR3,DTNUSR3                                                   
         MVC   DNIDS,DTNIDS                                                     
         MVC   DNDDSP,DTNDISP                                                   
         MVC   DNDBOK,DTNBOOK                                                   
         MVC   DNDSTN,DTNSTN                                                    
         MVC   DNDMST,DTNMSTR                                                   
         MVC   DNDNAM,DTNNAME                                                   
         MVC   DNDCDE,DTNCODE                                                   
         MVC   DNDCTL,DTNCNTRL                                                  
         MVC   DNDADJ,DTNADJST                                                  
         MVC   DNDFRM,DTNFRMLA                                                  
         MVC   DNDFMTB,DTNFMTAB                                                 
         MVC   DNDALF,DTNAMKT                                                   
         MVC   DNDNUN,DTNNUNV                                                   
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLED(L'CTITLE),CTITLE   SET UP TITLE                           
         BAS   RE,PRINTI           INIT PRINTING                                
         MVC   PLINE,SPACES                                                     
         MVC   PLINED(L'CTITLEU),CTITLEU SET UP TITLE UNDERLINE                 
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
         LA    R2,CARD                                                          
*                                                                               
INIT02   GOTO1 VCARDS,DMCB,(R2),=C'RE00'                                        
         CLC   =C'/*',0(R2)        END OF CARDS?                                
         BE    INIT04              YES                                          
         MVC   PLINE+1(L'CARD),CARD                                             
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R2               VALIDATE KEYWORD=VALUE                       
         BAS   RE,CARDVAL                                                       
         BE    INIT02                                                           
         B     XBASE               CC:NE MEANS INVALID KEYWORD                  
*                                                                               
INIT04   CLC   =CL4'TEST',RUN      TEST RUN GOES WITHIN CORE                    
         BNE   *+8                                                              
         MVI   TEST,C'Y'                                                        
*                                                                               
         CLI   MODE,C'I'           TEST INIT MODE                               
         BNE   EXITOK                                                           
*                                                                               
         BAS   RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         LA    R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET SYSTEM INITIALISED MESSAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
MESSAGE  NTR1  ,                                                                
         MVC   MSGFAC,=CL4'????'                                                
         CLC   TABSREP,SYSTEM      REP DATASPACE?                               
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'REP '                                                
         CLC   TABSADV,SYSTEM      ADV DATASPACE?                               
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'ADV '                                                
         CLC   TABSTST,SYSTEM      TEST DATASPACE?                              
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'TEST'                                                
         CLC   TABSBOTH,SYSTEM     BOTH ADV+REP SYSTEMS?                        
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'PROD'                                                
         LA    R3,MSGL                                                          
         WTO   TEXT=(R3)                                                        
*                                                                               
LOGGERX  XIT1  ,                                                                
*                                                                               
MSGL     DC    AL2(60)                                                          
MSG1     DC    CL60' '                                                          
         ORG   MSG1                                                             
         DC    C'TABS dataspace initialised for '                               
MSGFAC   DC    CL04' '                                                          
         ORG   MSG1+L'MSG1                                                      
         EJECT                                                                  
***********************************************************************         
* MAIN CODE FOR MODE=INIT                                             *         
***********************************************************************         
         SPACE 1                                                                
MAIN     NTR1                                                                   
         BAS   RE,FRKPAGES         SET COUNT OF 4K PAGES REQUIRED               
*                                                                               
         CLI   TEST,C'Y'           TEST RUN GOES WITHIN CORE                    
         BNE   MAIN02                                                           
         BAS   RE,GETMAIN                                                       
         B     MAIN04                                                           
*                                                                               
MAIN02   BAS   RE,FREESPC          FREE ANY OLD DATASPACES                      
         BAS   RE,MAKESPC          MAKE A NEW ONE                               
         BAS   RE,GETSPC           GET ADDRESS OF IT                            
*                                                                               
         MVC   AHEADER,DMOFFS                                                   
         MVC   ADSDATA,DMOFFS      STARTING FROM OFFS                           
*                                                                               
MAIN04   L     R1,ADSDATA                                                       
         LA    RF,(RMAXRES-1)/64   PAGES FOR HEADERS                            
         LA    RF,1(RF)                                                         
         SLL   RF,12               * 4K                                         
         AR    R1,RF                                                            
         ST    R1,ADSDATA          A(FIRST TABLE ENTRY)                         
*                                                                               
         BAS   RE,ADCONS           BUILD ADCONS                                 
         BAS   RE,ADCINI           INITIALISE ADCON BLOCKS                      
*                                                                               
         BAS   RE,HEADERS          BUILD HEADER ENTRIES                         
         BAS   RE,TABLES           BUILD TABLE ENTRIES                          
*                                                                               
MAINX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET PAGES REQUIRED FOR EACH TABLE. TOTAL RETURNED IN (PAGES)        *         
***********************************************************************         
         SPACE 1                                                                
FRKPAGES NTR1  ,                                                                
         LA    RF,(((RMAXRES-1)/64)+1)   PAGES FOR TABLE HEADERS                
*                                                                               
         LA    RF,1(RF)            +1 PAGE FOR DUMPS                            
*                                                                               
         LA    RE,TZIPDSP          + PAGES FOR ZIP BLOCK                        
         AH    RE,=Y(TZBLKDSP)                                                  
         A     RE,=A(TZBUFDSP)                                                  
         SRL   RE,12                                                            
         LA    RE,1(RE)                                                         
         AR    RF,RE                                                            
*                                                                               
         L     RE,NLOCKS           + PAGES FOR LOCKTAB                          
         MHI   RE,TLKLEN                                                        
         SRL   RE,12                                                            
         LA    RE,1(RE)                                                         
         AR    RF,RE                                                            
*                                                                               
         AHI   RF,TABRUNPG         + PAGES FOR SERVER TABLE                     
*                                                                               
         LA    RE,TPQQCNT          + PAGES FOR PQ INDEX QUEUES                  
         MHI   RE,TPQQLEN                                                       
         AHI   RE,TPQLENQ+TPQLID                                                
         SRL   RE,12                                                            
         LA    RE,1(RE)                                                         
         AR    RF,RE                                                            
*                                  + PAGES FOR TEMPEST BUFFER DETAILS           
         L     RE,TPST             # ENTRIES                                    
         SRL   RE,5                                                             
         AHI   RE,1                ROUNDED UP INTO FULLWORDS                    
         SLL   RE,2                DITTO           BYTES                        
         AHI   RE,TTSSHDRL         ADD HEADER                                   
         ST    RE,TPSTF            AND SAVE                                     
*                                                                               
         SLL   RE,4                16*ENTRIES   (FOR 16 FACPAKS MAX)            
         AHI   RE,TTMSHDRL         PLUS HEADER LENGTH                           
*                                                                               
         SRL   RE,12               TURN INTO 4K PAGES                           
         LA    RE,1(RE)                                                         
         AR    RF,RE                                                            
*                                                                               
         L     R0,SSCT             SCRIPT TRACE BUFFER SIZE                     
         A     R0,STRC             TRACE BUFFER SIZE                            
         LTR   R0,R0               TRACE BUFFERS?                               
         BZ    FRKP02              NO                                           
         SRDL  R0,32                                                            
*                                                                               
         ICM   RE,15,DCTST         NUMBER OF TSTTAB ENTRIES                     
         BZ    FRKP02              NO TSTTAB                                    
         BCTR  RE,0                FIRST LINE TAKEN IS SPECIAL                  
*                                                                               
         MR    R0,RE               * NUMBER OF TSTTAB ENTRIES                   
         LA    R1,4095(R1)                                                      
         SRL   R1,12               ROUND TO NEXT HIGHEST 4K MULTIPLE            
         AR    RF,R1               ADD TO TOTAL REQUIRED                        
*                                                                               
FRKP02   LA    R3,HDRTAB           TABLE OF DATASPACE ENTRIES                   
         USING HDRTABD,R3                                                       
*                                                                               
FRKP04   CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    FRKP06              YES                                          
         ICM   R1,15,HDRNUM          NUMBER OF ENTRIES                          
         MH    R1,HDRWIDTH         * WIDTH OF 1 ENTRY                           
         LA    R1,4095(R1)                                                      
         SRL   R1,12               ROUND TO NEXT HIGHEST 4K MULTIPLE            
         AR    RF,R1               ADD TO TOTAL REQUIRED                        
         LA    R3,HDRTABL(R3)      NEXT TABLE ENTRY                             
         B     FRKP04                                                           
*                                                                               
FRKP06   ST    RF,PAGES            SET TOTAL NUMBER OF PAGES REQUIRED           
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD ADCONS                                                        *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   NTR1  ,                                                                
         LAM   R2,R2,DMALET                                                     
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         L     RF,ADSDATA                                                       
         STCM  RF,15,TABSDUMP      DUMP HEADERS ARE ALLOWED 4K                  
         AHI   RF,FOURK                                                         
*                                                                               
         STCM  RF,15,TABSTMS       DUMP HEADERS ARE ALLOWED 4K                  
         ICM   RE,15,TPSTF         + PAGES FOR TEMPEST BUFFER DETAILS           
         SLL   RE,4                16*ENTRIES                                   
         AHI   RE,TTMSHDRL                                                      
         SRL   RE,12                                                            
         LA    RE,1(RE)                                                         
         SLL   RE,12                                                            
         AR    RF,RE                                                            
*                                                                               
         STCM  RF,15,TABSZIP       ZIP BLOCK HAS EQUATED LENGTHS                
         A     RF,=A(TZIPDSP+TZBLKDSP+TZBUFDSP+TABSID+TABSID)                   
         LA    RF,4095(RF)                                                      
         SRL   RF,12                                                            
         SLL   RF,12               ROUND UP TO NEXT 4K                          
*                                                                               
         STCM  RF,15,TABSRUN       SET A(SERVER TABLE)                          
         AHI   RF,TABRUNPG*FOURK   (#PAGES*4096)                                
*                                                                               
         STCM  RF,15,TABSPQNX      PQ BLOCK HAS EQUATED VALUES                  
         A     RF,=A((TPQQCNT*TPQQLEN)+TPQLID+TPQLENQ)                          
         LA    RF,4095(RF)                                                      
         SRL   RF,12                                                            
         SLL   RF,12               ROUND UP TO NEXT 4K                          
*                                                                               
         STCM  RF,15,TABSLOX                                                    
         ICM   RE,15,NLOCKS                                                     
         MHI   RE,TLKLEN                                                        
         AR    RF,RE                                                            
         LA    RF,4095(RF)                                                      
         SRL   RF,12                                                            
         SLL   RF,12               ROUND UP TO NEXT 4K                          
*                                                                               
         STCM  RF,15,ADSDATA                                                    
         SAC   0                                                                
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE ADCONS                                                   *         
***********************************************************************         
         SPACE 1                                                                
ADCINI   NTR1  ,                                                                
         BRAS  RE,SETDMPS      *** BUILD DUMP HEADER DETAILS                    
*                                                                               
ADCI18   LAM   R2,R2,DMALET    *** BUILD PKZIP BLOCKS                           
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSZIP                                                    
         USING TABSZIPD,R2                                                      
         MVC   0(16,R2),=CL16'*ZIP*ZIP*ZIP*ZIP'                                 
         LA    R2,TABSID(,R2)                                                   
*                                                                               
         LA    R0,TAZNUM                                                        
ADCI20   XC    TABSZIPD(TZLENQ),TABSZIPD                                        
         LA    R2,TZLENQ(,R2)                                                   
         BCT   R0,ADCI20                                                        
*                                                                               
         MVC   0(16,R2),=CL16'*ZIPHDR**ZIPHDR*'                                 
         LA    R2,TABSID(,R2)                                                   
         LR    R3,R2                                                            
         AH    R3,=Y(TZBLKDSP)     R3 POINTS TO OUTPUT BUFFERS                  
*                                                                               
         USING TZIPHDRD,R2                                                      
         LA    R0,TZHNUM                                                        
         L     RF,=A(TZBUFLEN)                                                  
ADCI22   XC    TZIPHDRD(TZHLENQ),TZIPHDRD                                       
         STCM  R3,15,TZHBODY                                                    
         STCM  RF,15,TZHBLEN                                                    
         LA    R2,TZHLENQ(,R2)                                                  
         AR    R3,RF                                                            
         BCT   R0,ADCI22                                                        
*                                                                               
ADCI24   LAM   R2,R2,DMALET    *** BUILD SERVER INDEX BLOCKS                    
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSRUN                                                    
         USING TABSRUND,R2                                                      
         MVC   TABSLRUN,=CL16'*RUN*RUN*RUN*RUN'                                 
         LR    R0,R2                                                            
         AHI   R0,TABSRUNL                                                      
         ST    R0,TABSASVR         SET A(SERVER TABLE)                          
         AHI   R0,TABSVRLN                                                      
         ST    R0,TABSAQUE         SET A(QUEUE TABLE)                           
         LHI   R0,TABSSVRN                                                      
         STH   R0,TABSNSVR         SET N'SERVER ENTRIES                         
         LHI   R0,TABSQUEN                                                      
         STH   R0,TABSNQUE         SET N'QUEUE ENTRIES                          
*                                                                               
         LAM   R2,R2,DMALET    *** BUILD PQ INDEX BLOCKS                        
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSPQNX                                                   
         MVC   0(TPQLID,R2),=CL16'*PQI*PQI*PQI*PQI'                             
         AHI   R2,TPQLENQ                                                       
         MVC   0(TPQLID,R2),=CL16'*PQQ*PQQ*PQQ*PQQ'                             
*                                                                               
ADCI26   LAM   R2,R2,DMALET    *** BUILD LOCKTABLE                              
         L     R2,AHEADER                                                       
         SAC   512                                                              
         ICM   R2,15,TABSLOX                                                    
         ICM   RF,15,NLOCKS                                                     
         STCM  RF,15,12(R2)    SET NUMBER OF ENTRIES                            
         MHI   RF,TLKLEN                                                        
         AR    RF,R2                                                            
         STCM  RF,15,4(R2)     SET A(END)                                       
         MVI   8(R2),C'Y'                                                       
         LA    RF,TLKLEN(R2)                                                    
         STCM  RF,15,0(R2)     SET A(CURRENT)                                   
*                                                                               
ADCI28   LAM   R2,R2,DMALET    *** BUILD TEMPEST INFORMATION                    
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSTMS                                                    
         USING TTMSHDRD,R2                                                      
         XC    0(TTMSHDRL,R2),0(R2)                                             
         MVC   0(8,R2),=CL8'*TEMPEST'                                           
         ICM   RE,15,TPST                                                       
         STCM  RE,15,TTMSNTRY                                                   
*                                                                               
ADCI30   DS    0H              *** NEXT ADCON HERE                              
         B     ADCIX                                                            
*                                                                               
ADCIX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER INFO                                                   *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
         SPACE 1                                                                
HEADERS  NTR1  ,                                                                
         LAM   R2,R2,DMALET                                                     
         L     R2,AHEADER                                                       
         SAC   512                 SET UP AMODE                                 
         USING DMSPACED,R2                                                      
*                                                                               
         LA    R3,HDRTAB           TABLE OF DATASPACE ENTRIES                   
         USING HDRTABD,R3                                                       
         B     HDR04               FIRST 64 BYTES FREE                          
*                                                                               
HDR02    CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    HDR06               YES                                          
*                                                                               
         STCM  R2,15,HDRADR        SAVE A(IN DATASPACE) FOR LATER               
         MVC   DSPNAME,HDRNAME     EYECATCHER                                   
         MVC   DSPTYPE,HDRTYPE     TYPE                                         
         ICM   RF,15,ADSDATA       A(FIRST ROW)                                 
         STCM  RF,15,DSPECB                                                     
         MVC   DSPTWIDE,HDRWIDTH   WIDTH                                        
         ICM   R1,15,HDRNUM          NUMBER OF ENTRIES                          
         MH    R1,HDRWIDTH         * WIDTH OF 1 ENTRY                           
         AR    RF,R1               + A(START)                                   
         BCTR  RF,0                                                             
         STCM  RF,15,DSPTEND       A(END-1)                                     
         LA    RF,1(RF)                                                         
*                                                                               
         LA    RF,4095(RF)         ROUND TO NEXT HIGHEST 4K MULTIPLE            
         SRL   RF,12                                                            
         SLL   RF,12                                                            
         STCM  RF,15,ADSDATA       SET A(NEXT TABLE)                            
*                                                                               
         LA    R3,HDRTABL(R3)      NEXT TABLE ENTRY                             
*                                                                               
HDR04    LA    R2,L'DSPHDR(,R2)    NEXT HEADER                                  
         B     HDR02                                                            
*                                                                               
HDR06    SAC   0                                                                
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE DETAILS                                                 *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
         SPACE 1                                                                
TABLES   NTR1  ,                                                                
         BAS   RE,TSTSET                                                        
         BAS   RE,DDASET                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP TSTTAB ENTRIES                                               *         
***********************************************************************         
         SPACE 1                                                                
TSTSET   NTR1  ,                                                                
         STAR  CLEAR=ARZERO,ARS=ON                                              
         LAM   R2,R2,DMALET                                                     
         ICM   R2,15,=AL4(DTTST)   ADDRESS TSTTAB HEADER ENTRY                  
         SLL   R2,17                                                            
         SRL   R2,17-6                                                          
         A     R2,AHEADER                                                       
         USING DMSPACED,R2                                                      
*                                                                               
         CPYA  RF,R2                                                            
         LA    RF,*+10             GET INTO 31 BIT MODE (JUST IN CASE)          
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
*                                                                               
         XR    R4,R4               SET UP BXLE PARAMETERS                       
         ICM   R4,3,DSPTWIDE                                                    
         ICM   R5,15,DSPTEND                                                    
         ICM   R2,7,DSPTFRST+1                                                  
         USING TSTTABD,R2          PASS TSTTAB TO CLEAR A(TRCTAB)               
*                                                                               
         XC    TSTTRC,TSTTRC       FIRST LINE HOLDS SPECIAL INFORMATION         
         XC    TSTSCT,TSTSCT                                                    
         ICM   RF,15,PTSTDA        FIRST 2 BYTES ARE # TRACKS PER CI            
         STCM  RF,3,0(R2)                                                       
         ICM   RF,15,DCTST         NEXT 2 BYTES ARE # ENTRIES IN TABLE          
         STCM  RF,3,2(R2)                                                       
         B     TSTS06              REST OF LINE IS EMPTY                        
*                                                                               
TSTS02   XC    TSTTRC,TSTTRC                                                    
         ICM   RF,15,ADSDATA       NEXT FREE ADDRESS                            
         ST    RF,TSTTRC           SET BUFFER ADDRESS                           
*                                                                               
         ICM   R0,15,STRC          GET LENGTH OF TRC BUFFER                     
         BZ    TSTS04                                                           
         AR    R0,RF                                                            
         BCTR  R0,0                                                             
         STCM  R0,15,40(RF)        SET END ADDRESS                              
*                                                                               
         LA    R0,4                SET EYECATCHER                               
         MVC   0(8,RF),=C'*TRCBUFF'                                             
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ICM   RF,15,ADSDATA                                                    
         ICM   R0,15,STRC                                                       
         AR    RF,R0                                                            
         STCM  RF,15,ADSDATA       BUMP ADSDATA TO NEXT FREE                    
*                                                                               
TSTS04   XC    TSTSCT,TSTSCT       PROCESS SCRIPT TRACE TABLE                   
         ICM   R0,15,SSCT          TEST NEED SCRIPT TRACE BUFFER                
         BZ    TSTS06                                                           
*                                                                               
         STCM  RF,15,TSTSCT         SET BUFFER ADDRESS                          
         AR    R0,RF                                                            
         BCTR  R0,0                                                             
         STCM  R0,15,SCTEND-SCTTABD(RF)  SET END ADDRESS                        
*                                                                               
         LA    R0,4                SET EYECATCHER                               
         MVC   0(8,RF),=C'*SCTBUF*'                                             
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ICM   RF,15,ADSDATA                                                    
         ICM   R0,15,SSCT                                                       
         AR    RF,R0                                                            
         STCM  RF,15,ADSDATA       BUMP ADSDATA TO NEXT FREE                    
*                                                                               
TSTS06   BXLE  R2,R4,TSTS02                                                     
         DROP  R2                                                               
*                                                                               
         REAR  ARS=OFF                                                          
         LA    RF,*+6              GET OUT OF 31-BIT MODE                       
         BSM   0,RF                                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP DARE MAIL TABLE ENTRIES                                      *         
***********************************************************************         
         SPACE 1                                                                
DDASET   NTR1  ,                                                                
         STAR  CLEAR=ARZERO,ARS=ON                                              
         LAM   R2,R2,DMALET                                                     
         ICM   R2,15,=AL4(DTDARE)  ADDRESS DARETAB HEADER ENTRY                 
         SLL   R2,17                                                            
         SRL   R2,17-6                                                          
         A     R2,AHEADER                                                       
         USING DMSPACED,R2                                                      
         ICM   R2,7,DSPTFRST+1                                                  
*                                                                               
         LA    RF,*+10             GET INTO 31 BIT MODE (JUST IN CASE)          
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
*                                                                               
         USING TABDARED,R2                                                      
         LHI   RF,TBDSORT-TABDARED STORAGE HEADER LENGTH                        
         XR    RE,RE                                                            
         LA    R0,TBDDARL          LENGTH OF A SLOT                             
         DR    RE,R0               NUMBER OF ENTRIES FOR STORAGE HEADER         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RE,1(RF)            PUT SPACE FOR FRONT REQUIRED IN RE           
*                                                                               
         ICM   RF,15,DCDARE        HALVE THE NUMBER OF ENTRIES                  
         SR    RF,RE                                                            
         SRL   RF,1                                                             
         STCM  RF,15,TBDMAX        SET MAX NUMBER OF ENTRIES                    
*                                                                               
         LA    RE,TBDDARL          LENGTH OF A SINGLE ENTRY                     
         MSR   RF,RE               RF=LENGTH OF UNSORTED TABLE                  
         AR    RF,R2                                                            
         AHI   RF,TBDSORT-TABDARED PLUS HEADER                                  
         STCM  RF,15,TBDFRST       SET A(UNSORTED TABLE)                        
*                                                                               
         XC    TBDNOW,TBDNOW       RESET CURRENT COUNT                          
*                                                                               
         USING BSPARA,TBDBSP   *** SET PARAMETERS FOR BINSRCH                   
         LA    RF,TBDSORT                                                       
         STCM  RF,15,BSPSTRT       SET A(SORTED TABLE)                          
*                                                                               
         LA    RF,TBDKEYL                                                       
         STCM  RF,15,BSPLENK       KEY LENGTH                                   
         MVI   BSPKEYD,0           DISPLACEMENT TO KEY                          
         LA    RF,TBDDARL                                                       
         STCM  RF,15,BSPLENR       RECORD LENGTH                                
*                                                                               
         ICM   RE,15,DCDARE        HALVE THE NUMBER OF ENTRIES                  
         SRL   RE,1                                                             
         LA    RF,TBDDARL          LENGTH OF A SINGLE ENTRY                     
         MSR   RE,RF               RE=LENGTH OF SORTED TABLE                    
*                                                                               
         AHI   RE,-(TBDSORT-TABDARED)                                           
         SRDL  RE,32               REDUCE BY LENGTH OF HEADER INFO              
         LA    R0,TBDDARL                                                       
         DR    RE,R0               FIND HOW MANY WILL FIT INTO SPACE            
         STCM  RF,15,BSPEND        SET MAX NUMBER OF RECORDS                    
*                                                                               
         XC    BSPNOR,BSPNOR       TABLE IS EMPTY PRESENTLY                     
         STAM  R2,R2,BSPARS        SET ACCESS REGISTER                          
*                                                                               
         REAR  ARS=OFF                                                          
         LA    RF,EXITOK           GET OUT OF 31-BIT MODE                       
         BSM   0,RF                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
SETOPS   NTR1  ,                                                                
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
FREESPC  ST    RE,SAVERE           DELETE DATASPACE                             
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'DEL '                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                 NB - IGNORE CONDITON CODE                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MAKESPC  ST    RE,SAVERE           CREATE NEW DATASPACE                         
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'MAKE'                                                 
         MVC   WORK+4(12),DSPACE                                                
         ICM   RF,15,PAGES         NUMBER OF 4K PAGES                           
         STCM  RF,15,WORK+16                                                    
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GETSPC   ST    RE,SAVERE           GET ALET                                     
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   DMALET,WORK+24                                                   
         MVC   DMTOKN,WORK+28                                                   
         OC    DMALET,DMALET                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GETMAIN ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETMAIN  NTR1  ,                                                                
         ICM   R0,15,PAGES         NUMBER OF 4K PAGES                           
         SLL   R0,12                                                            
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R0,15,GETLEN                                                     
         STCM  R1,15,ADSDATA                                                    
         STCM  R1,15,AHEADER                                                    
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SETWAIT - SET TIMER AND WAIT INDEFINITELY - ONLY RETURN WHEN AN     *         
* INTERRUPT IS DETECTED                                               *         
***********************************************************************         
         SPACE 1                                                                
SETWAIT  NTR1  ,                                                                
         L     R1,AOPERECB                                                      
         WAIT  ECB=(1)                                                          
*                                                                               
SETWAITX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
         SPACE 1                                                                
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LR    R2,R1               R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),(1,SCNBLK)                          
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,=CL40'Invalid Line Format'                                    
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,=CL40'Invalid Keyword'                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,=CL40'Value not a valid number'                               
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,=CL40'Value not a valid character string'                     
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,=CL40'Length of input string too short'                       
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,=CL40'Length of input string too long'                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,=CL40'Numeric value too small'                                
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,=CL40'Numeric value too large'                                
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,=CL40'Invalid/missing value'                                  
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(15),=CL15' *** ERROR ***'                                  
         MVC   PLINE+15(40),0(R1)                                               
         BAS   RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT TEST RESULTS FOR ANALYSIS                                    *         
***********************************************************************         
         SPACE 1                                                                
RESULTS  NTR1  ,                                                                
         MVC   TITLE,SPACES                                                     
         MVC   TITLED(L'DTITLE),DTITLE    SET UP TITLE                          
         MVC   PLINE,SPACES                                                     
         MVC   PLINED(L'DTITLEU),DTITLEU  SET UP TITLE UNDERLINE                
         ZAP   LINE,MAXLINE               FORCE TITLE PRINTING                  
         BAS   RE,PRINTL                  PRINT TITLE AND UNDERLINE             
*                                                                               
         L     R2,AHEADER          TEST IS IN CORE                              
         USING DMSPACED,R2                                                      
*                                                                               
         LA    R0,4                FIRST 64 BYTES ARE ADCONS                    
RES02    GOTO1 VHEXOUT,DMCB,(R2),MYWORK,16,0                                    
         MVC   PLINED+00+0(8),MYWORK+0                                          
         MVC   PLINED+08+1(8),MYWORK+8                                          
         MVC   PLINED+16+2(8),MYWORK+16                                         
         MVC   PLINED+24+3(8),MYWORK+24                                         
         BAS   RE,PRINTL                                                        
         LA    R2,16(R2)                                                        
         BCT   R0,RES02                                                         
*                                                                               
         LA    R3,HDRTAB           NOW OUTPUT DATASPACE ENTRIES                 
         USING HDRTABD,R3                                                       
*                                                                               
RES04    CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    RES06               YES                                          
         CLC   DSPNAME,HDRNAME                                                  
         BE    *+6                                                              
         DC    H'0'                SOMETHING BADLY WRONG                        
*                                                                               
         MVC   PLINED(L'DSPNAME),DSPNAME             NAME                       
         MVC   PLINED+10(6),=CL6'Start='             START ADDRESS              
         GOTO1 VHEXOUT,DMCB,DSPECB,PLINE+17,4,0                                 
         MVC   PLINED+25(6),=CL6'Width='             WIDTH                      
         GOTO1 VHEXOUT,DMCB,DSPTWIDE,PLINE+32,2,0                               
         MVC   PLINED+37(6),=CL6'End-1='             END ADDRESS                
         GOTO1 VHEXOUT,DMCB,DSPTEND,PLINE+44,4,0                                
         MVC   PLINED+54(12),=CL12'Num Entries='     ENTRY COUNT                
         GOTO1 VHEXOUT,DMCB,HDRNUM,PLINE+67,4,0                                 
*                                                                               
         BAS   RE,PRINTL                                                        
         LA    R3,HDRTABL(R3)      NEXT TABLE ENTRY                             
         LA    R2,L'DSPHDR(,R2)    NEXT HEADER                                  
         B     RES04                                                            
*                                                                               
RES06    BAS   RE,PRINTL           SPACE LINE                                   
*                                                                               
         L     R2,AHEADER          TEST IN CORE                                 
         USING FATABSD,R2                                                       
         L     R2,TABSDUMP                                                      
         LHI   R0,256              OUTPUT DUMP TABLE                            
RES08    GOTO1 VHEXOUT,DMCB,(R2),MYWORK,16,0                                    
         MVC   PLINED+00+0(8),MYWORK+0                                          
         MVC   PLINED+08+1(8),MYWORK+8                                          
         MVC   PLINED+16+2(8),MYWORK+16                                         
         MVC   PLINED+24+3(8),MYWORK+24                                         
         MVC   PLINED+32+4(16),0(R2)                                            
         BAS   RE,PRINTL                                                        
         LA    R2,16(R2)                                                        
         BCT   R0,RES08                                                         
         B     EXITOK                                                           
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRINTI   NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         PUT   SYSPRINT,TITLE      PRINT TITLES                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINTL   NTR1  ,                                                                
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLES                                 
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK              EXIT                                         
         SPACE 2                                                                
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
         SPACE 1                                                                
PRINTX   NTR1  ,                                                                
         CLOSE SYSPRINT                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET UP AREA FOR WRITING DUMPS                                       *         
***********************************************************************         
         SPACE 1                                                                
SETDMPS  NTR1  ,                                                                
         LAM   R2,R2,DMALET                                                     
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSDUMP                                                   
         USING TORDUMPD,R2                                                      
*                                                                               
         L     R3,=V(FACIDTAB)                                                  
         USING FACITABD,R3                                                      
*                                                                               
         LHI   RF,1                FIRST SLOT IS ERROR SLOT                     
         MVC   TDSYSNA,FACISN4     SET SYSTEM NAME (????)                       
         STCM  RF,15,TDDUMPFS      IT ONLY HAS 1 DUMP - ALWAYS DUMP #1          
         STCM  RF,15,TDDUMPNO                                                   
         STCM  RF,15,TDDUMPMX                                                   
         LHI   RF,2                DUMP WRITING STARTS AT DUMP #2               
         ST    RF,CTDMPA                                                        
         ST    RF,CTDMPR                                                        
         ST    RF,CTDMPT                                                        
         AHI   R2,TDLENQ           NEXT IN DATASPACE                            
         AHI   R3,L'FACIDTAB       NEXT IN TABLE                                
*                                                                               
SDMP02   CLI   0(R3),X'FF'         END OF TABLE REACHED?                        
         BE    SDMPX               YES                                          
         MVC   TDSYSNA,FACISN4     SET SYSTEM NAME                              
*                                                                               
         TM    FACIFL,FACITST      TEST SYSTEM?                                 
         BZ    SDMP04              NO                                           
         L     RF,CTDMPT                                                        
         STCM  RF,15,TDDUMPFS      SET FIRST NUMBER                             
         STCM  RF,15,TDDUMPNO      SET DEFAULT CURRENT NUMBER TO FIRST          
         A     RF,NMDMPT                                                        
*&&US*&& CLC   =C'TST',FACISN4     US ONLY - TST GETS 3X DUMPS OF               
*&&US*&& BNE   *+12                          OTHER SYSTEMS                      
*&&US*&& A     RF,NMDMPT                                                        
*&&US*&& A     RF,NMDMPT                                                        
         BCTR  RF,0                                                             
         STCM  RF,15,TDDUMPMX                                                   
         AHI   RF,1                                                             
         ST    RF,CTDMPT           SET CURRENT HIGH WATER                       
         B     SDMP10                                                           
*                                                                               
SDMP04   TM    FACIFL,FACIREP      REP SYSTEM?                                  
         BZ    SDMP06              NO                                           
         L     RF,CTDMPR                                                        
         STCM  RF,15,TDDUMPFS      SET FIRST NUMBER                             
         STCM  RF,15,TDDUMPNO      SET DEFAULT CURRENT NUMBER TO FIRST          
         A     RF,NMDMPR                                                        
         BCTR  RF,0                                                             
         STCM  RF,15,TDDUMPMX                                                   
         AHI   RF,1                                                             
         ST    RF,CTDMPR           SET CURRENT HIGH WATER                       
         B     SDMP10                                                           
*                                                                               
SDMP06   CLC   =CL4'DARE',FACISN4                                               
         BNE   SDMP08                                                           
         LHI   RF,1                DARE DUMPS INTO FIRST SLOT ALSO              
         STCM  RF,15,TDDUMPFS                                                   
         STCM  RF,15,TDDUMPNO                                                   
         STCM  RF,15,TDDUMPMX                                                   
         B     SDMP10                                                           
*                                                                               
SDMP08   L     RF,CTDMPA                                                        
         STCM  RF,15,TDDUMPFS      SET FIRST NUMBER                             
         STCM  RF,15,TDDUMPNO      SET DEFAULT CURRENT NUMBER TO FIRST          
         A     RF,NMDMPA                                                        
         BCTR  RF,0                                                             
         STCM  RF,15,TDDUMPMX                                                   
         AHI   RF,1                                                             
         ST    RF,CTDMPA           SET CURRENT HIGH WATER                       
         B     SDMP10                                                           
*                                                                               
SDMP10   AHI   R2,TDLENQ           NEXT IN DATASPACE                            
         AHI   R3,L'FACIDTAB       NEXT IN TABLE                                
         B     SDMP02                                                           
*                                                                               
SDMPX    SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         J     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         ORG   TABSDSP+((((*-TABSDSP)/16)+1)*16)                                
         SPACE 1                                                                
CARDTAB  DC    CL8'MODE    ',F'001',F'0000010'                                  
         DC    AL1(03,CTCHR,L'MODE,0),AL4(MODE)                                 
         DC    CL8'RUN     ',F'001',F'0000010'                                  
         DC    AL1(02,CTCHR,L'RUN,0),AL4(RUN)                                   
         DC    CL8'KILL    ',F'001',F'0000004'                                  
         DC    AL1(03,CTCHR,L'KILL,0),AL4(KILL)                                 
         DC    CL8'DSPACE  ',F'001',F'0000012'                                  
         DC    AL1(05,CTCHR,L'DSPACE,0),AL4(DSPACE)                             
         DC    CL8'SYSTEM  ',F'001',F'0000004'                                  
         DC    AL1(05,CTCHR,L'SYSTEM,0),AL4(SYSTEM)                             
         DC    CL8'Y2K     ',F'001',F'0000001'                                  
         DC    AL1(02,CTCHR,L'DCY2K,0),AL4(DCY2K)                               
         DC    CL8'UTL     ',F'001',F'0050000'                                  
         DC    AL1(02,CTNUM,0,0),AL4(DCUTL)                                     
         DC    CL8'TBUFF   ',F'001',F'0005000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCTBUFF)                                   
         DC    CL8'TPRNT   ',F'001',F'0050000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCTPRNT)                                   
         DC    CL8'LOCKET  ',F'001',F'0050000'                                  
         DC    AL1(02,CTNUM,0,0),AL4(DCLOCK)                                    
         DC    CL8'JOBTAB  ',F'001',F'0005000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCJOB)                                     
         DC    CL8'IAMTAB  ',F'001',F'0005000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCIAM)                                     
         DC    CL8'TSTTAB  ',F'002',F'0000020'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCTST)                                     
         DC    CL8'DARETAB ',F'000',F'0005000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDARE)                                    
         DC    CL8'ASSIST  ',F'000',F'0008000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCASS)                                     
         DC    CL8'TSTTRKS ',F'001',F'0000020'                                  
         DC    AL1(06,CTNUM,0,0),AL4(PTSTDA)                                    
         DC    CL8'VRSNTAB ',F'001',F'0005000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCVRSN)                                    
         DC    CL8'BCTAB   ',F'001',F'0005000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCBRD)                                     
         DC    CL8'TRCBUFF ',F'000',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(STRC)                                      
         DC    CL8'SCTBUFF ',F'000',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(SSCT)                                      
         DC    CL8'TEMPEST ',F'001',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(TPST)                                      
         DC    CL8'REPDUMPS',F'001',F'0000050'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPR)                                    
         DC    CL8'ADVDUMPS',F'001',F'0000050'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPA)                                    
         DC    CL8'TSTDUMPS',F'001',F'0000100'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPT)                                    
         DC    CL8'LOCKTAB ',F'001',F'1000000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(NLOCKS)                                    
         DC    CL8'PROFILE1',F'001',F'1000000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCUSR1)                                    
         DC    CL8'PROFILE2',F'001',F'1000000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCUSR2)                                    
         DC    CL8'PROFILE3',F'001',F'1000000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCUSR3)                                    
         DC    CL8'CTIRECS ',F'001',F'0050000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCIDS)                                     
         DC    CL8'DDISP   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDDSP)                                    
         DC    CL8'DBOOK   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDBOK)                                    
         DC    CL8'DSTATION',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDSTN)                                    
         DC    CL8'DMASTER ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDMST)                                    
         DC    CL8'DNAME   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDNAM)                                    
         DC    CL8'DCODE   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDCDE)                                    
         DC    CL8'DCONTROL',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDCTL)                                    
         DC    CL8'DADJUST ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDADJ)                                    
         DC    CL8'DFORMULA',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDFRM)                                    
         DC    CL8'DFMTAB  ',F'001',F'0001000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCDFMTB)                                   
         DC    CL8'ALPHMKT ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDALF)                                    
         DC    CL8'NADUNV  ',F'001',F'0001000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCDNUN)                                    
*                                                                               
CARDTABX DC    AL1(CARDEOT)                                                     
         SPACE 2                                                                
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
TABSDSP  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
VHEXOUT  DC    V(HEXOUT)                                                        
VCARDS   DC    V(CARDS)                                                         
*                                                                               
ARZERO   DC    16F'0'                                                           
RMAXRES  EQU   256                 INCREASE IF MORE THAN 256 TABLES             
DSTABLE  EQU   C'T'                DSPTYPE=TABLE                                
FOURK    EQU   4*1024                                                           
*                                                                               
TABSTST  DC    C'T'                TEST SYSTEM                                  
TABSREP  DC    C'R'                REP SYSTEM                                   
TABSADV  DC    C'A'                ADV SYSTEM                                   
TABSBOTH DC    C'B'                BOTH ADV+REP SYSTEMS                         
*                                                                               
SPACES   DC    CL166' '                                                         
MAXLINE  DC    PL3'60'                                                          
         SPACE 2                                                                
CTITLE   DC    C'Input Card Details'                                            
CTITLEU  DC    C'------------------'                                            
DTITLE   DC    C'Test results'                                                  
DTITLEU  DC    C'------------'                                                  
         SPACE 2                                                                
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
AOPERECB DC    A(0)                                                             
ACOMM    DC    A(0)                                                             
*                                                                               
MODE     DC    CL10'INIT'                                                       
RUN      DC    CL4'DSP '                                                        
KILL     DC    CL4'N   '                                                        
TEST     DC    C'N'                                                             
DSPACE   DC    CL12' '                                                          
SYSTEM   DC    CL4' '                                                           
DCY2K    DC    CL1'N'                                                           
*                                                                               
NLOCKS   DC    F'0'                NUMBER OF LOCKS IN DSPACE LOCKTAB            
SSCT     DC    F'0'                SIZE OF SCRIPT BUFFER                        
TPST     DC    F'0'                NUMBER OF ENTRIES IN TEMPEST PAGE            
TPSTF    DC    F'0'                SIZE OF BLOCK FOR TEMPEST PAGE               
STRC     DC    F'0'                SIZE OF TRACE BUFFER                         
PTSTDA   DC    F'0'                TSTTAB, TRACKS PER CI                        
*                                                                               
NMDMPR   DC    F'2'                NUMBER OF DUMPS PER SYSTEM ADV               
NMDMPA   DC    F'2'                NUMBER OF DUMPS PER SYSTEM REP               
NMDMPT   DC    F'2'                NUMBER OF DUMPS PER SYSTEM TST               
CTDMPR   DC    F'0'                                                             
CTDMPA   DC    F'0'                                                             
CTDMPT   DC    F'0'                                                             
         EJECT                                                                  
***********************************************************************         
* DATASPACE HEADER TABLE ENTRY DETAILS                                *         
***********************************************************************         
         SPACE 1                                                                
         ORG   TABSDSP+((((*-TABSDSP)/16)+1)*16)                                
         SPACE 1                                                                
HDRTAB   DS    0XL16                                                            
*                                                                               
DNUTL    DC    CL8' '              UTL TABLE:     EYECATCHER                    
DCUTL    DC    F'00001'                           NUMBER OF ENTRIES             
DWUTL    DC    Y(TUTLLENV)                        WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAUTL    DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNTBUFF  DC    CL8' '              TBUFF TABLE:   EYECATCHER                    
DCTBUFF  DC    F'00001'                           NUMBER OF ENTRIES             
DWTBUFF  DC    Y(32+2048)                         WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DATBUFF  DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNTPRNT  DC    CL8' '              TPRNT TABLE:   EYECATCHER                    
DCTPRNT  DC    F'00001'                           NUMBER OF ENTRIES             
DWTPRNT  DC    Y(PRQDL)                           WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DATPRNT  DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNLOCK   DC    CL8' '              LOCK TABLE:    EYECATCHER                    
DCLOCK   DC    F'00001'                           NUMBER OF ENTRIES             
DWLOCK   DC    Y(UPDTABL)                         WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DALOCK   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNJOB    DC    CL8' '              JOB TABLE:     EYECATCHER                    
DCJOB    DC    F'00001'                           NUMBER OF ENTRIES             
DWJOB    DC    Y(48)                              WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAJOB    DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNIAM    DC    CL8' '              IAM TABLE:     EYECATCHER                    
DCIAM    DC    F'00001'                           NUMBER OF ENTRIES             
DWIAM    DC    Y(12)                              WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAIAM    DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNTST    DC    CL8' '              TEST TABLE:    EYECATCHER                    
DCTST    DC    F'00000'                           NUMBER OF ENTRIES             
DWTST    DC    Y(TSTLNEQ)                         WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DATST    DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNVRSN   DC    CL8' '              VERSION TABLE: EYECATCHER                    
DCVRSN   DC    F'00001'                           NUMBER OF ENTRIES             
DWVRSN   DC    Y(VRSNLEN)                         WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAVRSN   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNBRD    DC    CL8' '              BRDCAST TABLE: EYECATCHER                    
DCBRD    DC    F'00001'                           NUMBER OF ENTRIES             
DWBRD    DC    Y(BCTABL)                          WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DABRD    DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDARE   DC    CL8' '              DARE NOTIFY  : EYECATCHER                    
DCDARE   DC    F'00100'                           NUMBER OF ENTRIES             
DWDARE   DC    Y(8)                               WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADARE   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNASS    DC    CL8' '              DARE NOTIFY  : EYECATCHER                    
DCASS    DC    F'00010'                           NUMBER OF ENTRIES             
DWASS    DC    Y(ASSISTLQ)                        WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAASS    DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNUSR1   DC    CL8' '              PROFILES 1   : EYECATCHER                    
DCUSR1   DC    F'10'                              NUMBER OF ENTRIES             
DWUSR1   DC    Y(1024)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAUSR1   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNUSR2   DC    CL8' '              PROFILES 2   : EYECATCHER                    
DCUSR2   DC    F'10'                              NUMBER OF ENTRIES             
DWUSR2   DC    Y(1024)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAUSR2   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDDSP   DC    CL8' '              DDISP        : EYECATCHER                    
DCDDSP   DC    F'02'                              NUMBER OF ENTRIES             
DWDDSP   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADDSP   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDBOK   DC    CL8' '              DBOOK        : EYECATCHER                    
DCDBOK   DC    F'02'                              NUMBER OF ENTRIES             
DWDBOK   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADBOK   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDSTN   DC    CL8' '              DSTATION     : EYECATCHER                    
DCDSTN   DC    F'02'                              NUMBER OF ENTRIES             
DWDSTN   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADSTN   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDMST   DC    CL8' '              DMASTER      : EYECATCHER                    
DCDMST   DC    F'02'                              NUMBER OF ENTRIES             
DWDMST   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADMST   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDNAM   DC    CL8' '              DNAME        : EYECATCHER                    
DCDNAM   DC    F'02'                              NUMBER OF ENTRIES             
DWDNAM   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADNAM   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDCDE   DC    CL8' '              DCODE        : EYECATCHER                    
DCDCDE   DC    F'02'                              NUMBER OF ENTRIES             
DWDCDE   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADCDE   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDCTL   DC    CL8' '              DCONTROL     : EYECATCHER                    
DCDCTL   DC    F'02'                              NUMBER OF ENTRIES             
DWDCTL   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADCTL   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDADJ   DC    CL8' '              DADJUST      : EYECATCHER                    
DCDADJ   DC    F'02'                              NUMBER OF ENTRIES             
DWDADJ   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADADJ   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDFRM   DC    CL8' '              DFORMULA     : EYECATCHER                    
DCDFRM   DC    F'02'                              NUMBER OF ENTRIES             
DWDFRM   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADFRM   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDFMTB  DC    CL8' '              DFMTAB       : EYECATCHER                    
DCDFMTB  DC    F'02'                              NUMBER OF ENTRIES             
DWDFMTB  DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADFMTB  DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDALF   DC    CL8' '              ALPHMKT      : EYECATCHER                    
DCDALF   DC    F'02'                              NUMBER OF ENTRIES             
DWDALF   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADALF   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNDNUN   DC    CL8' '              NAD UNIVERSE : EYECATCHER                    
DCDNUN   DC    F'02'                              NUMBER OF ENTRIES             
DWDNUN   DC    Y(1000)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DADNUN   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNUSR3   DC    CL8' '              PROFILES 3   : EYECATCHER                    
DCUSR3   DC    F'10'                              NUMBER OF ENTRIES             
DWUSR3   DC    Y(1024)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAUSR3   DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DNIDS    DC    CL8' '              CTIRECS      : EYECATCHER                    
DCIDS    DC    F'10'                              NUMBER OF ENTRIES             
DWIDS    DC    Y(1024)                            WIDTH                         
         DC    AL1(DSTABLE)                       TYPE (=TABLE)                 
         DC    X'00'                              SPARE                         
DAIDS    DC    A(0)                               A(IN DATASPACE)               
*                                                                               
DEND     DC    X'FFFFFFFF'         END OF TABLE                                 
*                                                                               
HDRTABD  DSECT                                                                  
HDRNAME  DS    CL8                                                              
HDRNUM   DS    F                                                                
HDRWIDTH DS    H                                                                
HDRTYPE  DS    X                                                                
HDRSPR   DS    X                                                                
HDRADR   DS    A                                                                
HDRTABL  EQU   *-HDRTABD                                                        
*                                                                               
TABSDSP  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE DC                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
BUFFER   DS    60000C                                                           
         DS    0D                                                               
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
NDMPS    DS    F                   NUMBER OF DUMPS PER SYSTEM                   
RDMPS    DS    F                   REMAINDER                                    
*                                                                               
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
GETLEN   DS    F                   LENGTH RETURNED IF GETMAIN                   
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
WAITER   DS    A                                                                
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
PLINE    DS    0CL166                                                           
         DS    XL1                                                              
PLINED   DS    CL165                                                            
TITLE    DS    0CL166                                                           
         DS    XL1                                                              
TITLED   DS    CL165                                                            
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
         TITLE 'VARIABLE SCAN MODULE'                                           
SCANNER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SWORKX-SWORKD,**SCAN**                                           
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         SR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN1                                                            
*                                                                               
SCAN1    SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
         SPACE 2                                                                
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
         SPACE 2                                                                
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*HANDLE LINES OF DATA                                                           
*                                                                               
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BAS   RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
         SPACE 2                                                                
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
         SPACE 2                                                                
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
         SPACE 2                                                                
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
         SPACE 2                                                                
VARPAK   PACK  SDUB,0(0,R2)                                                     
         SPACE 2                                                                
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
         SPACE 2                                                                
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
         SPACE 2                                                                
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
         SPACE 2                                                                
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
         SPACE 2                                                                
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
         SPACE 2                                                                
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
         SPACE 2                                                                
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
         SPACE 2                                                                
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
         SPACE 2                                                                
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
         SPACE 2                                                                
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
         SPACE 2                                                                
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
         SPACE 2                                                                
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
         SPACE 2                                                                
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
*                                                                               
       ++INCLUDE DDLOCKTLK                                                      
SWORKX   DS    0C                                                               
         SPACE 1                                                                
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 2                                                                
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* FATABSDMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDMP                                                      
         PRINT ON                                                               
* FATABSTMS                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSTMS                                                      
         PRINT ON                                                               
* FATABSPQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE FATABSPQ                                                       
         PRINT ON                                                               
* FATABSZIP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSZIP                                                      
         PRINT ON                                                               
* FATABSRUN                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSRUN                                                      
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FASCTTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASCTTAB                                                       
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FAPRQ                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAPRQ                                                          
         PRINT ON                                                               
* FAUPDTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAUPDTAB                                                       
         PRINT ON                                                               
* DDASSISTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDASSISTD                                                      
         PRINT ON                                                               
* FADARETABD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDAR                                                      
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* FATAB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATAB                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061FATABSDSPS05/01/02'                                      
         END                                                                    
