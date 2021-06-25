*          DATA SET SPLNK17    AT LEVEL 036 AS OF 10/28/20                      
*PROCESS USING(WARN(15))           ENABLE ALL USING STATEMENT WARNINGS          
*PHASE T21E17B                                                                  
*INCLUDE GETBROAD                                                               
SPLNK17  TITLE '- SPOT DEMO DOWNLOADS FOR DESKTOP'                              
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,WORKERKEY=SDLK,        *        
               SYSTEM=SPTSYSQ,APPEND=Y,SERVERTYPE=TSTDEMD,             *        
               AUTOCLEAR=Y,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),       *        
               FILES=FILES                                                      
                                                                                
CODE     NMOD1 0,**SL17**,RR=RE                                                 
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK PARAMETER BLOCK)                 
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         L     RF,LP_ARUNP                                                      
         MVC   RUNMODE,RUNPMODE-RUNPARMD(RF)                                    
                                                                                
         USING TSARD,TSARBLK                                                    
         USING BUFFD,FLMBUF                                                     
         USING GDEMBLKD,GDEMBLK                                                 
         MVC   VERSNUM,LP_VRSN1                                                 
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         NI    LP_AIND1,FF-(LP_AICOM)                                           
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* RUN DOWNLOAD REQUEST                                                *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
         GOTOR LP_APUTO            CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* BOOK LIST HEADER                                                              
***********************************************************************         
                                                                                
NXTBLH   XR    R4,R4                                                            
         L     R4,FILIND                                                        
         USING LW_D,R4                                                          
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JE    NXTBLH10            YES                                          
*                                                                               
         OC    FILEID,FILEID       IF FILE FILTER IS ON                         
         JZ    NXTBLH15            NO, IT IS NOT                                
         XC    ABLTNTRY,ABLTNTRY   ALWAYS START SCAN AT FIRST ENTRY             
*                                                                               
         LLC   R1,FILIND           NO                                           
         LA    R1,1(R1)                                                         
         CLM   R1,3,LW_NUMN        TEST ALL FILES PROCESSED                     
         JNL   NOMORE              NONE                                         
         STC   R1,FILIND           IF FILE FILTER ON,                           
         MHI   R1,L'FILEID                                                      
         LA    R1,LW_DATA2(R1)                                                  
         MVC   FILEID(L'FILEID),0(R1)                                           
         J     NXTBLH15                                                         
*                                                                               
NXTBLH10 XC    FILEID,FILEID                                                    
         XC    ABLTNTRY,ABLTNTRY                                                
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,LW_NUMN        CAN'T POSSIBLY HANDLE MORE THAN 255          
         CHI   R1,255                                                           
         JH    *+2                                                              
         LA    R1,LW_DATA2                                                      
         MVC   FILEID(L'FILEID),0(R1)                                           
         MVI   FILIND,0                                                         
         DROP  R4                                                               
*                                                                               
NXTBLH15 ICM   RF,15,ABLTNTRY      POINT TO FIRST/NEXT BLTTAB ENTRY             
         JZ    *+12                                                             
NXTBLH20 AHI   RF,BLTTABL                                                       
         J     *+8                                                              
         LA    RF,BLTTAB                                                        
         CLI   0(RF),EOT           TEST END OF TABLE                            
         JE    NOMORE                                                           
         MVC   BOOKFILE,BLTTFILE-BLTTABD(RF)                                    
         LA    R0,BOOKFILE                                                      
         ST    R0,LP_ADATA                                                      
         STCM  RF,15,ABLTNTRY                                                   
                                                                                
         OC    FILEID,FILEID       FILTER ON FILE ID PROVIDED                   
         JZ    NXTBLH30                                                         
         CLC   FILEID,BOOKFILE                                                  
         JNE   NXTBLH20                                                         
                                                                                
NXTBLH30 OC    MARKET,MARKET       LIST BY MARKET                               
         JZ    NXTBLHX             REQUEST BOOKS ONLY FOR SOME FILES            
         TM    BLTTINDS-BLTTABD(RF),$MKTBLST                                    
         JNO   NXTBLH20                                                         
                                                                                
NXTBLHX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* BOOK LIST DETAIL                                                    *         
***********************************************************************         
                                                                                
         USING NBWORKD,RC                                                       
NXTBLD   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JE    NXTBLD20                                                         
*                                                                               
         GOTOR BUFFER,DMCB,('TSANXT',0)                                         
NXTBLD10 TM    TSERRS,TSEEOF       TEST ALL RECORDS READ                        
         JNZ   NOMORE                                                           
                                                                                
         LA    R0,BOOKREC                                                       
         ST    R0,LP_ADATA         RETURN BOOK RECORD                           
         J     EXITY                                                            
*                                                                               
* BUILD DBLOCK AND CALL DEMAND TO STORE BOOKS IN TSAR                           
*                                                                               
NXTBLD20 DS    0H                                                               
         LA    R0,DBLOCK           INITIALIZE DBLOCK                            
         LHI   R1,DBLOCKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   GTADFSBK,GAFBINIT   INIT TO 0                                    
                                                                                
         MVI   LOCFLAG,LOC_FIRST_TIME                                           
         MVC   DBFILE,FILTP                                                     
         MVC   DBSELAGY,LP_AGY                                                  
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
                                                                                
         XC    ALOCNTRY,ALOCNTRY                                                
         XC    NBSPILL,NBSPILL                                                  
         OC    MARKET,MARKET       BOOK LIST BY MARKET?                         
**       JZ    *+14                                                             
**       MVC   DBSELALF,MARKET     YES                                          
**       J     NXTBLD30            NO STATION INPUT                             
* ALWAYS SET SPILL IF ANY AND PROCEED TO PROCESS STATION                        
* IF MARKET IS NOT PASSED IN VIA MARKET MAPCODE CHECK TO SEE IF                 
* THE MARKET IS ATTACHED TO THE STATION STRING                                  
         MVC   DBSELALF,MARKET                                                  
                                                                                
         MVC   NBCARD,SPACES       SCAN FOR STATION/SPILL                       
         MVC   NBCARD(STAL),STATN                                               
         GOTOR VSCANNER,DMCB,(C'C',NBCARD),NBSCAN,C',=,/'                       
         USING SCANBLKD,NBSCAN                                                  
                                                                                
         MVC   DBSELSTA,SC1STFLD   ALPHA STATION SPACE PADDED                   
         CLI   DBSELSTA+4,C' '                                                  
         JNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
                                                                                
* RADIO COUNTY COVERAGE DONT HAVE MARKETS                                       
         L     RF,ABLTNTRY                                                      
         CLI   BLTTMEDC-BLTTABD(RF),C'U'                                        
         JNE   NXTBLD28                                                         
         MVC   DBSELMED,BLTTMEDC-BLTTABD(RF)                                    
         MVC   DBSELSRC,BLTTSRCC-BLTTABD(RF)                                    
         MVC   DBFILE,FILRTP                                                    
         J     NXTBLD50                                                         
                                                                                
NXTBLD28 OC    MARKET,MARKET       IF MARKET IS ALREADY FILLED IN VIA           
         JNZ   NXTBLD30            MARKET MAPCODE                               
         XC    SVMKALF,SVMKALF                                                  
         CLI   SC2NDLEN,0          TRANSLATE ALPHA TO NUMRIC SPILL              
         JE    NXTBLD40            NO SPILL                                     
         MVC   DBSELALF,SC2NDFLD                                                
*                                                                               
NXTBLD30 MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   SVMKALF,DBSELALF                                                 
                                                                                
         L     RF,ABLTNTRY                                                      
         MVC   DBSELMED,BLTTMEDC-BLTTABD(RF)                                    
         MVC   DBSELSRC,BLTTSRCC-BLTTABD(RF)                                    
         CLI   DBSELSRC,C'F'       USE NSI TRANSLATION                          
         JNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
***      CLI   DBSELMED,C'W'       WTP FILE USE NSI TV TRANSLATION              
***      JE    *+8                                                              
         CLI   DBSELMED,C'O'       OTP FILE USE NSI TV TRANSLATION              
         JE    *+8                                                              
         CLI   DBSELMED,C'D'       DPT FILE USE NSI TV TRANSLATION              
         JNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
                                                                                
         GOTOR VDEMAND,DMCB,DBLOCK,0,0                                          
                                                                                
         L     RF,ABLTNTRY                                                      
         MVC   DBSELSRC,BLTTSRCC-BLTTABD(RF)                                    
         MVC   DBSELMED,BLTTMEDC-BLTTABD(RF)                                    
                                                                                
         CLI   DBERROR,0           HAVE ERROR?                                  
         JNE   *+16                 YES                                         
         MVC   DBSELMK,DBSELRMK    SET NUMERIC IF NO ERRORS                     
         MVC   NBSPILL,DBSELRMK                                                 
         OC    MARKET,MARKET       HAVE MARKET?                                 
         JNZ   *+10                 YES                                         
         XC    DBSELRMK,DBSELRMK   NEED DBSELRMK FOR LIST BY MARKET             
         MVI   DBERROR,0                                                        
         J     NXTBLD50                                                         
*                                                                               
*  IF NO ALPHA MARKET PASSED CHECK TO SEE IF LOCAL CABLE                        
* IF LOCAL CABLE SWITCH CALL LETTERS AND SET SPILL MKT                          
NXTBLD40 DS    0C                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,LOCALCAB                                               
         ICM   RE,15,0(R1)                                                      
         JZ    *+2                 THIS SHOULD NEVER HAPPEN                     
         L     R0,4(R1)            L'TABLE ENTRY                                
         USING LOCCABD,RE                                                       
         OC    ALOCNTRY,ALOCNTRY   SEE IF ANYMORE LINKS FOR SAME                
         JZ    NXTBLD43            CALL LETTERS                                 
         L     RE,ALOCNTRY                                                      
         J     NXTBLD46                                                         
*                                                                               
NXTBLD43 CLI   0(RE),X'FF'         END OF LOCALCAB TABLE                        
         JE    NXTBLD50                                                         
         CLC   LOCSPOTC,DBSELSTA   MATCH ON SPOT CALL LETTERS?                  
         JE    NXTBLD46                                                         
         AR    RE,R0                                                            
         J     NXTBLD43                                                         
NXTBLD46 MVC   DBSELSTA(5),LOCNSIC      LINK STATION TO READ                    
         MVC   DBSELMK,LOCSMKT                                                  
         MVC   NBSPILL,LOCSMKT                                                  
         MVC   DBSELALF,LOCALPHA         SPILL MKT TO READ                      
         CLI   LOCLEVEL,LOC_NOT_LAST_LEVEL                                      
         JNE   *+12                                                             
         LR    RF,RE                                                            
         AR    RF,R0                                                            
         ST    RF,ALOCNTRY                                                      
         CLI   LOCLEVEL,LOC_LAST_LEVEL                                          
         JNE   *+10                                                             
         XC    ALOCNTRY,ALOCNTRY                                                
         DROP  RE                                                               
*                                                                               
NXTBLD50 MVI   DBFUNCT,DBGETMB     GET ALL BOOKS FOR A STATION/SPILL            
         MVI   DBBEST,C'A'         WANT BOOKS FOR ALL EQUIV STATS TOO           
*                                                                               
*   FOR RADIO COUNTY COVERAGE - ALWAYS LOOK UP BOOK LIST                        
*   FOR ONE COUNTY NUMBER SINCE THE BOOK LIST IS THE SAME FOR ALL               
*   COUNTIES                                                                    
         CLI   DBSELMED,C'U'                                                    
         JNE   *+14                                                             
         MVC   DBSELRMK,=X'000A'                                                
         MVI   DBFUNCT,DBGETASB    GET ALL BOOKS/STATIONS FOR A COUNTY          
*                                                                               
         CLC   DBSELSTA(4),SPACES  DONT NEED TO CHECK 5 BYTES                   
         JH    NXTBLD54            FIFTH BYTE IS ALWAYS SET TO C'T'             
         OC    MARKET,MARKET                                                    
         JZ    NXTBLD54                                                         
         MVI   DBFUNCT,DBGETASB    GET ALL BOOKS/STATIONS FOR A MARKET          
         CLI   DBSELMED,C'R'                                                    
         JNE   NXTBLD54                                                         
         MVI   DBFUNCT,DBGETAMB                                                 
NXTBLD54 L     RF,ABLTNTRY                                                      
         MVC   DBSELMED,BLTTMEDC-BLTTABD(RF)                                    
         MVC   DBSELSRC,BLTTSRCC-BLTTABD(RF)                                    
* DONT REINIT TSAR BUFFER FOR MULTIPLE ENTRIES OF THE SAME                      
* LOCAL CABLE CALL LETTERS                                                      
         CLI   LOCFLAG,LOC_NOT_FIRST_TIME                                       
         JE    NXTBLD58                                                         
         GOTOR BUFFER,DMCB,('TSAINI',0),('BOOKKEYL',BOOKRECL)                   
NXTBLD58 L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,LIVEBKTY       PARAM1=A(LIVE BOOK TYPE TABLE)          
         ICM   RE,15,0(R1)                                                      
         JNZ   *+6                                                              
         DC    H'0'                THIS SHOULD NEVER HAPPEN                     
         ST    RE,ALVBTTAB         STORE TABLE ADDRESS                          
         MVC   LVBTTABL,6(R1)      LENGTH TABLE ENTRY RETURNED TO US            
*                                                                               
* READY FOR LOOKUP                                                              
*                                                                               
NXTBLD60 L     RE,=A(DHOOK)        CALL DEMAND FOR BOOK LIST                    
         A     RE,SRVRRELO                                                      
         ST    RE,ADHOOK                                                        
*                                                                               
***********************************************************************         
* SBTK INCORRECTLY REQ'S TT (NSI) WHEN TF (FUSION) IS CORRECT FILETYPE          
* MODIFY FILETYPE FROM TT TO TF AND RETRIEVE FUSION BOOKS INSTEAD               
*                                           MOSBTK-429  -HWON 3/21/2016         
***********************************************************************         
*                                                                               
         CLI   GTADFSBK,0          ALREADY SET GTADFSBK?                        
         JNE   *+8                  YES                                         
         BRAS  RE,TGADFSBK         TEST GET FUSION BOOKS                        
*                                                                               
         MVC   DBLOCK2,DBLOCK      SAVE DBLOCK TO DBLOCK2                       
*                                                                               
         CLI   GTADFSBK,GAFBFUS    GET FUSION BOOKS?                            
         JNE   NXTBLD65            NO                                           
*                                                                               
***********************************************************************         
* IF RETRIEVING TT OR TF FILETYPE & STANDARD OR LS BOOKTYPE                     
* THEN GET ADDITIONAL TT (NSI) BOOKS VIA ANOTHER DEMAND CALL                    
*          ----------         SPEC-37508 & SPEC-37012  -HWON  7/31/2019         
***********************************************************************         
         CLI   BTYPE,0             HAVE BOOKTYPE?                               
         JE    *+12                                                             
         CLI   BTYPE,BOOKTYPE_LS   OR ASKING FOR LS BOOKS?                      
         JNE   NXTBLD65            NO, ONLY GET FUSION BOOKS                    
*                                  YES - GET ADD'L LS NSI BOOKS                 
         MVI   DBSELMED,MEDTP      FORCE USTV                                   
         MVI   DBSELSRC,SRCNSI     FORCE ADD'L NSI BOOK LOOKUP                  
*                                                                               
         GOTOR VDEMAND,DMCB,DBLOCK,ADHOOK                                       
*                                                                               
         MVC   DBLOCK,DBLOCK2      RESTORE DBLOCK                               
         MVI   DBSELSRC,SRCFUS     FUSION BOOK LOOKUP                           
*                                                                               
NXTBLD65 GOTOR VDEMAND,DMCB,DBLOCK,ADHOOK                                       
         MVC   DBLOCK,DBLOCK2      RESTORE DBLOCK                               
*                                                                               
         OC    ALOCNTRY,ALOCNTRY   SEE IF ANYMORE LINKS FOR SAME                
         JZ    *+12                CALL LETTERS                                 
         MVI   LOCFLAG,LOC_NOT_FIRST_TIME                                       
         J     NXTBLD40            GO BACK AND GET MORE BOOKS                   
*                                                                               
         CLI   DBERROR,IOEEOF      TEST DEMAND POSTED EOF                       
         JE    *+12                                                             
         CLI   DBERROR,0           EXIT ON OTHER ERRORS                         
         JNE   NOMORE                                                           
*                                                                               
         XC    BOOKREC(BOOKKEYL),BOOKREC    GET FIRST BOOK                      
         GOTOR BUFFER,DMCB,('TSARDH',0)                                         
         J     NXTBLD10                                                         
*                                                                               
***********************************************************************         
* TEST NEED TO GET FUSION BOOKS                                                 
*  NOTE: SBTK INCORRECTLY REQUESTS TT (NSI) FILETYPE WHEN IT SHOULD             
*        REQUEST FOR TF (FUSION) FILETYPE FOR A CABLE STATION LOOKUP            
*                                                                               
* TEST THE FOLLOWING FIELDS TO DETERMINE CORRECT FILETYPE                       
* - IF REQUEST BY STATION                                                       
* - IF FILETYPE IS TT OR TF                                                     
* - IF SBTK PASSED AN ALPHA MARKET                                              
* - IF STATION IS CABLE  (CALL DEFINE AND CONFIRM NO AFFILIATION)               
* - IF MARKET IS NOT LPM (CALL DEMTABS AND CROSS CHECK FUSNENDP TABLE)          
* - EITHER                                                                      
* -  - IF MARKET RECORD IS SET TO FUSION                                        
* -  - IF 00A (CABLE DEMO LOOKUP) IS SET TO FUSION                              
*                                       MOSBTK-429    -HWON 3/21/2016           
*  ON EXIT: GTADFSBK  - X'00' - INIT                                            
*                     - C'F'  - NEEDS ADDITIONAL FUSION BOOKS                   
*                     - C'N'  - ONLY READ NSI BOOKS                             
***********************************************************************         
*                                                                               
TGADFSBK NTR1  BASE=*,LABEL=*                                                   
         OC    STATN,STATN         STATION REQUESTED?                           
         JZ    GAFBX                NO                                          
*                                                                               
         ICM   RF,15,ABLTNTRY      POINT TO BLTTAB ENTRY                        
         CLC   =C'TT ',BLTTFILE-BLTTABD(RF)                                     
         JE    GAFB005                                                          
         CLC   =C'TF ',BLTTFILE-BLTTABD(RF)                                     
         JNE   GAFBX                                                            
*                                                                               
GAFB005  OC    SVMKALF,SVMKALF     HAVE ALPHA?                                  
         JZ    GAFBX                                                            
*                                                                               
         BRAS  RE,GETAFFL                                                       
         CLC   AFFILS,=CL5'N/A  '     HAVE AFFIL?                               
         JNE   GAFBX                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),MYDMCB,FUSNENDP                                             
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JZ    *+2                 BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
         USING FUSENDPD,RE                                                      
GAFB010  DS    0H                                                               
         CLC   =X'FFFF',0(RE)      EOF - NOT LPM                                
         JE    GAFB020                                                          
         CLC   FUSAMKT,SVMKALF     LPM?                                         
         JE    GAFBX               YES, THEN TT FILETYPE                        
         AR    RE,R0                                                            
         J     GAFB010                                                          
* IF WE GET HERE, THEN WE KNOW MARKET IS NOT LPM                                
* NEED TO READ THE MARKET RECORD TO CHECK CABLE DEMO FLAG                       
GAFB020  DS    0H                                                               
         BRAS  RE,GETMKT                                                        
         CLI   GTADFSBK,GAFBINIT   DID MARKET SET A FILETYPE?                   
         JNE   GAFBX                YES                                         
* IF WE GET HERE, MARKET RECORD CABLE DEMO FIELD WAS NULL                       
* NEED TO CHECK THE 00A PROFILE, 'CABLE DEMO LOOKUP' VALUE                      
         XC    WORK,WORK                                                        
         MVC   WORK+0(4),=C's00A'                                               
         MVC   WORK+4(2),LP_AGY                                                 
         MVC   WORK+6(1),DBSELMED                                               
         GOTOR VGETPROF,MYDMCB,WORK,WORK+16,VDATAMGR                            
         MVC   GTADFSBK,WORK+16                                                 
GAFBX    CLI   GTADFSBK,GAFBINIT   ANY FLAG SET?                                
         JNE   EXIT                                                             
         MVI   GTADFSBK,GAFBNSI    NO, SET TO DEFAULT NSI BOOK LOOKUP           
         J     EXIT                                                             
*                                                                               
***********************************************************************         
* CALL DEFINE TO GET THE STATION AFFILIATION CODE                               
*                                                                               
* EXIT  : AFFILS    =FOR BROADCAST, THE AFFILIATION CODE                        
*                   =FOR CABLE, CL5'N/A'                                        
***********************************************************************         
GETAFFL  NTR1  LABEL=*                                                          
*                                                                               
         MVC   AFFILS,=CL5'N/A  '           FORCE AFFIL = "N/A"                 
                                                                                
         LA    R2,DBLOCK2                                                       
ADB      USING DBLOCK,R2                    R2=A(DBLOCK)                        
         XC    DBLOCK2,DBLOCK2              SET FIXED VALUES IN DBLOCK          
         MVC   ADB.DBFILE,=C'TP '           FILE,                               
         MVC   ADB.DBAREC,AIO2                                                  
         MVI   ADB.DBFUNCT,DBGETDEM         FUNCTION,                           
         MVC   ADB.DBCOMFCS,ACOMFACS                                            
         MVI   ADB.DBSELSRC,C'N'            SOURCE,                             
         XC    ADB.DBSELBK,ADB.DBSELBK      LATEST BOOK                         
         MVC   ADB.DBSELSTA,DBSELSTA                                            
         MVI   ADB.DBSELMED,C'T'            MEDIA                               
         MVC   ADB.DBSELAGY,=C'SJ'          AGENCY CODE,                        
         MVI   ADB.DBSELDAY,X'40'           DAY,                                
         MVC   ADB.DBSELTIM,=AL2(0800,0815) AND TIMES,                          
         GOTOR VDEMAND,DMCB,DBLOCK2,0                                           
         CLI   ADB.DBERROR,10               NOFOUND?                            
         JE    GETAFFLX                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEFINE-COMFACSD(RF)                                       
         GOTOR (RF),MYDMCB,=C'AFFL',DBLOCK2,WORK                                
         MVC   AFFILS,WORK                                                      
GETAFFLX J     EXITCC                                                           
         DROP  ADB                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
* READ MARKET RECORD AND GET CABLE DEMO FILETYPE                                
*                                                                               
* EXIT  : GTADFSBK    COPY OF MKTCDEM - CABLE DEMOS 0,F OR N                    
**********************************************************************          
                                                                                
         USING GTMKWORK,R3                                                      
GETMKT   NTR1  LABEL=*,WORK=(R3,GTMKWRKL)                                       
                                                                                
         LA    RF,GTMKKEY                                                       
         USING ANMRECD,RF                    READ A-N MARKET PASSIVE            
         XC    ANMKEYD,ANMKEYD                                                  
         MVI   ANMKTYPE,ANMKTYPQ             C'L'-PASSIVE                       
         MVC   ANMKAGCY,LP_AGY                                                  
         MVC   ANMKMED,DBSELMED                                                 
         MVC   ANMKAMRK,SVMKALF                                                 
         GOTOR VDATAMGR,MYDMCB,(0,=CL8'DMRDHI'),=C'STATION',GTMKKEY,   +        
               GTMKAIO                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         LA    RF,GTMKAIO                                                       
         CLC   ANMKNMRK,=CL7'0000000'                                           
         JNH   *+2                           MUST HAVE A MARKET!                
                                                                                
         LA    R1,GTMKKEY                                                       
         USING MKTREC,R1           READ MARKET RECORD                           
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,DBSELMED                                                 
         MVC   MKTKMKT,ANMKNMRK                                                 
         MVC   MKTKAGY,LP_AGY                                                   
         MVC   MKTKFILL,=CL7'0000000'                                           
         GOTOR VDATAMGR,MYDMCB,(0,=CL8'DMREAD'),=C'STATION',GTMKKEY,   +        
               GTMKAIO                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         LA    R1,GTMKAIO                                                       
         MVC   GTADFSBK,MKTCDEM    COPY THE CABLE DEMO FILETYPE                 
         J     EXITY                                                            
         DROP  R1,R3,RF                                                         
GTMKWORK DSECT                                                                  
GTMKKEY  DS    CL(L'MKTKEY)                                                     
GTMKAIO  DS    XL(L'MKTREC)                                                     
GTMKWRKL EQU   *-GTMKWORK                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* HOOK TO DEMAND                                                                
**********************************************************************          
                                                                                
DHOOK    CLC   DBSELSTA(4),SPACES                                               
         JE    *+14                                                             
         OC    DBSELSTA,DBSELSTA   LOOKUP BY STATION?                           
         JNZ   *+14                                                             
         OC    MARKET,MARKET       LOOKUP BY MARKET?                            
         JNZ   DHOOK40             DEMAND RETURNS DIFFERENT KEYS                
*                                                                               
         CLI   DBSELMED,C'U'       COUNTY COVERAGE LOOKED UP BY MKT             
         JE    DHOOK40                                                          
         MVI   BOOKTYPE+1,0                                                     
                                                                                
         L     R2,DBAREC           IF LOOKUP BY STATION USE SBKEY               
         USING SBKEY,R2            R2=A(STATION/BOOK RECORD)                    
                                                                                
DHOOK20  TM    SBBOOK,X'80'        IGNORE INVALID BOOKS                         
         JNZ   DHOOKX                                                           
                                                                                
                                                                                
         CLI   DBSELMED,MEDRAD     SAME ALPHA MKT MATCH?                        
         JNE   DHOOK26                                                          
         CLC   DBSELALF,SVMKALF                                                 
         JE    DHOOK28                                                          
                                                                                
*HOOK26  OC    SVMKALF,SVMKALF     IF NO SPILL MKT WAS ASKED FOR                
*        JNZ   *+14                AND WE GET A KEY THAT HAS SPILL              
*        OC    NBSPILL,NBSPILL     MKT ITS PROBABLY FINE - DUE TO               
*        JZ    DHOOK28             LOCAL CABLE LINK SWITCH IN DEGET             
*                                  DEGET DOES THE MKT FILTERING                 
*                                                                               
DHOOK26  CLC   NBSPILL,SBKMKT      MAKE SURE CORRECT SPILL MKT(OR NONE)         
         JNE   DHOOKX                                                           
                                                                                
DHOOK28  MVC   CURRSTA,SBSTAT                                                   
         CLI   DBSELSRC,C'F'                                                    
         JNE   *+10                                                             
         MVC   CURRSTA,DBACTSTA                                                 
         MVC   BOOKWKMO,SBBOOK                                                  
         MVC   BOOKTYPE(L'SBBTYP),SBBTYP                                        
         L     RF,ABLTNTRY                                                      
         TM    BLTTINDS-BLTTABD(RF),$VALDWKY                                    
         JZ    DHOOK30                                                          
         CLI   DBSELMED,MEDON                                                   
         JNE   DHOOK60                                                          
         CLC   SBBOOK,=AL1(104,50)                                              
         JL    DHOOKX                                                           
         J     DHOOK60                                                          
                                                                                
DHOOK30  XC    BOOKWKMO,EFFS                                                    
         CLC   SBBOOK,=AL1(90,07)                                               
         JNL   *+8                                                              
         MVI   BOOKTYPE,0                                                       
         CLI   BOOKTYPE,X'E0'                                                   
         JNE   *+8                                                              
         MVI   BOOKTYPE,0                                                       
         CLI   BOOKTYPE,5                                                       
         JNE   DHOOK60                                                          
         MVI   BOOKTYPE,0                                                       
         J     DHOOK60                                                          
         DROP  R2                                                               
                                                                                
DHOOK40  LA    R2,DBKEY            LOOKUP BY MARKET USES MLKEY                  
         USING MLKEY,R2                                                         
                                                                                
         CLI   DBSELMED,C'R'                                                    
         JNE   *+14                                                             
         CLC   DBSELALF,SVMKALF                                                 
         JNE   DHOOKX                                                           
                                                                                
         MVC   CURRSTA,MLSTAT                                                   
         MVI   CURRSTA+4,C'T'                                                   
         MVI   BOOKTYPE+1,0                                                     
         MVC   BOOKWKMO,MLBOOK                                                  
         XC    BOOKWKMO,EFFS       INVERSE BOOK FOR COMPARISON                  
         MVC   BOOKTYPE(L'MLBTYP),MLBTYP                                        
                                                                                
         CLI   DBSELMED,C'U'                                                    
         JE    DHOOK42                                                          
         CLI   BOOKTYPE,X'E0'      IGNORE EXTENDED SPILL BOOKS                  
         JE    DHOOKX                                                           
         CLI   BOOKTYPE,C'F'       IGNORE BOOKTYPES 'F' AND 'Y'                 
         JE    DHOOKX              (DEMONSTRATION DATA)                         
         CLI   BOOKTYPE,C'Y'                                                    
         JE    DHOOKX                                                           
                                                                                
                                                                                
DHOOK42  CLI   DBSELMED,MEDON      ONLY SHOW OVERNIGHTS STARTING                
         JNE   *+14                DEC0604                                      
         CLC   BOOKWKMO,OVCUTOFF                                                
         JL    DHOOKX                                                           
                                                                                
         CLC   BOOKWKMO,BTCUTOFF   IF BOOK IS PRIOR TO BKTYP CUT-OFF,           
         JNL   *+8                                                              
         MVI   BOOKTYPE,0          KILL THE BOOKTYPE                            
                                                                                
         CLI   DBSELSRC,SRCFUS     FOR FUSION                                   
         JNE   *+12                                                             
         TM    MLSTAT,X'F0'        IGNORE SPILLS                                
         JO    DHOOKX                                                           
                                                                                
         CLI   DBSELMED,MEDTP      FOR USTV                                     
         JNE   DHOOK50                                                          
         CLC   BOOKWKMO,BKCUTOFF   IF BOOK IS PRIOR TO BOOK CUT-OFF             
         JL    DHOOKX                                                           
         CLI   BOOKTYPE,5          IGNORE NSI 2A-6A DATA                        
         JE    DHOOKX                                                           
         DROP  R2                                                               
                                                                                
DHOOK50  XC    BOOKWKMO,EFFS       RESTORE BOOKS TO REVERSE ORDER               
                                                                                
DHOOK60  CLI   BTYPE,0             FILTER ON BOOK TYPE                          
         JE    DHOOK80                                                          
         CLI   BTYPE,C'~'          WANT STANDARD BOOKTYPE                       
         JE    DHOOK70                                                          
         CLC   BTYPE,BOOKTYPE                                                   
         JNE   DHOOKX                                                           
         J     DHOOK80                                                          
DHOOK70  DS    0H                  NOT 0 BOOKTYPE                               
* CHECK  IF WE  ARE PROCESSING OVERNIGHTS                                       
* IF SO BOOKTYPE 'L' IS FINE TOO - IT IS OVERNIGHTS LIVE ONLY                   
         CLI   DBSELMED,MEDON                                                   
         JNE   *+12                                                             
         CLI   BOOKTYPE,C'L'                                                    
         JE    DHOOK80                                                          
                                                                                
DHOOK80  DS    0H                                                               
         CLI   DBSELMED,C'U'             COUNTY COVERAGE RADIO BYPASS           
         JE    *+8                       BKTYPE CHECK - BKTYPE=STATE            
         CLI   DBSELMED,C'D'             DAYPARTS FILE HAS NO BOOKTYES          
         JNE   DHOOK86                                                          
         MVC   BOOKTYPE,=X'0000'                                                
         MVC   SV2CHRBT,=X'0000'                                                
         J     DHOOK95                                                          
                                                                                
* ADD CODE TO SKIP ALL THE LIVE + BOOTKYPES FOR OVERNIGHTS                      
* WE WANT TO DUPLICATE THE LIVE ONLY BOOK LIST AND PASS IT DOWN AS              
* THE LIVE PLUS STANDARD BOOK LIST.                                             
DHOOK86  CLI   DBSELMED,MEDON                                                   
*******  JNE   DHOOK92                                                          
         J     DHOOK92                                                          
         MVI   MODEFLAG,0                                                       
         CLI   BOOKTYPE,C'H'            HISPANIC LIVE ONLY BOOK STARTED         
         JNE   *+14                     TO BE LOADED MAR12/07- WE NEED          
         CLC   BOOKWKMO,=AL1(107,11)    THE HISPANIC LIVE+ PRIOR TO.            
         JL    DHOOK92                                                          
         L     RF,ALVBTTAB                                                      
         USING LVBKTYD,RF                                                       
DHOOK90  CLC   =X'FFFF',0(RF)                                                   
         JE    DHOOK92                                                          
         CLC   LVPLUSBT,BOOKTYPE     SKIP IF LIVE PLUS BOOKTYPE                 
         JE    DHOOKX                                                           
         AH    RF,LVBTTABL                                                      
         J     DHOOK90                                                          
         DROP  RF                                                               
                                                                                
DHOOK92  LA    R2,BOOKTYPE               CONVERT TO 2 CHAR BOOKTYPE             
         MVC   SVBKTYPE,BOOKTYPE                                                
         GOTOR (#TRNSBT,ATRNSBT),DMCB,(R2),1,(R2),12                            
*                                                                               
*======================================================================         
* IF SBTK VERSION PRIOR TO V4.0 THEN EXCLUDE THESE BOOKTYPES                    
*  *************************** NOTE *****************************               
*  *THESE NIELSEN BOOKS ARE DISABLED UNTIL SBTK CAN SUPPORT THEM*               
*  *VRSN CHECK CHGD TO V7.0, SO THEY ARE ALWAYS EXCLUDED        *               
*  *                                            -BPOO 10/25/2010*               
*  *************************** NOTE *****************************               
*======================================================================         
         MVI   BOOKINDF,0                                                       
         CLC   VERSNUM,=AL4(SPVER70)                                            
         JNL   DHOOK93                                                          
         CLI   DBSELSRC,C'N'                                                    
         JNE   DHOOK95                                                          
         CLC   BOOKTYPE,=C'1 '          PERMANENTLY EXCLUDE ZEROCELL            
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'2 '                                                  
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'3 '                                                  
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'4 '                                                  
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'E '          EXTENDED                                
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'Q3'          LIVE+3 PARENT ONLY                      
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'S3'          LIVE+3 WIRED PARENT ONLY                
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'N '          SPECIAL FOX NET                         
         JE    DHOOKX                                                           
*                                                                               
         CLC   BOOKTYPE,=C'D '          DMA PREVIEW                             
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'C '          DMA CABLE                               
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'U '          LIVE CABLE                              
         JE    DHOOKX                                                           
         CLC   BOOKTYPE,=C'C3'          LIVE+3 DMA CABLE                        
         JE    DHOOKX                                                           
         J     DHOOK95                                                          
*                                                                               
*======================================================================         
* SBTK V 4.0 OR LATER, DISPLAY THESE NIELSON                                    
*======================================================================         
DHOOK93  MVI   BOOKINDF,C'A'            DEFAULT INDICATOR TO ALL                
         CLI   DBSELSRC,C'N'                                                    
         JNE   DHOOK95                                                          
         CLC   BOOKTYPE,=C'D '          DMA PREVIEW                             
         JE    DHOOK94                                                          
         XC    BOOKWKMO,EFFS                                                    
         CLC   BOOKWKMO,CBCUTOFF        CABLE BOOKTYPES ALLOW EFFECTIVE         
         JL    DHOOK93A                 JAN/11 BECAUSE OF DMA SYSCODES          
         XC    BOOKWKMO,EFFS                                                    
         J     DHOOK95                                                          
DHOOK93A XC    BOOKWKMO,EFFS            FLIP BITS BACK TO REVERSE ORDER         
         CLC   BOOKTYPE,=C'C '          DMA CABLE                               
         JE    DHOOK94                                                          
         CLC   BOOKTYPE,=C'U '          LIVE CABLE                              
         JE    DHOOK94                                                          
         CLC   BOOKTYPE,=C'C3'          LIVE+3 DMA CABLE                        
         JNE   DHOOK95                                                          
DHOOK94  MVI   BOOKINDF,C'R'            SET TO RESEARCH ONLY FOR ABOVE          
*                                       BOOKTYPES                               
*                                                                               
*======================================================================         
* SPOT DESKTOP BOOK INDICATOR FLAG                                              
*****************************  NOTE  **********************************         
* FEATURE DISABLED - DESIGN NEEDS TO BE REVISITED      -BPOO 4/29/2009*         
*****************************  NOTE  **********************************         
*======================================================================         
DHOOK95  J     *+14                                                             
         CLC   VERSNUM,=AL4(SPVER90) NEW BOOK LIST CODE ONLY FOR                
         JNL   DHOOK97               VERSION 9.0 FOR NOW                        
         MVC   (BKNETWKF-BKINDFLG)+STATIND(6),SPACES                            
         MVC   BKINDFLG,STATIND                                                 
         J     DHOOK139                                                         
DHOOK97  MVC   SV2CHRBT,BOOKTYPE                                                
                                                                                
         CLI   BTYPE,C'~'                FILTER ON STANDARD?                    
         JNE   DHOOK101                  CHECK IF CABLE BOOKTYPES               
         CLI   DBSELSRC,C'N'             FOR NSI- IF NOT NSI ~                  
         JE    DHOOK100                  FILTER ONLY WANTS 00 BOOKTYPE          
         CLI   SVBKTYPE,0                                                       
         JNE   DHOOKX                                                           
                                                                                
*  BOOKTYPES THAT ARE VALID FOR  ~ FOR NSI                                      
                                                                                
DHOOK100 LA    RE,CBTYSTND               VALID UNDER STANDARD                   
         USING CBKTYTBD,RE                                                      
         CLC   SV2CHRBT,CBKTWIRE                                                
         JE    *+10                                                             
         CLC   SV2CHRBT,CBKTDMAC            DMA CABLE?                          
         JE    *+10                                                             
         CLC   SV2CHRBT,CBKTLVOW            LIVE ONLY WIRED                     
         JE    *+10                                                             
         CLC   SV2CHRBT,CBKTLVOC            LIVE ONLY DMA CABLE                 
         JE    *+10                                                             
         CLC   SV2CHRBT,CBKTSTND            STANDARD                            
         JNE   DHOOKX                                                           
         DROP  RE                                                               
* SET STATION TYPE INDICATOR - SPOT DESKTOP DOES NOT WANT CABLE TO BE           
* DOWNLOADED SEPERATELY BY BOOKTYPE.  THEY WANT CABLE TO BE PASSED              
* DOWN TOGETHER AS THE NON CABLE BOOKTYPE AND HAVE FLAGS TO INDICATE            
* WHETHER FOR THIS BOOKTYPE/BOOK THERE ARE CABLE BOOKS AVAILABLE                
DHOOK101 LA    R6,CBTYPTAB                                                      
         USING CBKTYTBD,R6                                                      
*        CLI   SV2CHRBT,C'T'                                                    
*        JNE   *+6                                                              
*        DC    H'0'                                                             
         MVC   (BKNETWKF-BKINDFLG)+STATIND(6),=C'NNNNNN'  DEFAULT               
         CLI   DBSELSRC,C'F'                FUSION?                             
         JNE   DHOOK102                                                         
         MVI   (BKFCBLF-BKINDFLG)+STATIND,C'Y'                                  
         J     DHOOK103                                                         
DHOOK102 CLI   DBSELSRC,C'N'                                                    
         JE    DHOOK103                                                         
         MVC   (BKNETWKF-BKINDFLG)+STATIND(6),SPACES                            
         J     DHOOK122                                                         
DHOOK103 CLC   SV2CHRBT,CBKTWIRE            WIRE CABLED?                        
         JNE   DHOOK106                                                         
         MVI   (BKNCBLW-BKINDFLG)+STATIND,C'Y'                                  
         MVC   BOOKTYPE,CBKTSTND            OVERRIDE TO STANDARD                
DHOOK106 CLC   SV2CHRBT,CBKTDMAC            DMA CABLE?                          
         JNE   DHOOK108                                                         
         MVI   (BKNCBLC-BKINDFLG)+STATIND,C'Y'                                  
         MVC   BOOKTYPE,CBKTSTND            OVERRIDE TO STANDARD                
DHOOK108 CLC   SV2CHRBT,CBKTLVOW            LIVE ONLY WIRED                     
         JNE   DHOOK109                                                         
         MVI   (BKNCBLLW-BKINDFLG)+STATIND,C'Y'                                 
         MVC   BOOKTYPE,CBKTSTND            OVERRIDE TO STANDARD                
DHOOK109 CLC   SV2CHRBT,CBKTLVOC            LIVE ONLY DMA CABLE                 
         JNE   DHOOK110                                                         
         MVI   (BKNCBLLC-BKINDFLG)+STATIND,C'Y'                                 
         MVC   BOOKTYPE,CBKTSTND            OVERRIDE TO STANDARD                
DHOOK110 CLC   SV2CHRBT,CBKTSTND            STANDARD                            
         JNE   DHOOK111                                                         
         CLI   DBSELSRC,C'F'                FUSION DONT MARK                    
         JE    DHOOK111                     STANDARD NSI INDICATOR              
         MVI   (BKNETWKF-BKINDFLG)+STATIND,C'Y'                                 
         J     DHOOK112                                                         
DHOOK111 AHI   R6,CBKTYTBL                                                      
         CLI   0(R6),X'FF'                                                      
         JE    DHOOK122                                                         
         J     DHOOK103                                                         
DHOOK112 DS    0H                                                               
* IF ANY OF THE CABLE INDICATORS ARE TURNED ON-PASSING THE BOOKTYPE             
* REQUIREMENTS, CHECK CABLE LIST USED BY STAPACK TO MAKE SURE                   
* THIS IS A VALID CABLE STAION - IF NOT IN TABLE THEN TURN OFF                  
* CABLE INDICATORS                                                              
         CLI   DBSELSRC,C'F'       FUSION DOESNT HAVE TO LOOK AT                
         JE    DHOOK122            STAPACK TABLE                                
         CLI   DBSELMED,C'D'       DAYPART DOESNT HAVE TO LOOK AT               
         JE    DHOOK122            STAPACK TABLE                                
         L     RF,VCABLETB                                                      
         ZIC   R0,5(RF)            LENGTH                                       
         SHI   R0,X'80'            BUT IT WAS MODIFIED IN STAPACK:MS030         
DHOOK118 CLC   7(4,RF),CURRSTA     STATION IN TABLE?                            
         JE    DHOOK120                                                         
         AR    RF,R0                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   DHOOK118                                                         
DHOOK119 MVC   (BKNETWKF-BKINDFLG)+STATIND(6),=C'YNNNNN'                        
         J     DHOOK122                                                         
DHOOK120 MVI   (BKNETWKF-BKINDFLG)+STATIND,C'N'    CABLE IN LIST                
* IF ITS  A CABLE STATION IN STAPACK LIST BUT DOESNT PASS OUR                   
* CABLE BOOKTYPES CHECK THEN SKIP IT                                            
         CLC   (BKNETWKF-BKINDFLG)+STATIND(6),=C'NNNNNN'                        
         JE    DHOOKX                                                           
                                                                                
DHOOK122 MVC   SVBKREC(BOOKRECL),BOOKREC                                        
         GOTOR BUFFER,DMCB,('TSARDH',0)                                         
         JNE   DHOOK138                                                         
         CLC   SVBKREC(BOOKKEYL),BOOKREC                                        
         JE    DHOOK128                                                         
         MVC   BOOKREC(BOOKRECL),SVBKREC                                        
         J     DHOOK138                                                         
DHOOK128 GOTOR BUFFER,DMCB,('TSAGET',0)                                         
                                                                                
         LA    R0,6                   COMBINE STATION FLAGS                     
         LA    RE,STATIND                                                       
         LA    RF,BKINDFLG                                                      
DHOOK132 CLI   0(RE),C'Y'                                                       
         JNE   DHOOK134                                                         
         MVI   0(RF),C'Y'                                                       
DHOOK134 AHI   RE,1                                                             
         AHI   RF,1                                                             
         JCT   R0,DHOOK132                                                      
         J     DHOOK140                                                         
                                                                                
DHOOK138 MVC   BOOKREC(BOOKRECL),SVBKREC                                        
         MVC   BKINDFLG,STATIND                                                 
********************************************************************            
DHOOK139 GOTOR BUFFER,DMCB,('TSAADD',0)                                         
         J     DHOOK160                                                         
********************************************************************            
DHOOK140 DS    0H                                                               
         GOTOR BUFFER,DMCB,('TSAWRT',0)                                         
         JNE   *+2                                                              
********************************************************************            
*                                                                               
*======================================================================         
* COMPARE THE 1 BYTE BOOKTYPE READ TO SEE IF WE ARE DEALING WITH                
* OVERNIGHT LIVE ONLY BOOKTYPES, IF SO I WANT TO DUPLICATE AND ADD              
* THE SAME BOOK RECORDS INTO BUFFER FOR CORRESPONDING LIVE+ BOOKTYPES           
*======================================================================         
DHOOK160 CLI   DBSELMED,MEDON                                                   
         JNE   DHOOK300                                                         
         CLI   MODEFLAG,LIVEPLUS               ALREADY PROCESSED LIVE+?         
         JE    DHOOKX                          IF SO EXIT                       
         L     RF,ALVBTTAB                                                      
         USING LVBKTYD,RF                                                       
DHOOK180 CLC   =X'FFFF',0(RF)                                                   
         JE    DHOOK20                                                          
         CLC   LVONLYBT,SVBKTYPE                LIVE ONLY BOOKTYPE?             
         JE    DHOOK240                                                         
         AH    RF,LVBTTABL                                                      
         J     DHOOK180                                                         
DHOOK240 MVI   MODEFLAG,LIVEPLUS               LIVE ONLY BOOKTYPE FOUND         
         XC    BOOKTYPE,BOOKTYPE                                                
         MVC   BOOKTYPE(L'LVPLUSBT),LVPLUSBT   ADD LIVE+ BOOKTYPE REC           
         J     DHOOK92                                                          
         DROP  RF                                                               
                                                                                
*======================================================================         
* ADD FUSION LS BOOKS ON|AFTER JAN/17 TO ALSO SHOW UP AS STANDARD               
*                                           MOSBTK-429 -HWON 05/10/2017         
** COMMENTED OUT**                                                              
* ADD NSI LS BOOKS ON|AFTER JAN/19 TO ALSO SHOW UP AS STANDARD                  
*                              SPEC-37508 & SPEC-37012 -HWON  7/31/2019         
** COMMENTED OUT**                                                              
*======================================================================         
DHOOK300 DS    0H                                                               
         CLI   BTYPE,0             FILTERING BY BOOKTYPE?                       
         JNE   DHOOKX               YES, SKIP                                   
         CLI   DBSELMED,MEDTP      BOOK IS FOR USTV?                            
         JNE   DHOOKX               NO, SKIP                                    
         CLC   BOOKTYPE,=C'LS'     BOOK IS FOR BOOKTYPE LS?                     
         JNE   DHOOKX               NO, SKIP                                    
*&&DO                                                                           
         LA    RF,=AL1(119,01)     NSI, CHK BOOK ON OR AFTER JAN/19?            
         CLI   DBSELSRC,SRCNSI     SOURCE IS FOR NSI?                           
         JE    DHOOK305             YES                                         
*&&                                                                             
         LA    RF,=AL1(117,01)     FUSION, CHK BOOK ON OR AFTER JAN/17?         
         CLI   DBSELSRC,SRCFUS     SOURCE IS FOR FUSION?                        
         JNE   DHOOKX               NO, SKIP                                    
DHOOK305 XC    BOOKWKMO,EFFS                                                    
         CLC   BOOKWKMO,0(RF)      IS BOOK ON OR AFTER ABOVE DATE               
         JL    DHOOK310                NO, SKIP                                 
*                                                                               
         XC    BOOKWKMO,EFFS                                                    
         XC    BOOKTYPE,BOOKTYPE     YES, ADD AS ADDITIONAL STANDARD            
         J     DHOOK92                                                          
DHOOK310 XC    BOOKWKMO,EFFS                                                    
DHOOKX   J     EXITY                           RETURN TO DEMAND                 
         DROP  RC                                                               
                                                                                
NBWORKD  DSECT                     ** NXTBLD LOCAL W/S **                       
NBCARD   DS    CL80                SCANNER INPUT                                
NBSCAN   DS    XL(SCBLKLQ)         SCANNER OUTPUT                               
NBSPILL  DS    XL2                 NUMERIC SPILL MARKET                         
ADHOOK   DS    A                                                                
ALVBTTAB DS    A                   A(LIVE/LIVE PLUS BKTYPE TABLE)               
LVBTTABL DS    H                   LENGTH OF TABLE ENTRY                        
SVBKTYPE DS    CL(L'BOOKTYPE)                                                   
MODEFLAG DS    C                                                                
LIVEPLUS EQU   X'80'               PROCESSED LIVE PLUS BOOKTYPE                 
SV2CHRBT DS    CL2                                                              
STATIND  DS    CL(L'BKINDFLG)      STATION INDICATOR                            
SVBKREC  DS    CL(BOOKRECL)                                                     
CURRSTA  DS    CL(L'DBSELSTA)                                                   
SVDBERR  DS    CL(L'DBERROR)                                                    
MYDMCB   DS    6F                                                               
*                                                                               
GTADFSBK DS    C                   GET ADDITIONAL FUSION BOOK FLAG              
GAFBINIT EQU   0                   INIT                                         
GAFBFUS  EQU   C'F'                GET ADDITIONAL FUSION BOOKS                  
GAFBNSI  EQU   C'N'                GET ONLY NSI BOOKS                           
AFFILS   DS    CL5                                                              
DBLOCK2  DS    XL(L'DBLOCK)                                                     
NBWORKL  EQU   *-NBWORKD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* NEXT STATION ROUTINE                                                *         
***********************************************************************         
* -----------------------------------------------------------------             
* BEN - 03/2010                                                                 
* NOTE - FOR ARBITRON BOOK LIST LOOKUP - IT HAS BEEN DISCOVERED THAT            
* THE PC IS SPLITTING THE STATION/SPILL MKT INTO THE 2 SEPERATE MAPS            
* WHEN IN DESIGN THE STAITON AND MARKET MAPCODES ARE NOT MEANT                  
* TO HANDLE THIS.  THE MARKET MAPCODE WAS ALWAYS MEANT TO BE                    
* A BOOK LIST FOR THE ENTIRE MARKET.                                            
* I WILL CHANGE THE CODE TO HONOR THE STATION MAPCODE AND MARKET MAP            
* TO BE THE SAME AS STATION/SPILL STRING PASSED INTO THE STATION MAP            
* THIS WILL ALSO MEAN WE WILL NOT BE ABLE TO HANDLE MULTIPLE STATIONS           
* FOR MULTIPLE MARKETS IF THAT IS EVER NEEDED IN THE FUTURE                     
* -----------------------------------------------------------------             
                                                                                
NXTSTA   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSTA10                                                         
         XC    STATN,STATN                                                      
* THE XT (DUMMY FILETYPE) FOR MEDIA X, DOES NOT HAVE BOKS                       
         CLC   =C'XT ',FILEID                                                   
         JE    NOMORE                                                           
* RADIO COUNTY COVERAGE DONT HAVE STATION LEVEL BOOK LIST                       
         CLC   =C'RUA',FILEID                                                   
         JE    NXTSTA20                                                         
*                                                                               
         SR    R4,R4               START AT FIRST STATION/SPILL                 
         ICM   R4,7,ASTA                                                        
         JNZ   NXTSTA05                                                         
         XC    STACOUNT,STACOUNT   NO STATIONS                                  
         OC    MARKET,MARKET       MARKET ONLY LOOKUP                           
         JNZ   NXTSTA20                                                         
         DC    H'0'                NO MARKET OR STATION - DIE                   
NXTSTA05 SR    R3,R3                                                            
         ICM   R3,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         STCM  R4,15,ASTANTRY                                                   
         STCM  R3,3,STACOUNT                                                    
NXTSTA10 DS    0C                                                               
* RADIO COUNTY COVERAGE DONT HAVE STATION LEVEL BOOK LIST                       
         CLC   =C'RUA',FILEID                                                   
         JE    NOMORE                                                           
                                                                                
NXTSTA14 SR    R3,R3                                                            
         ICM   R3,3,STACOUNT                                                    
         JZ    NOMORE                                                           
         BCTR  R3,0                                                             
         STCM  R3,3,STACOUNT                                                    
         J     NXTSTA16                                                         
                                                                                
         OC    MARKET,MARKET       MARKET - NO STATION                          
         JNZ   NOMORE                                                           
NXTSTA16 ICM   R4,15,ASTANTRY      LIST IS SORTED!                              
         CLC   STATN,0(R4)         SAME STATION/NETWORK?                        
         JNE   NXTSTA18             NO                                          
         LA    R4,STAL(R4)          YES, SKIP IT AND PROCESS NEXT               
         STCM  R4,15,ASTANTRY                                                   
         J     NXTSTA14                                                         
*                                                                               
NXTSTA18 MVC   STATN,0(R4)                                                      
         CLC   =C'RUA',BOOKFILE    DISPLAY NO STATION FOR COUNTY                
         JNE   *+10                COVERAGE FOR RADIO                           
         XC    STATN,STATN                                                      
         LA    R4,STAL(R4)                                                      
         STCM  R4,15,ASTANTRY                                                   
                                                                                
NXTSTA20 LA    R0,STATN                                                         
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CANADIAN DESKTOP STATION BOOK LIST                                  *         
***********************************************************************         
                                                                                
NXTSBL   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSBL12                                                         
                                                                                
         LA    R0,DBLOCK                                                        
         LHI   R1,DBLOCKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   DBFILE,FILTP                                                     
         MVI   DBSELMED,C'C'                                                    
         MVI   DBSELSRC,SRCNTI     SET SOURCE AS NTI (DEFAULT)                  
         CLI   RTGSVC,C'1'                                                      
         JNE   *+8                                                              
         MVI   DBSELSRC,SRCBBM     SET SOURCE AS BBM                            
         MVC   DBSELSTA,STATION                                                 
         CLI   MEDIA,0                                                          
         JNE   *+8                                                              
         MVI   MEDIA,C'T'                                                       
         MVC   DBSELSTA+L'DBSELSTA-L'MEDIA(L'MEDIA),MEDIA                       
         MVC   DBSELAGY,LP_AGY                                                  
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBFUNCT,DBGETMB                                                  
                                                                                
         GOTOR BUFFER,DMCB,('TSAINI',0),('BOOKKEYL',BOOKRECL)                   
                                                                                
NXTSBL02 GOTOR VDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,IOEEOF      TEST DEMAND POSTED EOF                       
         JE    NXTSBL10                                                         
         CLI   DBERROR,0           EXIT ON OTHER ERRORS                         
         JNE   NOMORE                                                           
                                                                                
         L     R2,DBAREC                                                        
         USING SBKEY,R2            R2=A(STATION/BOOK RECORD)                    
****                                                                            
**** CODE REMOVED, THIS CODE WOULD NEVER HAVE WORKED PROPERLY                   
****                                                 -HWON 02/03/16             
****     CLI   DBSELAGY,C'T'                                                    
****     JNE   *+14                                                             
****     CLC   SBBOOK,=AL1(108,52)                                              
****     JNL   NXTSBL02                                                         
         CLI   SBBTYP,C'W'         TEST WEEKLY BOOK                             
         JE    NXTSBL04                                                         
         CLI   DBSELSRC,SRCNTI     TEST NTI/CSI                                 
         JNE   NXTSBL06                                                         
         CLC   SBBOOK,=AL1(96,01)  TEST JAN/96 OR LATER                         
         JL    NXTSBL06                                                         
                                                                                
NXTSBL04 GOTOR VNSIWEEK,DMCB,(C'D',SBBOOK),(1,VGETDAY),VADDAY,VDATCON           
         ICM   R1,7,1(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(6),0(R1)                                                    
*                                                                               
         L     RF,=V(GETBROAD)                                                  
         A     RF,SRVRRELO                                                      
         GOTOR (RF),DMCB,(X'01',WORK),TEMPBRO,VGETDAY,VADDAY                    
         MVC   TEMPBRO(6),WORK                                                  
         GOTOR VADDAY,DMCB,TEMPBRO,TEMPBRO,6                                    
         CLC   TEMPBRO(6),TEMPBRO+6                                             
         JNE   NXTSBL02                                                         
*                                                                               
         GOTOR VDATCON,DMCB,(0,WORK),(3,WORK+6)                                 
         MVC   BOOKWKMO,WORK+6                                                  
         J     NXTSBL08                                                         
                                                                                
NXTSBL06 MVC   BOOKWKMO,SBBOOK                                                  
                                                                                
NXTSBL08 XC    BOOKWKMO,EFFS                                                    
         MVC   BOOKMKTA,DBSELALF                                                
         GOTOR BUFFER,DMCB,('TSAADD',0)                                         
         J     NXTSBL02                                                         
         DROP  R2                                                               
                                                                                
NXTSBL10 XC    BOOKREC(BOOKKEYL),BOOKREC                                        
         GOTOR BUFFER,DMCB,('TSARDH',0)                                         
         J     NXTSBL14                                                         
                                                                                
NXTSBL12 GOTOR BUFFER,DMCB,('TSANXT',0)                                         
                                                                                
NXTSBL14 TM    TSERRS,TSEEOF       TEST ALL RECORDS READ                        
         JNZ   NOMORE                                                           
         LA    R0,BOOKREC                                                       
         ST    R0,LP_ADATA         RETURN BOOK RECORD                           
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROJECTION FORMULA LIST                                    *         
***********************************************************************         
                                                                                
NXTPRJ   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTPRJ10                                                         
         SR    R2,R2                                                            
         ICM   R2,7,APROJ                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R3,R3                                                            
         ICM   R3,3,LW_NUMN-LW_D(R2)                                            
         LA    R2,LW_DATA2-LW_D(R2)                                             
         STCM  R2,15,APRJNTRY                                                   
         STCM  R3,3,PRJCOUNT                                                    
                                                                                
                                                                                
NXTPRJ10 DS    0H                                                               
         ICM   R3,3,PRJCOUNT                                                    
         JZ    NOMORE                                                           
         BCTR  R3,0                                                             
         STCM  R3,3,PRJCOUNT                                                    
* CLEAR BLOCK                                                                   
         LA    R0,GDEMBLK                                                       
         LHI   R1,GDEMBLKL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
                                                                                
* COUNT LENGTH OF INPUT STRING                                                  
         ICM   R2,15,APRJNTRY                                                   
         LA    R0,0                                                             
NXTPRJ20 CHI   R0,PROJL           MAX LENGTH OF STRING                          
         JE    NXTPRJ30                                                         
         CLI   0(R2),X'40'                                                      
         JNH   NXTPRJ30                                                         
         AHI   R0,1                                                             
         AHI   R2,1                                                             
         J     NXTPRJ20                                                         
                                                                                
NXTPRJ30 ICM   R2,15,APRJNTRY                                                   
         USING PJWORKD,R2                                                       
* CALL UPGRADE VALIDATION ROUTINE                                               
         LA    R4,GDEMBLK+(GDUPG-GDEMBLKD)                                      
*                                                                               
         GOTOR (#VALUPG,AVALUPG),DMCB,(R2),(R0),(R4)                            
*                                                                               
         MVC   PROJFORM,PJSTRING                                                
         MVC   PROJID,PJIDNUM                                                   
         MVI   PROJVFLG,C'Y'                                                    
         JE    *+8                                                              
         MVI   PROJVFLG,C'N'                                                    
         LA    R2,PJWORKL(R2)                                                   
         STCM  R2,15,APRJNTRY                                                   
         LA    R0,PROJREC                                                       
         ST    R0,LP_ADATA         RETURN BOOK RECORD                           
NXTPRJX  J     EXITY                                                            
         DROP  R2                                                               
PJWORKD  DSECT                     ** NXTPRJ LOCAL W/S **                       
PJSTRING DS    CL(PROJL)           PROJECTION STRING                            
PJIDNUM  DS    CL(L'PROJID)        PROJECTION ID NUMBER                         
PJWORKL  EQU   *-PJWORKD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMO LOOKUPS                                                        *         
***********************************************************************         
                                                                                
NXTDLU   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         MVC   GDAESTR,AIO3+1                                                   
         XC    GDSTABTY,GDSTABTY             CLEAR TO FORCE LOOKUP              
         XC    GDSTAMKT,GDSTAMKT             CLEAR TO FORCE LOOKUP              
*                                                                               
         GOTOR (#GETDEM,AGETDEM),GDEMBLK                                        
         JNE   NOMORE                                                           
         LA    R0,GDEMBLK                                                       
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* COUNTY LIST                                                         *         
***********************************************************************         
                                                                                
         USING NCWORKD,RC                                                       
NXTCNTY  CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCTY12                                                         
* CALL DEMO SYSTEM FOR COUNTY LIST                                              
         GOTOR BUFFER,DMCB,('TSAINI',0),('CNTYRECL',CNTYRECL)                   
         LA    R0,DBLOCK                                                        
         LHI   R1,DBLOCKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   DBFILE,FILRTP                                                    
         MVI   DBSELSRC,SRCARB                                                  
         MVI   DBSELMED,MEDCTY                                                  
         MVC   DBSELBK,BOOK+4                                                   
         MVC   DBBTYPE,STATE                                                    
         MVI   DBFUNCT,DBGETMKB                                                 
         MVC   DBSELAGY,LP_AGY                                                  
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,=A(CTYHOOK)      CALL  DEMAND FOR COUNTY LIST                 
         A     RE,SRVRRELO                                                      
         ST    RE,NCADHOOK                                                      
         GOTOR VDEMAND,DMCB,DBLOCK,NCADHOOK                                     
* TEST IF HAVE TO SET FROM HERE                                                 
NXTCTY10 XC    CNTYREC(CNTYRECL),CNTYREC                                        
         GOTOR BUFFER,DMCB,('TSARDH',0)                                         
         J     NXTCTY14                                                         
                                                                                
NXTCTY12 GOTOR BUFFER,DMCB,('TSANXT',0)                                         
                                                                                
NXTCTY14 TM    TSERRS,TSEEOF       TEST ALL RECORDS READ                        
         JNZ   NOMORE                                                           
         LA    R0,CNTYREC                                                       
         ST    R0,LP_ADATA         RETURN BOOK RECORD                           
         J     EXITY                                                            
*                                                                               
* HOOK TO COUNTY NAME RECORDS                                                   
CTYHOOK  DS    0H                                                               
         L     R2,DBAREC                                                        
         USING CYKEY,R2                                                         
         XC    CNTYREC(CNTYRECL),CNTYREC                                        
         MVC   COUNTYCD,CYCOUNTY                                                
         LA    RE,CYFRSTEL                                                      
         CLI   0(RE),DMECODEQ         MUST FIND COUNTY ELEMENT                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING DMELEM,RE                                                        
         MVC   TIER,DMMNO+1                                                     
         ZIC   RF,DMLEN                                                         
         SHI   RF,L'DMECODE+L'DMLEN+L'DMMNO                                     
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,8(R1)                                                         
         J     *+10                                                             
         MVC   COUNTYNM(0),DMMNAME                                              
         GOTOR BUFFER,DMCB,('TSAADD',0)                                         
         DROP  R2,RE,RC                                                         
         J     EXITY                                                            
NCWORKD  DSECT                     ** NXTCNTY LOCAL W/S **                      
NCADHOOK DS    A                                                                
NCWORKL  EQU   *-NCWORKD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILE                                                       *         
***********************************************************************         
                                                                                
VALFIL   LM    R2,R4,LP_AINP                                                    
         BCTR  R3,0                                                             
         MVC   WORK(L'VBOFILE),SPACES                                           
         BASR  RE,0                                                             
         EX    R3,8(RE)            EXTRACT INPUT INTO WORK                      
         J     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
                                                                                
         LA    RF,BLTTAB           LOOK UP FILE NAME IN TABLE                   
         USING BLTTABD,RF                                                       
         LHI   R0,BLTTABN                                                       
VALFIL02 CLC   BLTTFILE,WORK                                                    
         JE    VALFIL04                                                         
         AHI   RF,BLTTABL                                                       
         JCT   R0,VALFIL02                                                      
         J     EXITN                                                            
                                                                                
VALFIL04 MVC   0(L'VBOFILE,R4),BLTTFILE                                         
         MVC   FILEINDS,BLTTINDS                                                
         J     EXITY                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT CODE                                                *         
***********************************************************************         
                                                                                
VALCLT   GOTOR (#VALCLT,AVALCLT),LP_AINP                                        
         JNE   EXITCC                                                           
         MVC   GDCLTA,QCLTA                                                     
         J     EXITCC                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEMO CODE FOR RADIO                                        *         
***********************************************************************         
                                                                                
         USING VDWORKD,RC                                                       
VRDB     USING DBLOCK,VDDBLOCK                                                  
VALRDEM  LM    R2,R4,LP_AINP       R2=A(INPUT STRING), R3=INPUT LENGTH          
         BCTR  R4,0                                                             
         USING VDOUTD,R4           R4=A(DEMO OUTPUT ROW)                        
                                                                                
         LA    R0,VDWORKD                                                       
         LHI   R1,VDWORKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   VDFLDH,L'VDFLDH+L'VDFLD                                          
         STC   R3,VDFLDH+5                                                      
         MVC   VDFLD,SPACES                                                     
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VDFLD(0),0(R2)                                                   
                                                                                
         CLI   VDODEMTY,C'T'                                                    
         JNE   *+8                                                              
         MVI   VDODEMTY,C'I'                                                    
                                                                                
                                                                                
         MVI   VRDB.DBSELMED,MEDRAD                                             
         MVI   VRDB.DBSELSRC,SRCARB                                             
         MVC   VRDB.DBCOMFCS,ACOMFACS                                           
         MVC   VRDB.DBSELAGY,LP_AGY                                             
         GOTOR VDEMOVAL,DMCB,VDFLDH,(1,VDODEMCD),VRDB.DBLOCK                    
         CLI   4(R1),0                                                          
         JE    EXITN                                                            
         CLI   VDODEMTY,C'C'        WE WANT FULL PRECISION CUMES                
         JNE   *+8                                                              
         MVI   VDODEMTY,C'W'                                                    
         CLI   VDODEMTY,C'I'        WE WANT FULL PRECISION IMPS                 
         JNE   *+8                                                              
         MVI   VDODEMTY,C'B'                                                    
         MVI   VDODEMYN,VDODEMYQ   SET DEMO IS VALID                            
         MVC   VDODEMNM,SPACES                                                  
         MVC   VRDB.DBFILE,FILTP                                                
         GOTOR VDEMOCON,DMCB,(1,VDODEMCD),(6,VDODEMNM),                *        
               (C'S',VRDB.DBLOCK),0                                             
         J     EXITY                                                            
         DROP  R4,RC                                                            
***********************************************************************         
***********************************************************************         
* VALIDATE DEMO CODE                                                  *         
***********************************************************************         
                                                                                
         USING VDWORKD,RC                                                       
VDB      USING DBLOCK,VDDBLOCK                                                  
VALDEM   LM    R2,R4,LP_AINP       R2=A(INPUT STRING), R3=INPUT LENGTH          
         USING VDOUTD,R4           R4=A(DEMO OUTPUT ROW)                        
                                                                                
         LA    R0,VDWORKD                                                       
         LHI   R1,VDWORKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   VDFLDH,L'VDFLDH+L'VDFLD                                          
         STC   R3,VDFLDH+5                                                      
         MVC   VDFLD,SPACES                                                     
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VDFLD(0),0(R2)                                                   
                                                                                
         MVC   VDB.DBFILE,FILTP                                                 
         MVI   VDB.DBSELMED,MEDTP                                               
         MVI   VDB.DBSELSRC,SRCNSI                                              
         MVC   VDB.DBCOMFCS,ACOMFACS                                            
         MVC   VDB.DBSELAGY,LP_AGY                                              
         GOTOR VDEMOVAL,DMCB,VDFLDH,(1,VDODEMCD),VDB.DBLOCK                     
         CLI   4(R1),0                                                          
         JE    EXITN                                                            
*&&DO                                                                           
         CLI   VDODEMTY,C'R'       ENSURE GOOD PREFIX                           
         JE    *+8                                                              
         CLI   VDODEMTY,C'I'                                                    
         JE    *+8                                                              
         CLI   VDODEMTY,C'D'                                                    
         JE    *+8                                                              
         CLI   VDODEMTY,C'T'                                                    
         JNE   EXITN                                                            
*&&                                                                             
         MVI   VDODEMYN,VDODEMYQ   SET DEMO IS VALID                            
         CLI   VDODEMTY,C'T'                                                    
         JNE   *+8                                                              
         MVI   VDODEMTY,C'I'                                                    
         MVC   VDODEMNM,SPACES                                                  
         GOTOR VDEMOCON,DMCB,(1,VDODEMCD),(6,VDODEMNM),                *        
               (C'S',VDB.DBLOCK),0                                              
         J     EXITY                                                            
         DROP  R4,RC                                                            
                                                                                
VDWORKD  DSECT                     ** VALDEM LOCAL W/S **                       
VDFLDH   DS    XL8                 DUMMY FIELD HEADER                           
VDFLD    DS    CL80                DUMMY INPUT FIELD                            
VDDBLOCK DS    XL(DBLOCKL)         DEMO CONTROL BLOCK                           
VDWORKL  EQU   *-VDWORKD                                                        
                                                                                
VDOUTD   DSECT                     ** DEMO OUTPUT ROW **                        
VDODEMCD DS    0XL3                ** DEMO CODE **                              
         DS    X                                                                
VDODEMTY DS    C                   DEMO TYPE                                    
VDODEMNO DS    X                   DEMO NUMBER                                  
VDODEMYN DS    C                   ** DEMO IS VALID INDICATOR **                
VDODEMYQ EQU   C'Y'                DEMO IS VALID                                
VDODEMNQ EQU   C'N'                DEMO IS INVALID                              
VDODEMNM DS    CL20                DEMO NAME/INPUT STRING                       
VDOUTL   EQU   *-VDOUTD            WIDTH OF DEMO ROW                            
VDOUTL2  EQU   *-VDODEMTY          WIDTH OF DEMO ROW                            
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION                                                    *         
***********************************************************************         
                                                                                
***      USING VDIWORKD,RC                                                      
VALDMIND LM    R2,R4,LP_AINP                                                    
         USING VDIOUTD,R4                                                       
         USING VDIINPD,R2                                                       
         CHI   R3,3                MUST BE LENGTH OF 3!!                        
         JNE   EXITN                                                            
         CLC   0(3,R2),=C'MSA'                                                  
         JNE   *+12                                                             
         MVI   VDIODMIN,X'00'      MSA DEFAULT                                  
         J     EXITY                                                            
         CLC   0(3,R2),=C'TSA'                                                  
         JNE   *+12                                                             
         MVI   VDIODMIN,X'02'                                                   
         J     EXITY                                                            
         CLC   0(3,R2),=C'ADI'                                                  
         JNE   *+12                                                             
         MVI   VDIODMIN,X'03'                                                   
         J     EXITY                                                            
* ONLY GET HERE IF NOT VALID (MSA/TSA/ADI)                                      
         J     EXITN                                                            
VDIINPD  DSECT                                                                  
VDIIDMIN DS    CL3                                                              
                                                                                
VDIOUTD  DSECT                                                                  
VDIODMIN DS    X                                                                
VDIOUTL  EQU   *-VDIOUTD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION                                                    *         
***********************************************************************         
                                                                                
         USING VSWORKD,RC                                                       
VALSTA   LM    R2,R4,LP_AINP                                                    
         USING VSOSTA,R4           R4=A(STATION OUTPUT ROW)                     
         STC   R3,VSOILEN                                                       
         MVI   VSOSPILL,VSOSPINQ                                                
         MVC   VSCARD,SPACES                                                    
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VSCARD(0),0(R2)                                                  
         MVC   VSOINPUT,VSCARD                                                  
                                                                                
         GOTOR VSCANNER,DMCB,(C'C',VSCARD),VSSCAN,C',=,/'                       
         CLI   4(R1),1                                                          
         JNE   VALSTAN                                                          
         USING SCANBLKD,VSSCAN                                                  
         CLI   SC1STLEN,0          GET INPUT LEFT OF SEPARATOR?                 
         JE    VALSTAN              NO                                          
         TM    SC1STVAL,SCNUMQ     IS IT NUMERIC?                               
         JZ    VALSTA02             NO                                          
         CLI   SC1STLEN,4          NUMERIC FIELD >4?                            
         JH    VALSTAN              OK, THATS BAD!                              
         J     VALSTA04                                                         
                                                                                
VALSTA02 CLI   SC1STLEN,5                                                       
         JH    VALSTAN                                                          
                                                                                
VALSTA04 MVI   VSOVALYN,VSOVALYQ   SET STATION IS VALID                         
         CLI   SC2NDLEN,0          MARKET/SPILL?                                
         JE    *+8                   NO                                         
         MVI   VSOSPILL,VSOSPIYQ     YES                                        
         J     EXITY                                                            
                                                                                
VALSTAN  MVI   VSOVALYN,VSOVALNQ   SET STATION IS NOT VALID                     
         J     EXITY                                                            
         DROP  R4,RC                                                            
                                                                                
VSWORKD  DSECT                     ** VALSTA LOCAL W/S **                       
VSCARD   DS    CL80                SCANNER INPUT                                
VSSCAN   DS    10XL(SCBLKLQ)       SCANNER OUTPUT                               
VSWORKL  EQU   *-VSWORKD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MULTIPLE DAY/TIME EXPRESSIONS                              *         
***********************************************************************         
                                                                                
         USING VMWORKD,RC                                                       
VALMDT   LM    R2,R4,LP_AINP                                                    
*&&DO                                                                           
*------------------------------------------------------------------             
*- NOTE- NO NEED TO SCAN FOR "+" TO PARSE OUT MULTI DAYTIME       |             
* ROTATIONS.  SPOT DESKTOP DOES NOT NEED IT RIGHT NOW             |             
* BECAUSE WE HAVE PROBLEMS WITH STRINGS LONGER THAN 20 CHARS      |             
* JUST CALL ROUTINE TO VALIDATE DAYTIME STRING.                   |             
* IF SPOT DESKTOP DECIDES TO SUPPORT MULT DAYTIME ROTATIONS THEN  |             
* WE CANT USE THE COMMA DAYTIME SYNTAX BECAUSE IT CAN GO OVER     |             
* 20 CHARS.  WE'LL NEED TO USE PERIOD DAY FORMAT                  |             
* BEN                                                             |             
*------------------------------------------------------------------             
         GOTOR (#VALDTM,AVALDTM),DMCB,(R2),(R3),(R4)                            
         JNE   EXITN                                                            
         AHI   R4,VSODTMEW                                                      
         J     VALMDTX                                                          
*&&                                                                             
********************************************************************            
* THE START OF CODE WHICH SUPPORTS MULTI DAYTIME ROTATIONS         *            
********************************************************************            
         XC    VMFLDH,VMFLDH                                                    
         MVI   VMFLDH,L'VMFLD+L'VMFLDH                                          
         MVI   VMFLD,C' '                                                       
         MVC   VMFLD+1(L'VMFLD-1),VMFLD                                         
         STC   R3,VMFLDH+5                                                      
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VMFLD(0),0(R2)                                                   
                                                                                
         MVI   SCANLEN,30          NON STANDARD LENGTH -30 MORE                 
         ICM   R0,15,VMSCANDC                                                   
         GOTOR VSCANNER,DMCB,(SCANLEN,VMFLDH),VMSCAN,(R0)                       
***      GOTOR VSCANNER,DMCB,VMFLDH,VMSCAN,(R0)                                 
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          R0=NUMBER OF SCANNER LINES                   
         JZ    EXITN                                                            
         CHI   R0,VSODTME#                                                      
         JH    EXITN                                                            
         LA    R2,VMSCAN                                                        
         USING SCANBLKD,R2         R2=A(SCANNER LINE)                           
                                                                                
VALMDT02 SR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         OC    SC1STFLD,SPACES                                                  
         GOTOR (#VALDTM,AVALDTM),DMCB,SC1STFLD,(RF),(R4)                        
         JNE   EXITN                                                            
*****    AHI   R2,L'VMSCAN                                                      
         ZIC   RE,SCANLEN                                                       
         OR    RE,RE               CHECK IF NON STANDARD SCANNER LENGTH         
         JZ    *+8                 WAS SPECIFIED- IF SO ADD FOR LINE+22         
         AHI   RE,22                                                            
         AR    R2,RE                                                            
         AHI   R4,VSODTMEW                                                      
         JCT   R0,VALMDT02                                                      
******************************************************************              
VALMDTX  MVI   0(R4),FF                                                         
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
VMWORKD  DSECT                     ** VALMDT LOCAL W/S **                       
VMFLDH   DS    XL8                 DUMMY FIELD HEADER                           
VMFLD    DS    XL240               DUMMY INPUT FIELD                            
VMSCAN   DS    15XL(SCBLKLQ)       SCANNER OUTPUT                               
VMWORKL  EQU   *-VMWORKD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE EFFECTIVE DATES                                            *         
***********************************************************************         
                                                                                
         USING VEWORKD,RC                                                       
VALDAT   LM    R2,R4,LP_AINP                                                    
         MVC   VECARD,SPACES                                                    
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VECARD(0),0(R2)                                                  
                                                                                
         GOTOR VSCANNER,DMCB,(C'C',VECARD),VESCAN,C',= -'                       
         CLI   4(R1),0                                                          
         JE    EXITN                                                            
         USING SCANBLKD,VESCAN                                                  
         CLI   SC1STLEN,0                                                       
         JE    EXITN                                                            
                                                                                
         GOTOR VDATVAL,DMCB,(0,SC1STFLD),VEEDATE                                
         OC    0(4,R1),0(R1)                                                    
         JZ    EXITN                                                            
         GOTOR VDATCON,DMCB,(0,VEEDATE),(2,0(R4))                               
                                                                                
         CLI   SC2NDLEN,0          TEST SINGLE DATE ENTERED                     
         JE    EXITY                                                            
         GOTOR VDATVAL,DMCB,(0,SC2NDFLD),VEEDATE                                
         OC    0(4,R1),0(R1)                                                    
         JZ    EXITN                                                            
         GOTOR VDATCON,DMCB,(0,VEEDATE),(2,2(R4))                               
         J     EXITY                                                            
         DROP  RC                                                               
                                                                                
VEWORKD  DSECT                     ** VALEFD LOCAL W/S **                       
VECARD   DS    CL80                SCANNER INPUT                                
VEEDATE  DS    CL6                 EBCDIC DATE                                  
VESCAN   DS    15XL(SCBLKLQ)       SCANNER OUTPUT                               
VEWORKL  EQU   *-VEWORKD                                                        
                                                                                
VSOUTD   DSECT                     ** STATION/DAY/TIME ARRAY **                 
                                                                                
VSOSTA   DS    0C                  ** STATION **                                
VSOINPUT DS    CL11                INPUT FIELD                                  
VSOVALYN DS    C                   ** STATION IS VALID INDICATOR **             
VSOVALYQ EQU   C'Y'                STATION IS VALID                             
VSOVALNQ EQU   C'N'                STATION IS NOT VALID                         
VSOILEN  DS    X                   LENGTH OF INPUT STRING                       
VSOSPILL DS    C                   ** SPILL INDICATOR **                        
VSOSPIYQ EQU   C'Y'                SPILL INPUT                                  
VSOSPINQ EQU   C'N'                SPILL NOT INPUT                              
VSOSTAL  EQU   *-VSOSTA                                                         
VSOSTATE DS    (VSOSTAT#)XL(VSOSTATW),X    STATES                               
VSOSTATL EQU   *-VSOSTATE                                                       
VSOCNTY  DS    (VSOCNTY#)XL(VSOCNTYW),X    COUNTIES                             
VSOCNTYL EQU   *-VSOCNTY                                                        
VSOCNTY# EQU   10                  MAXIMUM NUMBER OF COUNTIES                   
VSOCNTYW EQU   2                   LENGTH OF EACH ENTRY IS 2                    
VSOSTAT# EQU   10                  MAXIMUM NUMBER IF STATES                     
VSOSTATW EQU   1                   LENGTH OF EACH ENTRY IS 2                    
                                                                                
VSODTM   DS    0X                  ** DAY/TIME VALUES **                        
VSODTME# EQU   12                  MAXIMUM NUMBER OF ENTRIES                    
VSODTMEW EQU   5                   WIDTH OF EACH ENTRY                          
         DS    (VSODTME#)XL(VSODTMEW),X                                         
VSODTML  EQU   *-VSODTM                                                         
                                                                                
**VSOEFDT  DS    XL4                 EFFECTIVE DATE                             
**VSOINVN  DS    CL4                 INVENTORY NUMBER                           
VSOBUYD  DS    XL4                 BUYSTART-BUYEND DATE                         
VSOLPMSD DS    XL4                 LPM START DATE                               
VSOSYSC  DS    XL2                 SYSCODE                                      
                                                                                
VSOROWS  DS    XL4                 NUMBER OF ROWS                               
                                                                                
VSOUTL   EQU   *-VSOUTD                                                         
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MULTIPLE BOOKS                                             *         
***********************************************************************         
                                                                                
         USING VBWORKD,RC                                                       
VALBKS   LM    R2,R4,LP_AINP                                                    
         XC    VBFLDH,VBFLDH                                                    
         MVI   VBFLDH,L'VBFLD+L'VBFLDH                                          
         MVI   VBFLD,C' '                                                       
         MVC   VBFLD+1(L'VBFLD-1),VBFLD                                         
         STC   R3,VBFLDH+5                                                      
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VBFLD(0),0(R2)                                                   
                                                                                
         ICM   R0,15,VBSCANDC                                                   
         GOTOR VSCANNER,DMCB,VBFLDH,VBSCAN,(R0)                                 
         MVC   VBSCAN#,4(R1)                                                    
         SR    R0,R0                                                            
         ICM   R0,1,VBSCAN#        R0=NUMBER OF SCANNER LINES                   
         JZ    EXITN                                                            
         LA    R2,VBSCAN                                                        
         USING SCANBLKD,R2         R2=A(SCANNER LINE)                           
                                                                                
VALBKS02 SR    RF,RF                                                            
         ICM   RF,1,SC1STLEN                                                    
         JZ    EXITN                                                            
         OC    SC1STFLD,SPACES                                                  
         GOTOR (#VALDLB,AVALDLB),DMCB,(FILEINDS,SC1STFLD),(RF),VBWORK           
         JNE   EXITN                                                            
                                                                                
         CLM   R0,1,VBSCAN#        TEST FIRST BOOK                              
         JNE   VALBKS04                                                         
         MVC   0(6,R4),VBWORK                                                   
         MVC   VBSVBKTY,0(R4)      SAVE BOOK TYPE                               
         AHI   R4,L'VBOBOOK1                                                    
         J     VALBKS06                                                         
                                                                                
VALBKS04 CLC   VBSVBKTY,VBWORK     MUST BE SAME BOOK TYPE AS FIRST              
         JNE   EXITN                                                            
         CLI   VBWORK+1,0          SECOND ETC. CAN'T HAVE WEEKS                 
         JNE   EXITN                                                            
         MVC   0(2,R4),VBWORK+4                                                 
         AHI   R4,L'VBOBOOK2                                                    
                                                                                
VALBKS06 AHI   R2,L'VBSCAN         BUMP TO NEXT SCANNER BLOCK ENTRY             
         JCT   R0,VALBKS02         DO FOR NUMBER OF ENTRIES                     
         J     EXITY                                                            
         DROP  R2,RC                                                            
                                                                                
VBWORKD  DSECT                     ** VALMDT LOCAL W/S **                       
VBFLDH   DS    XL8                 DUMMY FIELD HEADER                           
VBFLD    DS    XL240               DUMMY INPUT FIELD                            
VBWORK   DS    XL6                 WORK AREA                                    
VBSVBKTY DS    X                   FIRST BOOK TYPE                              
VBSCAN#  DS    X                   NUMBER OF SCANNER LINES                      
VBSCAN   DS    15XL(SCBLKLQ)       SCANNER OUTPUT                               
VBWORKL  EQU   *-VBWORKD                                                        
                                                                                
VBOUTD   DSECT                     ** BOOKS ARRAY LINE **                       
VBOFILE  DS    CL3                 FILE NAME                                    
VBOBOOKS DS    0X                  ** DEMO LOOKUP BOOKS **                      
VBOBOOK1 DS    XL6                 FIRST BOOK                                   
VBOBOOK2 DS    XL2                 SECOND BOOK                                  
VBOBOOK3 DS    XL2                 THIRD BOOK                                   
VBOBOOK4 DS    XL2                 FOURTH BOOK                                  
VBOBOOKL EQU   *-VBOBOOKS                                                       
VBOPCID  DS    CL5                 PC ID                                        
VBOUPNDX DS    X                   UPGRADE INDEX NUMBER                         
VBOLATBN DS    X                   NUMBER OF LATEST BOOKS                       
VBOLATBT DS    X                   BOOKTYPE FOR LATEST BOOK LOOKUP              
VBOUTL   EQU   *-VBOUTD                                                         
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MULTIPLE COUNTIES                                          *         
***********************************************************************         
                                                                                
         USING VCWORKD,RC                                                       
VALMCNTY LM    R2,R4,LP_AINP                                                    
         XC    VCFLDH,VCFLDH                                                    
         MVI   VCFLDH,L'VCFLD+L'VCFLDH                                          
         MVI   VCFLD,C' '                                                       
         MVC   VCFLD+1(L'VCFLD-1),VCFLD                                         
         STC   R3,VCFLDH+5                                                      
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VCFLD(0),0(R2)                                                   
                                                                                
         MVI   SCANLEN,30          NON STANDARD LENGTH -30 MORE                 
         ICM   R0,15,VMSCANDC                                                   
         GOTOR VSCANNER,DMCB,(SCANLEN,VCFLDH),VCSCAN,(R0)                       
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          R0=NUMBER OF SCANNER LINES                   
         JZ    EXITN                                                            
         CHI   R0,VSOCNTY#                                                      
         JH    EXITN                                                            
         LA    R2,VCSCAN                                                        
         USING SCANBLKD,R2         R2=A(SCANNER LINE)                           
                                                                                
VALMCT02 SR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         OC    SC1STFLD,SPACES                                                  
         TM    SC1STVAL,SCALPHAQ   COUNTY CODE HAS TO BE NUMERIC                
         JZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,R4),SC1STNUM+2  NUMERIC COUNTY CODE                          
*                                                                               
         ZIC   RE,SCANLEN                                                       
         OR    RE,RE               CHECK IF NON STANDARD SCANNER LENGTH         
         JZ    *+8                 WAS SPECIFIED- IF SO ADD FOR LINE+22         
         AHI   RE,22                                                            
         AR    R2,RE                                                            
         AHI   R4,VSOCNTYW                                                      
         JCT   R0,VALMCT02                                                      
******************************************************************              
VALMCTX  MVI   0(R4),FF                                                         
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
VCWORKD  DSECT                     ** VALMDT LOCAL W/S **                       
VCFLDH   DS    XL8                 DUMMY FIELD HEADER                           
VCFLD    DS    XL240               DUMMY INPUT FIELD                            
VCSCAN   DS    15XL(SCBLKLQ)       SCANNER OUTPUT                               
VCWORKL  EQU   *-VCWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND TRANSLATE MULTIPLE STATE CODES                                   
***********************************************************************         
                                                                                
         USING VSTWORKD,RC                                                      
VALMSTAT LM    R2,R4,LP_AINP                                                    
         XC    VSTFLDH,VSTFLDH                                                  
         MVI   VSTFLDH,L'VSTFLD+L'VSTFLDH                                       
         MVI   VSTFLD,C' '                                                      
         MVC   VSTFLD+1(L'VSTFLD-1),VSTFLD                                      
         STC   R3,VSTFLDH+5                                                     
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   VSTFLD(0),0(R2)                                                  
                                                                                
         MVI   SCANLEN,30          NON STANDARD LENGTH -30 MORE                 
         ICM   R0,15,VMSCANDC                                                   
         GOTOR VSCANNER,DMCB,(SCANLEN,VSTFLDH),VSTSCAN,(R0)                     
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          R0=NUMBER OF SCANNER LINES                   
         JZ    EXITN                                                            
         CHI   R0,VSOSTAT#                                                      
         JH    EXITN                                                            
         LA    R2,VSTSCAN                                                       
         USING SCANBLKD,R2         R2=A(SCANNER LINE)                           
                                                                                
VALMST02 SR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         OC    SC1STFLD,SPACES                                                  
* TRANSLATE STATE CODE                                                          
*                                                                               
         LA    RE,STACODE         STATE CODES DEFINED IN DESTACODE              
VALMST10 CLC   =X'FFFF',0(RE)     STATE CODE HAS TO BE IN TABLE                 
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   1(2,RE),SC1STFLD                                                 
         JE    VALMST20                                                         
         AHI   RE,28                                                            
         J     VALMST10                                                         
VALMST20 MVC   0(1,R4),0(RE)                                                    
*                                                                               
*                                                                               
         ZIC   RE,SCANLEN                                                       
         OR    RE,RE               CHECK IF NON STANDARD SCANNER LENGTH         
         JZ    *+8                 WAS SPECIFIED- IF SO ADD FOR LINE+22         
         AHI   RE,22                                                            
         AR    R2,RE                                                            
         AHI   R4,VSOSTATW                                                      
         JCT   R0,VALMST02                                                      
******************************************************************              
VALMSTX  MVI   0(R4),FF                                                         
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
VSTWORKD DSECT                     ** VALMDT LOCAL W/S **                       
VSTFLDH  DS    XL8                 DUMMY FIELD HEADER                           
VSTFLD   DS    XL240               DUMMY INPUT FIELD                            
VSTSCAN  DS    15XL(SCBLKLQ)       SCANNER OUTPUT                               
VSTWORKL EQU   *-VSTWORKD                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
* VALIDATE DAY EXPRESSION                                             *         
***********************************************************************         
                                                                                
VALDAY   LM    R2,R4,LP_AINP                                                    
         GOTOR VDAYVAL,DMCB,((R3),(R2)),(R4),=X'17'                             
         CLI   0(R4),0                                                          
         JE    EXITN                                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* TRANSLATE BOOKTYPE ROUTINE                                          *         
***********************************************************************         
                                                                                
TRNSBKT  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),C'~'          ALLOW SPOT DESKTOP SPECIAL                   
         JNE   *+12                STANDARD BOOKTYPE                            
         MVI   0(R4),C'~'                                                       
         J     EXITY                                                            
         GOTOR (#TRNSBT,ATRNSBT),DMCB,(R2),(R3),(R4),0                          
         CHI   R3,0                                                             
         JE    EXITY                                                            
         CLI   0(R4),X'FF'                                                      
         JE    EXITN                                                            
         AHI   R4,L'VBOLATBT                                                    
         J     EXITY                                                            
***********************************************************************         
* EDIT BOOK WEEK/MONTH                                                *         
***********************************************************************         
                                                                                
EDTBWMLQ EQU   10                  LENGTH OF OUTPUT                             
EDTBWM   LM    R2,R4,LP_AINP                                                    
         MVC   0(EDTBWMLQ,R4),SPACES                                            
         L     RF,ABLTNTRY                                                      
         TM    BLTTINDS-BLTTABD(RF),$VALDWKY                                    
         JNZ   EDTBWM02                                                         
         MVC   WORK(L'BOOKWKMO),0(R2)                                           
         XC    WORK(L'BOOKWKMO),EFFS                                            
         MVI   WORK+L'BOOKWKMO,1                                                
         GOTOR VDATCON,DMCB,(3,WORK),(6,(R4))                                   
         MVC   3(3,R4),4(R4)       REMOVE EMBEDDED SLASH                        
         J     EXITY                                                            
                                                                                
EDTBWM02 SR    R0,R0                                                            
         CLI   DBSELMED,MEDON      TEST OVERNIGHTS                              
         JNE   *+8                                                              
         LHI   R0,1                                                             
         GOTOR VNSIWEEK,DMCB,(C'D',(R2)),((R0),VGETDAY),VADDAY,VDATCON          
         ICM   R1,7,1(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(6),0(R1)                                                    
         GOTOR VDATCON,DMCB,(0,WORK),(5,(R4))                                   
         J     EXITY                                                            
                                                                                
***********************************************************************         
* EDIT BOOK MONTH                                                     *         
***********************************************************************         
                                                                                
EDTBMOLQ EQU   5                   LENGTH OF OUTPUT                             
EDTBMO   LM    R2,R4,LP_AINP                                                    
         MVC   0(EDTBMOLQ,R4),SPACES                                            
         MVC   WORK(L'BOOKWKMO),0(R2)                                           
         XC    WORK(L'BOOKWKMO),EFFS                                            
         MVI   WORK+L'BOOKWKMO,1                                                
         GOTOR VDATCON,DMCB,(3,WORK),(6,(R4))                                   
         MVC   3(3,R4),4(R4)       REMOVE EMBEDDED SLASH                        
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO TSAR (ONLINE) OR BUFFERIN (OFFLINE)                    *         
***********************************************************************         
                                                                                
         USING BUFFD,FLMBUF                                                     
BUFFER   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         MVC   TSACTN,0(R2)                                                     
                                                                                
         CLI   TSACTN,TSAINI       TEST INITIALIZATION CALL                     
         JNE   BUFFER02                                                         
         XC    TSARBLK,TSARBLK     CLEAR TSAR BLOCK                             
         MVI   TSACTN,TSAINI                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGN,8                                                         
         OI    TSINDS,TSINODSK+TSIXTTWA                                         
         MVC   TSKEYL,4(R2)                                                     
         MVC   TSRECL,6(R2)                                                     
                                                                                
BUFFER02 TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   BUFFER04            YES                                          
         GOTOR VTSAR,TSARD         ELSE USE TSAR                                
         J     EXITCC                                                           
                                                                                
BUFFER04 LHI   R0,BUFFAINI         CONVERT TSAR ACTION CODE TO BUFFERIN         
         CLI   TSACTN,TSAINI                                                    
         JNE   BUFFER06                                                         
         SR    RE,RE                                                            
         IC    RE,TSKEYL                                                        
         SR    RF,RF                                                            
         ICM   RF,3,TSRECL                                                      
         SR    RF,RE                                                            
         STCM  RE,3,BUFFLKEY       SET KEY LENGTH                               
         STCM  RF,3,BUFFLCOM       SET COMMENT LENGTH                           
         J     BUFFER08                                                         
                                                                                
BUFFER06 LHI   R0,BUFFAPUT                                                      
         CLI   TSACTN,TSAADD                                                    
         JE    BUFFER08                                                         
         CLI   TSACTN,TSAWRT                                                    
         JE    BUFFER08                                                         
         LHI   R0,BUFFASEQ                                                      
         CLI   TSACTN,TSANXT                                                    
         JE    BUFFER08                                                         
         MVC   TSARSAV,TSARREC                                                  
         LHI   R0,BUFFARDH                                                      
         CLI   TSACTN,TSARDH                                                    
         JE    BUFFER08                                                         
         DC    H'0'                                                             
                                                                                
BUFFER08 GOTOR ABUFFRIN,DMCB,((R0),BUFFD),TSARREC,ACOMFACS                      
         CLI   TSACTN,TSARDH                                                    
         JNE   BUFFER10                                                         
         SR    RF,RF                                                            
         IC    RF,TSKEYL                                                        
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JE    BUFFER10                                                         
         CLC   TSARREC(0),TSARSAV                                               
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
                                                                                
BUFFER10 MVC   TSERRS,BUFFERRS-BUFFPARM(R1)                                     
         CLI   TSERRS,0                                                         
         J     EXITCC                                                           
         EJECT                                                                  
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
FILTP    DC    C'TP '              TIME PERIOD FILE                             
FILRTP   DC    C'RTP'              RADIO TIME PERIOD FILE                       
MEDTP    EQU   C'T'                TIME PERIOD                                  
MEDWK    EQU   C'W'                WEEKLY                                       
MEDRAD   EQU   C'R'                RADIO                                        
MEDON    EQU   C'O'                OVERNIGHT                                    
MEDTDP   EQU   C'D'                TV DAYPARTS                                  
MEDCTY   EQU   C'U'                COUNTY COVERAGE                              
SRCNSI   EQU   C'N'                NIELSON                                      
SRCARB   EQU   C'A'                ARBITRON/BBM                                 
SRCTRI   EQU   C'T'                TRITON                                       
SRCNTI   EQU   SRCNSI              NTI FOR CANADA                               
SRCBBM   EQU   SRCARB              BBM FOR CANADA                               
SRCFUS   EQU   C'F'                FUSION                                       
SRCVID   EQU   C'V'                VIDEOLOGY                                    
SRCIHT   EQU   C'H'                IHEART                                       
SRCNCM   EQU   C'C'                NCM                                          
EOT      EQU   0                   END OF TABLE INDICATOR                       
                                                                                
         LTORG                                                                  
                                                                                
VMSCANDC DS    0C                                                               
VBSCANDC DC    C',=+',X'00'                                                     
                                                                                
*ARB RADIO BOOKTYPE TABLE WHICH HAS DIFFERENT MKT NUMBERS                       
*THAN THE STANDARD BOOK                                                         
*                                                                               
ARBMKTAB DS    0CL1                                                             
         DC    C'H',C'B',C'C'                                                   
         DC    X'FF'                                                            
*                                                                               
** NOTE- 02/26/15 - I FOUND THAT THE FILE SHOULD BE IN ALPHABETIC               
* ORDER IN TABLE ELSE THE CODE WILL NOT FIND THE NEXT FILE FOR                  
* MULTIPLE FILE BOOK LOOKUPS IF THE NEXT FILE IS GREATER THAN PREVIOUS          
* TABLE OF NSI NON CABLE BOOKTYPE AND CORRESPONDING CABLE BOOKTYPES             
* BOOKTY ES DEFINED AS 2 CHARACTER FORMAT FOR BOOK LIST DOWNLOAD                
* DSECT= BKTYTBD                                                                
CBTYPTAB DS    0XL10                                                            
CBTYSTND DC    X'0000',C'W ',C'C ',C'Z ',C'U '      STANDARD - CABLES           
         DC    C'H ',X'FFFF',C'H ',X'FFFF',X'FFFF'  HISPANIC = CABLES           
         DC    X'FF'                                                            
BLTTAB   DS    0X                  ** BOOK TYPE TABLE **                        
         DC    C'TT ',AL1(MEDTP),AL1(SRCNSI),AL1($MKTBLST)                      
         DC    C'TF ',AL1(MEDTP),AL1(SRCFUS),AL1($MKTBLST)                      
* tf was commented out                                                          
                                                                                
******   DC    C'T4 ',AL1(MEDTP),AL1(SRCNSI),AL1(0)                             
         DC    C'WTP',AL1(MEDWK),AL1(SRCNSI),AL1($VALDWKY)                      
         DC    C'OTP',AL1(MEDON),AL1(SRCNSI),AL1($VALDWKY+$VALDDMY)             
         DC    C'RDH',AL1(MEDRAD),AL1(SRCIHT),AL1($MKTBLST)                     
         DC    C'RTH',AL1(MEDRAD),AL1(SRCIHT),AL1($MKTBLST)                     
         DC    C'RTP',AL1(MEDRAD),AL1(SRCARB),AL1($MKTBLST)                     
         DC    C'RDP',AL1(MEDRAD),AL1(SRCARB),AL1($MKTBLST)                     
         DC    C'TDP',AL1(MEDTDP),AL1(SRCNSI),AL1($MKTBLST)                     
         DC    C'RUA',AL1(MEDCTY),AL1(SRCARB),AL1($MKTBLST)                     
         DC    C'TRT',AL1(MEDRAD),AL1(SRCTRI),AL1($MKTBLST)                     
         DC    C'TRD',AL1(MEDRAD),AL1(SRCTRI),AL1($MKTBLST)                     
         DC    C'TVD',AL1(MEDTP),AL1(SRCVID),AL1($MKTBLST)                      
         DC    C'XT ',AL1(MEDTP),AL1(SRCNSI),AL1($MKTBLST)                      
         DC    C'NCM',AL1(MEDTP),AL1(SRCNCM),AL1($MKTBLST)                      
**       DC    C'RTH',AL1(MEDRAD),AL1(SRCIHT),AL1($MKTBLST)                     
**       DC    C'RDH',AL1(MEDRAD),AL1(SRCIHT),AL1($MKTBLST)                     
BLTTABN  EQU   (*-BLTTAB)/BLTTABL                                               
BLTTABX  DC    AL1(EOT)                                                         
                                                                                
CBCUTOFF DC    AL1(111,01)         EFFECT JAN/11 ALLOW CABLE BKTYPES            
BTCUTOFF DC    AL1(90,07)          KILL BOOKTYPE PRIOR TO JUL90                 
BKCUTOFF DC    AL1(99,10)          CUTOFF  BOOK PRIOR TO OCT99                  
OVCUTOFF DC    AL1(104,50)         OVERNIGHT AFTER DEC0604                      
                                                                                
BLTTABD  DSECT                     ** DSECT TO COVER BOOK TYPE TABLE **         
BLTTFILE DS    CL3                 FILE IDENTIFIER                              
BLTTMEDC DS    C                   MEDIA CODE                                   
BLTTSRCC DS    C                   SOURCE CODE                                  
BLTTINDS DS    X                   INDICATORS                                   
$MKTBLST EQU   X'20'               SEND BOOK LIST BY MARKET FOR FILE            
BLTTABL  EQU   *-BLTTABD                                                        
*                                                                               
CBKTYTBD DSECT                                                                  
CBKTSTND DS    CL2                 STANDARD NON CABLE BOOKTYPE                  
CBKTWIRE DS    CL2                 WIRED CABLE BOOKTYPE X'FF'=N/A               
CBKTDMAC DS    CL2                 DMA CABLE BOOKTPYE   X'FF'=N/A               
CBKTLVOW DS    CL2                 LIVE ONLY HARDWIRED                          
CBKTLVOC DS    CL2                 LIVE ONLY DMA CABLE                          
CBKTYTBL EQU   *-CBKTYTBD                                                       
SVRDEF   CSECT                                                                  
*                                                                               
FLMBUF   BUFFD TYPE=D,KEYLEN=0,COMLEN=0,BUFFERS=10                              
                                                                                
FILES    DS    0X                  DUMMY FILES POINTER (FOR MULTI BIT)          
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR BOOK LIST DOWNLOAD - X'0009'                        *         
***********************************************************************         
                                                                                
REQBKL   LKREQ H,I#DEBOKL,OUTBKL                                                
                                                                                
SCall    LKREQ F,1,(I,B#SAVED,STAIND),CHAR,COL=*,LIST=F,OLEN=STAL,     *        
               MAXLEN=STAL,TEXT=SP#STA,DELIM=COMMA                              
**BType    LKREQ F,2,(D,B#SAVED,BTYPE),CHAR,COL=*,TEXT=SP#BKTY                  
BType    LKREQ F,2,(D,B#SAVED,BTYPE),(R,TRNSBKT),COL=*,TEXT=SP#BKTY,   *        
               OLEN=L'VBOLATBT                                                  
Mrkt     LKREQ F,3,(D,B#SAVED,MARKET),CHAR,COL=*,TEXT=SP#MKT                    
File     LKREQ F,4,(I,B#SAVED,FILIND),CHAR,COL=*,LIST=F,OLEN=L'FILEID, +        
               MAXLEN=L'FILEID,TEXT=SP#1BFIL,DELIM=COMMA                        
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR COUNTY LIST DOWNLOAD - X'000B'                      *         
***********************************************************************         
                                                                                
REQCTYL  LKREQ H,I#DECTYL,OUTCTYL                                               
                                                                                
STATECDE LKREQ F,1,(D,B#SAVED,STATE),(R,VALMSTAT),COL=*,               *        
               TEXT=(*,STATELIT),OLEN=L'STATE                                   
CTYLBK   LKREQ F,2,(D,B#SAVED,BOOK),(R,VALBKS),OLEN=L'BOOK,            *        
               TEXT=SP#1BBOK,COL=*                                              
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR COMPARAGRAPH REPORT DOWNLOAD - X'0014'              *         
***********************************************************************         
                                                                                
REQCGR   LKREQ H,I#DEPRGD,OUTDUM                                                
                                                                                
Demos    LKREQ F,1,(I,B#SAVED,SAVED),(R,VALDEM),LIST=F,OLEN=VDOUTL,    *        
               SORT=N,TEXT=SP#DEMO,COL=*                                        
                                                                                
DemInd   LKREQ F,19,(I,B#SAVED,SAVED),(R,VALDMIND),ARRAY=S,SORT=NO,    *        
               OLEN=L'VDIODMIN,TEXT=SP#RATIS,COL=*                              
Demos2   LKREQ F,20,,(R,VALRDEM),ARRAY=E,OLEN=VDOUTL2,                 *        
               TEXT=SP#DEMO,COL=*                                               
                                                                                
SCall    LKREQ F,2,(I,B#SAVED,SAVED),(R,VALSTA),ARRAY=S,OLEN=VSOSTAL,  *        
               SORT=N,TEXT=SP#STA,COL=*                                         
States   LKREQ F,21,,(R,VALMSTAT),OLEN=VSOSTATL,TEXT=(*,STATELIT),COL=*         
Counties LKREQ F,22,,(R,VALMCNTY),OLEN=VSOCNTYL,TEXT=(*,CNTYLIT),COL=*          
DayTm    LKREQ F,3,,(R,VALMDT),OLEN=VSODTML,TEXT=SP#1BDYT,COL=*                 
BuyStEnd LKREQ F,8,,(R,VALDAT),OLEN=L'VSOBUYD,TEXT=(*,BUYDLIT),COL=*            
LPMStart LKREQ F,9,,(R,VALDAT),OLEN=L'VSOLPMSD,TEXT=(*,LPMSDLIT),COL=*          
SysCd    LKREQ F,15,,UBIN,OLEN=L'VSOSYSC,TEXT=SP#SYSC,COL=*                     
#Rows    LKREQ F,4,,UBIN,ARRAY=E,OLEN=L'VSOROWS,TEXT=SP#NROWS,COL=*             
                                                                                
DFile    LKREQ F,5,(I,B#SAVED,SAVED),(R,VALFIL),ARRAY=S,               *        
               OLEN=L'VBOFILE,SORT=N,TEXT=SP#1BFIL,COL=*                        
Books    LKREQ F,6,,(R,VALBKS),OLEN=VBOBOOKL,TEXT=SP#1BBOK,COL=*                
PCID     LKREQ F,7,,CHAR,OLEN=L'VBOPCID,TEXT=SP#KEY,COL=*                       
UpNdx    LKREQ F,10,,UBIN,OLEN=L'VBOUPNDX,TEXT=SP#NUM,COL=*                     
LatBn    LKREQ F,16,,UBIN,OLEN=L'VBOLATBN,TEXT=(*,LBKNLIT),COL=*                
LatBt    LKREQ F,17,(I,B#SAVED,SAVED),(R,TRNSBKT),ARRAY=E,             *        
               OLEN=L'VBOLATBT,TEXT=(*,LBKTLIT),COL=*                           
**LATBT    LKREQ F,17,,CHAR,ARRAY=E,OLEN=L'VBOLATBT,TEXT=(*,LBKTLIT),           
**               COL=*                                                          
                                                                                
UpGrd    LKREQ F,11,(I,B#SAVED,SAVED),(U,#VALUPG,$VALUPG),             *        
               ARRAY=S,SORT=N,OLEN=VUFORML,TEXT=SP#UPGR,COL=*                   
UpTxt    LKREQ F,12,,CHAR,OLEN=15,TEXT=SP#COMM,COL=*                            
UpNdx    LKREQ F,13,,UBIN,ARRAY=E,OLEN=1,TEXT=SP#NUM,COL=*                      
                                                                                
DPrec    LKREQ F,14,(I,B#SAVED,SAVED),CHAR,OLEN=1,TEXT=SP#DPREC,COL=*           
DefBkt   LKREQ F,18,(I,B#SAVED,SAVED),(R,TRNSBKT),                     *        
               OLEN=L'VBOLATBT,TEXT=(*,DBKTLIT),COL=*                           
RhomeMKT LKREQ F,23,(I,B#SAVED,SAVED),CHAR,                            *        
               OLEN=3,TEXT=(*,HMKTLIT),COL=*                                    
*                                                                               
DImpPrec LKREQ F,24,(I,B#SAVED,SAVED),CHAR,OLEN=1,TEXT=SP#DPREC,COL=*           
PrsAgy   LKREQ F,25,(I,B#SAVED,SAVED),CHAR,OLEN=2,TEXT=SP#AGY,COL=*             
PsmShrF  LKREQ F,27,(I,B#SAVED,SAVED),CHAR,OLEN=1,TEXT=(*,PGMSHARE),   *        
               COL=*                                                            
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR CANADIAN DEMO LOOK-UPS - X'0018'                    *         
***********************************************************************         
                                                                                
REQCDL   LKREQ H,I#DECAND,OUTDUM                                                
                                                                                
Media    LKREQ F,1,(D,B#WORKD,WORKD),CHAR,TEXT=SP#MED,COL=*                     
                                                                                
RtgSv    LKREQ F,2,(I,B#SAVED,SAVED),CHAR,OLEN=1,TEXT=(*,RTGSLIT),     *        
               COL=*,ARRAY=S,SORT=N                                             
SCall    LKREQ F,3,,CHAR,OLEN=5,TEXT=SP#STA,COL=*                               
SMkts    LKREQ F,4,,CHAR,OLEN=3,ENTRIES=17,ECOUNT=Y,TEXT=(*,SMKTLIT)            
LUDay    LKREQ F,5,,(R,VALDAY),OLEN=1,TEXT=SP#DAY,COL=*                         
STime    LKREQ F,6,,LBIN,OLEN=2,TEXT=(*,STIMLIT),COL=*                          
ETime    LKREQ F,7,,LBIN,OLEN=2,TEXT=(*,ETIMLIT),COL=*                          
RBook    LKREQ F,8,,BMON,OLEN=2,TEXT=SP#BOOK,COL=*,ARRAY=E                      
                                                                                
2Decs    LKREQ F,9,(D,B#WORKD,WORKD),CHAR,OLEN=1,TEXT=(*,PRECLIT),COL=*         
1WPrf    LKREQ F,10,(D,B#WORKD,WORKD),CHAR,OLEN=16,TEXT=(*,PROFLIT),   *        
               COL=*                                                            
                                                                                
Demos    LKREQ F,11,(I,B#WORKD,WORKD),(U,#VALDCD,$VALDCD),LIST=F,      *        
               OLEN=3,TEXT=SP#1BDEM,COL=*                                       
                                                                                
         LKREQ E                                                                
                                                                                
OUTDUM   DS    0X                  DUMMY OUTPUT                                 
                                                                                
***********************************************************************         
* REQUEST MAP FOR CANADIAN DESKTOP STATION BOOK LIST - X'020E'        *         
***********************************************************************         
                                                                                
REQSBL   LKREQ H,I#DECSBL,OUTSBL                                                
                                                                                
Media    LKREQ F,1,(D,B#SAVED,MEDIA),CHAR,TEXT=(*,MEDCLIT),COL=*                
RtgSv    LKREQ F,2,(D,B#SAVED,RTGSVC),CHAR,TEXT=(*,RTGSLIT),COL=*               
SCall    LKREQ F,3,(D,B#SAVED,STATION),CHAR,TEXT=SP#STA,COL=*                   
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR ONLINE DEMO LOOKUPS - X'FD01'                       *         
***********************************************************************         
                                                                                
REQDLU   LKREQ H,I#DEMOLU,OUTDLU                                                
                                                                                
Media    LKREQ F,1,(D,B#SAVED,GDEMBLK+(GDMEDX-GDEMBLKD)),              *        
               (U,#VALMED,$VALMED),OLEN=L'GDMEDX,MAXLEN=L'GDMEDA,      *        
               TEXT=SP#MED,COL=*                                                
CltCd    LKREQ F,2,(D,B#SAVED,GDEMBLK+(GDCLTX-GDEMBLKD)),              *        
               (R,VALCLT),OLEN=L'GDCLTX,MAXLEN=L'GDCLTA,               *        
               TEXT=SP#CLI,COL=*                                                
EstNo    LKREQ F,3,(D,B#SAVED,GDEMBLK+(GDESTX-GDEMBLKD)),              *        
               LBIN,OLEN=L'GDESTX,TEXT=SP#EST,COL=*                             
StaCd    LKREQ F,4,(D,B#SAVED,GDEMBLK+(GDSTAX-GDEMBLKD)),              *        
               (U,#VALSTA,$VALSTA),OLEN=L'GDSTAX,TEXT=SP#STA,COL=*              
RBook    LKREQ F,6,(D,B#SAVED,GDEMBLK+(GDBOOK-GDEMBLKD)),              *        
               BMON,OLEN=L'GDBOOK,TEXT=SP#BOOK,COL=*                            
DayTm    LKREQ F,7,(D,B#SAVED,GDEMBLK+(GDDAYTIM-GDEMBLKD)),            *        
               (U,#VALDTM,$VALDTM),OLEN=L'GDDAYTIM,TEXT=SP#1BDYT,COL=*          
Upgrd    LKREQ F,8,(D,B#SAVED,GDEMBLK+(GDUPG-GDEMBLKD)),               *        
               (U,#VALUPG,$VALUPG),OLEN=L'GDUPG,TEXT=SP#UPGR,COL=*              
Demos    LKREQ F,9,(I,B#SAVED,GDEMBLK+(GDADEM-GDEMBLKD)),              *        
               (R,VALDEM),OLEN=3,LIST=F,SORT=N,TEXT=SP#DEMO,COL=*               
                                                                                
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR VALIDATING PROJECTION FORMULAS - X'000D'            *         
***********************************************************************         
                                                                                
REQVPJ   LKREQ H,I#DEUPGV,OUTPRJ                                                
                                                                                
PJSTRING LKREQ F,1,(I,B#SAVED,PROJIND),CHAR,ARRAY=S,SORT=N,            *        
               TEXT=SP#UPGR,COL=*,OLEN=PROJL,MAXLEN=PROJL                       
PJID     LKREQ F,2,(I,B#SAVED,PJIDIND),CHAR,COL=*,TEXT=(*,PJIDLIT),    *        
               ARRAY=E,OLEN=L'PROJID                                            
                                                                                
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
OUTBKL   LKOUT H                   ** BOOK LIST **                              
                                                                                
BKLREC   LKOUT R,12                                                             
Array    LKOUT C,12,(A,ARYBKL)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTDLU   LKOUT H                   ** DEMO LOOKUP (ONLINE) **                   
                                                                                
DLUREC   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYDLU)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTSBL   LKOUT H                   ** CANADIAN DESKTOP BOOK LIST **             
                                                                                
SDLREC   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYSBL)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTPRJ   LKOUT H                   ** VALIDATED PROJECTIONS LIST **             
                                                                                
PRJREC   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYPROJ)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTCTYL  LKOUT H                   ** COUNTY LIST**                             
                                                                                
CTYREC   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYCTYL)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
***********************************************************************         
* ARRAY ENTRY FOR BOOK LIST (HEADER)                                  *         
***********************************************************************         
                                                                                
ARYBKL   LKOUT A,(R,NXTBLH),MULTIROW=Y                                          
                                                                                
FilNm    LKOUT C,12,(D,B#SAVED,BOOKFILE),CHAR                                   
Array    LKOUT C,13,(A,ARYBKD)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY ENTRY FOR BOOK LIST (DETAIL)                                  *         
***********************************************************************         
                                                                                
ARYBKD   LKOUT A,(R,NXTSTA),MULTIROW=Y                                          
Array    LKOUT C,13,(A,ARYBKDS)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY ENTRY FOR BOOK LIST BY STATION (DETAIL)                       *         
***********************************************************************         
                                                                                
ARYBKDS  LKOUT A,(R,NXTBLD),MULTIROW=Y,ROWNAME=BOOKREC                          
                                                                                
BType    LKOUT C,1,(D,,BOOKTYPE),CHAR,ND=Y                                      
RBook    LKOUT C,2,(D,,BOOKWKMO),(R,EDTBWM),LEN=EDTBWMLQ                        
Statn    LKOUT C,3,(D,B#SAVED,STATN),CHAR,ND=Y,PCVERSION=3.0.0.31               
Bookind  LKOUT C,4,(D,,BOOKINDF),CHAR,ND=Y,PCVERSION=4.0.0.00                   
StatFLg  LKOUT C,5,(D,,BKINDFLG),CHAR,ND=Y,PCVERSION=9.0.0.00                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY ENTRY FOR CANADIAN DESKTOP STATION BOOK LIST                  *         
***********************************************************************         
                                                                                
ARYSBL   LKOUT A,(R,NXTSBL),MULTIROW=Y,ROWNAME=BOOKREC                          
                                                                                
RBook    LKOUT C,1,BOOKWKMO,(R,EDTBMO),LEN=EDTBMOLQ                             
MktAl    LKOUT C,2,BOOKMKTA,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY ENTRY FOR DEMO LOOKUP                                         *         
***********************************************************************         
                                                                                
ARYDLU   LKOUT A,(R,NXTDLU),MULTIROW=Y,ROWNAME=GDEMBLKD                         
                                                                                
ABook    LKOUT C,1,(D,,GDABOOK),BMON                                            
AProg    LKOUT C,2,(D,,GDAPROG),CHAR                                            
                                                                                
         LKOUT E                                                                
***********************************************************************         
* ARRAY ENTRY FOR PROJECTIONS VALIDATION                              *         
***********************************************************************         
                                                                                
ARYPROJ  LKOUT A,(R,NXTPRJ),MULTIROW=Y,ROWNAME=PROJREC                          
                                                                                
PJNUM    LKOUT C,1,(D,,PROJID),CHAR,ND=Y                                        
PJFORM   LKOUT C,2,(D,,PROJFORM),CHAR,ND=Y                                      
PJVFLAG  LKOUT C,3,(D,,PROJVFLG),CHAR,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY ENTRY FOR COUNTY LIST (HEADER)                                *         
***********************************************************************         
                                                                                
ARYCTYL  LKOUT A,(R,NXTCNTY),MULTIROW=Y,ROWNAME=CNTYREC                         
                                                                                
CtyNm    LKOUT C,1,(D,,COUNTYNM),CHAR                                           
CTYCDE   LKOUT C,2,(D,,COUNTYCD),UBIN                                           
Tier     LKOUT C,3,(D,,TIER),UBIN                                               
                                                                                
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
         LKARY T                                                                
         EJECT                                                                  
CTRYLIT  DC    C'Country (C=Canada, U=USA)'                                     
RTGSLIT  DC    C'Rating service (0, 1)'                                         
SMKTLIT  DC    C'Spill market alpha(s)'                                         
STIMLIT  DC    C'Military start time'                                           
ETIMLIT  DC    C'Military end time'                                             
PRECLIT  DC    C'2 decimal places?'                                             
PROFLIT  DC    C'1W profile'                                                    
MEDCLIT  DC    C'Media code'                                                    
LBKNLIT  DC    C'NUMBER OF LATEST BOOKS'                                        
LBKTLIT  DC    C'Booktype for latest books'                                     
DBKTLIT  DC    C'Default Booktype'                                              
PJIDLIT  DC    C'Projection ID Number'                                          
BUYDLIT  DC    C'BUY START-END DATE'                                            
LPMSDLIT DC    C'LPM START DATE'                                                
STATELIT DC    C'STATE'                                                         
CNTYLIT  DC    C'Counties'                                                      
HMKTLIT  DC    C'Home MKT'                                                      
PGMSHARE DC    C'Program Share Flag'                                            
                                                                                
       ++INCLUDE DESTACODE                                                      
SAVED    DSECT                     ** SAVED WORKING STORAGE **                  
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
ABUFFRIN DS    0A                  SET IF RUNNING OFFLINE                       
                                                                                
MEDIA    DS    CL(L'DBSELMED)      MEDIA CODE                                   
RTGSVC   DS    C                   RATING SERVICE (0=NSI, 1=BBM)                
STATION  DS    CL(L'DBSELSTA)      STATION CALL LETTERS                         
BTYPE    DS    C                   BOOK TYPE FILTER                             
STATE    DS    X                                                                
BOOK     DS    XL(L'VBOBOOK1)                                                   
STATN    DS    CL(STAL)            STATION CALL LETTERS (INCL SPILL)            
STACOUNT DS    HL2                 STATION COUNT                                
MARKET   DS    CL(L'DBSELALF)      ALPHA MARKET                                 
SVMKALF  DS    CL(L'DBSELALF)      ALPHA MARKET                                 
FILEID   DS    CL(L'BLTTFILE)      FILE ID FILTER                               
PRJCOUNT DS    HL2                 PROJECTION COUNT                             
SCANLEN  DS    X                   SCANNER NON STANDARD LENGTH                  
                                                                                
LOCFLAG  DS    X                                                                
LOC_FIRST_TIME     EQU X'01'                                                    
LOC_NOT_FIRST_TIME EQU X'10'                                                    
LOC_GET_FUS_BK     EQU X'80'                                                    
ALOCNTRY DS    A                                                                
ABLTNTRY DS    A                   A(NEXT BLTTAB ENTRY TO PROCESS)              
ASTANTRY DS    A                   A(NEXT STATION ENTRY TO PROCESS)             
APRJNTRY DS    A                   A(NEXT PROJECTION ENTRY TO PROCESS)          
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUE POINTERS **                 
STAIND   DS    X                   STATIONS INDICATOR                           
ASTA     DS    AL3                 A(STATIONS)                                  
PROJIND  DS    X                   PROJECTIONS INDICATOR                        
APROJ    DS    AL3                 A(PROJECTIONS)                               
PJIDIND  DS    X                   PROJECTIONS ID NUMBER INDICATOR              
APROJID  DS    AL3                 A(PROJECTIONS ID)                            
BOOKIND  DS    X                   BOOK INDICATOR                               
ABOOK    DS    AL3                 A(BOOKS)                                     
FILIND   DS    X                   DEMO FILES INDICATOR                         
AFILES   DS    AL3                 A(DEMO FILES)                                
*                                                                               
REQVALSL EQU   *-REQVALS                                                        
SPVER70  EQU   X'07000000'         SPOT DESKTOP 7.0                             
SPVER90  EQU   X'09000000'         SPOT DESKTOP 9.0                             
VERSNUM  DS    XL4                                                              
                                                                                
STAL     EQU   14                  LENGTH OF STATION(INCLUDING SPILL)           
PROJL    EQU   40                  LENGTH OF PROJECTION                         
FILEL    EQU   11                  LENGTH OF FILES                              
COMMA    EQU   C','                COMMA SEPARATOR                              
TEMPBRO  DS    CL12                WORKING STORAGE FOR GETBROAD                 
                                                                                
BOOKFILE DS    CL(L'BLTTFILE)      BOOK ID                                      
FILEINDS DS    X                   FILE TYPE INDICATORS                         
                                                                                
PROJREC  DS    0X                                                               
PROJID   DS    C                   PROJECTION ID NUMBER                         
PROJFORM DS    CL40                PROJECTION FORMULA                           
PROJVFLG DS    C                   Y/N   VALID PROJECTION                       
PROJRECL EQU   *-PROJREC                                                        
                                                                                
TSARREC  DS    XL64                TSAR RECORD                                  
         ORG   TSARREC                                                          
CNTYREC  DS    0X                 COUNTY OUTPUT RECORD BUFFER                   
COUNTYNM DS    CL30                                                             
COUNTYCD DS    XL(L'CYCOUNTY)                                                   
TIER     DS    XL1                                                              
CNTYRECL EQU   *-CNTYREC                                                        
         ORG   TSARREC                                                          
                                                                                
BOOKREC  DS    0X                  ** BOOK LIST TSAR RECORD **                  
BOOKTYPE DS    CL(L'SBBTYP+1)        BOOK TYPE                                  
**       DS    CL1                 EXTRA BYTE FOR 2 CHAR BOOKTYPES              
BOOKWKMO DS    XL(L'SBBOOK)        BOOK WEEK/MONTH                              
BOOKMKTA DS    CL(L'DBSELALF)      ALPHA MARKET CODE (CANADA)                   
BOOKKEYL EQU   *-BOOKREC                                                        
BOOKINDF DS    C                   R=RESEARCH ONLY, A=ALL                       
*                                                                               
BKINDFLG DS    0CL6                BOOK INDICATORS                              
BKNETWKF DS    C                   STANDARD NETWORK FLAG                        
* THE REST PASS DOWN BOOKTYPES  FOR CABLE STREAMS                               
* THE CONCEPT IS FOR EACH CABLE BOOKTYPE TO INDICATE THE STREAM                 
* LIKE WIRE , LIVE ONLY, LIVE +7                                                
BKNCBLF  DS    0CL4                NSI CABLE FLAGS                              
BKNCBLW  DS    C                   NSI WIRED CABLE FLAG                         
BKNCBLC  DS    C                   NSI DMA CABLE FLAG                           
BKNCBLLW DS    C                   NSI LIVE ONLY WIRED CABLE                    
BKNCBLLC DS    C                   NSI LIVE ONLY DMA CABLE                      
BKFCBLF  DS    C                   FUSION CABLE FLAG                            
**SVBKTY DS    CL2                 SAVE BOOKTYPE                                
                                                                                
BOOKRECL EQU   *-BOOKREC                                                        
         ORG                                                                    
                                                                                
GDEMBLK  DS    XL(GDEMBLKL)                                                     
                                                                                
* DEMO BLOCK HERE                                                               
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
DBLOCKL  EQU   *-DBLOCK                                                         
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPLNKWRK                                                       
                                                                                
WORKD    DSECT                     ** REDEFINE OVERWORK **                      
         ORG   OVERWORK                                                         
TSARBLK  DS    XL(TSARDL)          TSAR CONTROL BLOCK                           
TSARSAV  DS    XL(L'TSARREC)       TSAR SAVE RECORD AREA                        
         ORG                                                                    
                                                                                
* INCLUDED BOOKS FOLLOW                                                         
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE SPGENANMK                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SPLNK17   10/28/20'                                      
         END                                                                    
