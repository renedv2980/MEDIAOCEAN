*          DATA SET SPGETDEMF  AT LEVEL 158 AS OF 02/18/21                      
*PHASE T00A21C                                                                  
*INCLUDE NSIWEEK     ---   NEEDED FOR METERED MKT WKLY                          
*INCLUDE NETWEEK     ---   NEEDED FOR NETWORK CABLE                             
         SPACE 1                                                                
*======================================================================         
*         SPGETDEMF - DEMO LOOK-UP PARAMETER BLOCK INTERFACE          *         
*---------------------------------------------------------------------*         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-31705  01/26/21 OPTIMIZE CANADIAN NEW POSTING METHODOLOGY *         
* AKAT SPEC-43482  10/20/20 SUPPORT NEW UNIVERSE KEYWORDS             *         
* BPOO SPEC-20449  04/27/18 RADIO MBK USE 1ST BOOKS UNIVERSE          *         
* AKAT SPEC-20534  02/20/18 BYPASS NIELSEN AFFIDS FOR COMSCORE PASS 1 *         
* AKAT SPEC-13149  05/24/17 SET PARENT+ STATION FOR COMSCORE          *         
***********************************************************************         
* MOD LOG:                                                            *         
* --------                                                            *         
* THIS VERISON FIXES HHSHARES!!!                                      *         
* ???????  (???) --- HISTORY LOST                                     *         
*                                                                     *         
* 23MAR92  (EFJ) --- FLAGS BYTE ADDED                                 *         
*                --- TEST FOR CANADIAN HPT'S INSTEAD OF JUST MED=C    *         
* 04AUG93  (TS)  --- ADD LQH OPTION FOR OM                            *         
* 27OCT93  (TS)  --- FORCE BUYS WITH SPOTS > 26DEC93 TO NSI           *         
* 30MAR94  (RZ)  --- PASS THROUGH SPORTS OPTION                       *         
* 25JUL94  (GLEE) -- SUPPORT RADIO TIME-PERIOD LOOKUP                 *         
* 19AUG94  (GLEE) -- FIXED BUG W/ RADIO AFFIDAVIT LOOKUP              *         
* 10NOV94  (GLEE) -- CREATE EXTENDED DBLOCK FOR RADIO                 *         
* 13JAN95  (GLEE) -- CLEAR DBSELALF FIELD BECAUSE NO ALPHA MKT        *         
* 13FEB95  (GLEE) -- SUPPORT ALPHA MKT INPUT (NEW SPLKALF FIELD)      *         
* 20MAR96  (GLEE) -- TURN ON CANADIEN FLAG FOR NSI ON OR AFTER '96    *         
* 15NOV96  (RZ)   -- OLYMPIC EXCLUSION OPTION                         *         
* 26FEB97  (RZ)   -- ZAP IMP BASED SHARE CALCS - SO WE CAN CARRY      *         
*                 -- PAV BOOK SHARES BUT NOT TPT                      *         
* 16DEC98  (RZ)   -- METERED MARKET WEEKLY VPH                        *         
* 19AUG02  (MH)   -- USE CORERES SPGETIUN/DEFINE                      *         
* 15OCT03  (MH)   -- READ STA AND MKT RECORDS FOR SOFT CANADA OVRDS   *         
* 10FEB04  (RZ)   -- WTP END OF YEAR FIX FOR 2003                     *         
* 15JUL04  (EJ)   -- LPM SUPPORT                                      *         
* 16SEP04  (MH)   -- 2-DECIMAL DEMO SUPPORT                           *         
* 22SEP04  (AK)   -- HONOR LPM=NO OPTION FROM SPOT WRITER             *         
* 26OCT04  (BPOO) -- SUPPORT FUSION AND TP OVERNIGHTS                 *         
* 25JAN05  (AK)   -- LPM SUPPORT FOR AFFIDAVIT                        *         
* 17FEB05  (AK)   -- LPM SUPPORT FOR AFFIDAVIT FIX                    *         
* 07MAR05  (AK)   -- CHANGES FOR FUSION CABLE RATINGS                 *         
* 11JUL05  (AK)   -- LPM WEEKLY/OVERNIGHT SUPPORT                     *         
* 18AUG05  (AK)   -- HONOR SPILLDEF FOR CANDIAN SOFT DEMO OVERRIDES   *         
* 14SEP05  (AK)   -- HONOR WRITER OPT TO OVERRIDE 00A PROF TO C/F/L/0 *         
* 06OCT05  (AK)   -- ALWAYS GET RTG SVC FROM STA FOR CANADIAN SOFT DEM*         
* 19OCT05  (AK)   -- 0 FOR MARKET RTGSVC SUPPRESSES CABLE LOOKUPS     *         
* 16DEC05  (AK)   -- TEST IF WE WANT TO GO THROUGH SOFT DEMO CODE     *         
*                 -- AND GET RATING SVC FOR NWS                       *         
* 18JAN06  (AK)   -- FIX FUSION BUG WHEN TOGGLING BETWEEN NO CBL & CBL*         
* 25JAN06  (AK)   -- SKIP NULL HOMES DATA ON DEMAND HOOK              *         
* MAR1307  (BEN)  -- REWRITE WEEKLY/OVERNIGHT POSTING                 *         
* 11SEP07  (AK)   -- SPLKDPT DEFUNCT                                  *         
* 04SEP13  (AK)   -- SUPPORT 2-DECIMAL DEMOS FOR NETWORK              *         
* 03MAR14  (AK)   -- LOOK UP AFFIDS FOR DUMMY CALL LETTER PACAT       *         
*                 -- EVEN IF NO ACTUAL BOOK IS PASSED IN              *         
*======================================================================         
T00A21   TITLE 'SPGETDEMO - DEMO LOOK-UPS FROM PARAM BLOCK'                     
GETDEMO  RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,GETDEMO,RA,R9,R3,CLEAR=YES,RR=R4                     
         USING WORKD,RC                                                         
*                                                                               
         MVC   CALLRD,4(RD)        SAVE USER'S RD VALUE                         
         ST    R4,RELO                                                          
         ST    R1,SAVER1                                                        
         ST    RD,SAVERD                                                        
         L     RE,=V(NSIWEEK)                                                   
         A     RE,RELO                                                          
         ST    RE,VNSIWEEK                                                      
         L     RE,=V(NETWEEK)                                                   
         A     RE,RELO                                                          
         ST    RE,VNETWEEK                                                      
*                                                                               
         BRAS  RE,SETOVERL       INIT VARIOUS THINGS                            
*                                                                               
* COUNT NUMBER OF DEMOS PRESENT IN LIST AND SAVE *                              
         SPACE 1                                                                
         L     RE,SPLKALST                                                      
         SR    R0,R0                                                            
         CLI   0(RE),X'FF'                                                      
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         BCT   R0,*-12                                                          
         LPR   R0,R0                                                            
         BZ    EXIT                                                             
         STC   R0,SPLKALST                                                      
         ST    R0,NUMDEMS                                                       
*&&DO                                                                           
         LA    RE,DEXTRA1                                                       
         MVC   4(4,RE),DBEXTEND                                                 
         STCM  RE,15,DBEXTEND                                                   
****     ST    RE,DBEXTEND         SET AS EXTENSION 1                           
         MVC   0(4,RE),=C'UID '                                                 
         MVC   8(2,RE),SPLKUID     SET USERID                                   
*                                                                               
         ICM   RE,15,SPLKAREC                                                   
         CLC   0(8,RE),=C'DBEXTEND' PUNT - ALLOW APPS TO GET THIS               
         BNE   *+10                        CONTROL TO DEMO SYSTEM               
         MVC   DEXTRA1+4(4),8(RE)          SET IT IF IT BE THERE                
*                                                                               
         ICM   RE,15,SPLKA1W                                                    
         BZ    NO1W                                                             
         MVC   SV1WPROF,0(RE)      SAVE 1W PROFILE VALUES                       
*                                                                               
*                                                                               
         CLI   SPLKMED,C'T'        USTV ALWAYS IMP BASED                        
         BE    *+8                                                              
         CLI   SV1WPROF+5,C'I'     IMP BASED CALCULATED DEMOS                   
         BNE   NO1W                                                             
         MVI   TAPEOPT,C'Y'        CHECK FOR NSI/USTV BELOW                     
NO1W     DS    0C                                                               
*&&                                                                             
         BRAS  RE,SETUP1                                                        
*&&DO                                                                           
         MVC   DBAREC,SPLKAREC                                                  
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'                                                      
         CLI   SPLKTPTT,C'P'       ALLOW A SPECIFIC TP REQUEST                  
         BNE   *+8                  TO GO THROUGH                               
         MVI   DBTPTT,C'P'         SET FOR 4 WEEK AVG. ONLY                     
*                                                                               
         MVC   ADBUY,SPLKABUY                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         MVC   DBCOMFCS,SPLKAFAC                                                
         MVC   DBFILE,=C'TP '                                                   
         CLI   SPLKFIL,C'T'                                                     
         BE    *+10                                                             
         MVC   DBFILE,=C'PAV'    **********                                     
         MVC   DBSELMED,SPLKMED                                                 
         MVC   DBSELSRC,SPLKSRC                                                 
         MVC   DBSELAGY,SPLKAGY                                                 
         MVC   DBSELCLI,SPLKCLI                                                 
         MVC   DBSELUMK,SPLKUMK    USER MKT NUM FOR RTG SVC OVRD                
         MVC   DBSELBK,SPLKDBK                                                  
         MVC   DBBTYPE,SPLKBTYP                                                 
         CLI   DBBTYPE,C'A'        BOOK TYPE MUST BE A-Z                        
         BL    *+12                                                             
         CLI   DBBTYPE,C'Z'                                                     
         BNH   *+8                                                              
         MVI   DBBTYPE,0                                                        
* FOR SRC DATA NEED TO CHANGE DBSELSRC                                          
         CLI   DBBTYPE,C'S'                                                     
         BNE   *+12                                                             
         MVI   DBSELSRC,C'S'                                                    
         MVI   DBBTYPE,0                                                        
                                                                                
* FOR NETWORK ALLOW ANY BOOK TYPE TO COME IN                                    
                                                                                
         CLI   DBSELMED,C'N'       FOR NETWORK                                  
         BNE   *+10                                                             
         MVC   DBBTYPE,SPLKBTYP    MOVE IN STRAIGHT FROM CALLER                 
*&&                                                                             
*                                                                               
         MVC   ADBUY,SPLKABUY                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         BRAS  RE,FUSION                                                        
         BRAS  RE,LPM              LPM SUPPORT                                  
*                                                                               
         MVC   DBDAYOPT,SPLKDOPT                                                
         MVC   DBBEST,SPLKBEST                                                  
         MVC   DBSELSPO,SPLKSPRT                                                
         MVC   DBSELWKN,SPLKWKN                                                 
         MVC   DBSELDAT,SPLKLBK                                                 
         MVC   DBSELSTA,SPLKSTA                                                 
         CLI   SPLKNTI,C' '        SUPPORT FOR NTI CABLE LOOKUPS                
         BNH   NONCAB                                                           
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'C'                                                    
         MVC   DBSELSTA,SPLKNTI                                                 
         MVI   DBSELSTA+4,C'C'                                                  
         CLC   DBSELSTA(4),=C'TEL '                                             
         BNE   CWB                                                              
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELSTA+4,C'H'                                                  
         B     NONCAB                                                           
CWB      CLC   DBSELSTA(4),=C'WB  '                                             
         BNE   NONCAB                                                           
         MVI   DBSELSRC,C'K'                                                    
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
NONCAB   ICM   RE,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    INDB18              NO                                           
         USING SPLKXTD,RE          EXTENSION AREA DSECT                         
         TM    SPXTFLG2,SPXTUNIV   UNIVERSE LOOK-UP?                            
         BNZ   INDB19              YES                                          
         DROP  RE                  DROP EXTENSION USING                         
*                                                                               
INDB18   CLI   DBSELSRC,C'A'       ARB ALLOW NUMERIC STATION CALL               
         BE    *+8                                                              
         CLI   DBSELSRC,C'H'       IHT ALLOW NUMERIC STATION CALL               
         BE    *+8                                                              
         CLI   DBSELSRC,C'T'       TRITON AS WELL                               
         BNE   *+8                                                              
         CLI   DBSELMED,C'R'                                                    
         BE    *+8                                                              
         CLI   DBSELMED,C'D'       TV DAYPART                                   
         BE    *+8                                                              
         CLI   DBSELMED,C'U'       RADIO COUNTY COVERAGE                        
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'       FUSION ALLOW NUMERIC STATION CALL            
         BE    *+12                                                             
         CLI   DBSELSTA,C'Z'       TEST FOR A CABLE STATION                     
         BH    EXIT                TOO BAD - NO DEMOS YET                       
INDB19   OC    DBSELSTA,DBSELSTA   TEST STATION PRESENT                         
         BNZ   *+14                                                             
         MVI   DBFUNCT,DBGETTOT    NO STATION MEANS MKT TOT REQ                 
         MVC   DBSELRMK,SPLKRMK                                                 
         SPACE 2                                                                
         OC    DBSELALF,DBSELALF   ALPHA MKT SET FOR FUSION?                    
         BNZ   *+10                YES DO NOT SET WITH SPLKALF!                 
         MVC   DBSELALF,SPLKALF    PASS ALPHA MKT (IF THERE)                    
         MVC   DBSELMK,SPLKSPL     RTG SVC MKT FOR SPILL REQ                    
         CLI   DBSELMED,C'R'       IF MEDIA=RADIO,                              
         BNE   INDB20                                                           
         OC    DBSELRMK,DBSELRMK    AND NO RATING SVCE MKT# YET,                
         BNZ   INDB20                                                           
         MVC   DBSELRMK,SPLKSPL     SET RATING SVCE MKT#                        
INDB20   DS    0H                                                               
                                                                                
         MVC   DBSELPUR,SPLKPUR                                                 
         MVC   DBFRANCO,SV1WPR4    ANGLO/FRANCO OPTION (CANADA ONLY)            
*                                                                               
         CLI   DBBEST,C'W'         HAVE WEEKLY?                                 
         BE    INDB25              YES - DO NOT OVERWRITE WEEKLY                
         CLI   SV1WPR9,C'M'                                                     
         BNE   *+14                                                             
         MVI   DBBEST,C'M'         CANADIAN MM MONTHLY                          
*                                                                               
         MVC   DBUSEBBM,SV1WPR10   USE BBM IF AVAIL                             
*                                                                               
INDB25   MVC   SAVEHUT,SPLKSVI                                                  
         ICM   RE,15,SPLKAWGT      GET A(DEMO WEIGHT LIST)                      
         BZ    *+10                                                             
         MVC   SAVEWGTS,0(RE)                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
*&&DO                                                                           
         ICM   R3,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    INDB30                                                           
         USING SPLKXTD,R3                                                       
         TM    SPXTFLAG,SPXTDGTB   DOING A DBGETTLB CALL?                       
         BZ    INDB30              NO                                           
         DROP  R3                                                               
*&&                                                                             
         ICM   R8,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    INDB30                                                           
         USING SPLKXTD,R8                                                       
         TM    SPXTFLAG,SPXTDGTB   DOING A DBGETTLB CALL?                       
         BZ    INDB30              NO                                           
         DROP  R8                                                               
         MVI   DBFUNCT,DBGETTLB                                                 
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         L     R1,SAVER1                                                        
         L     RF,0(R1)                       GET A(SPLK BLOCK)                 
         MVC   SPLKLBK-SPDEMLK(2,RF),DBACTBK   SET LATEST BOOK                  
         B     EXIT                                                             
* SAVE OFF DBBTYPE... FOR POSTING AFFIDS THE BOOK AT THIS POINT IS              
* ONLY SET TO THE BOOK ON THE BUYLINE AND NOT THE AFFIDS                        
* THE BOOKTYPE SWITCH CODE IN DEGET INIT ROUTINE FOR CABLE                      
* WILL CHANGE THE BOOKTYPE GIVEN AN EFFECTIVE BOOK AND THIS COULD               
* RESULT IN THE WRONG BOOKTYPE BEING USED FOR AFFIDS CALL                       
*                                                                               
INDB30   MVI   DBFUNCT,DBGETCTL                                                 
         MVC   SVDBTYPE,DBBTYPE                                                 
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         MVC   DBBTYPE,SVDBTYPE                                                 
         TM    DBCSHR,DBOTOSHR     TEST COMPUTE SHARES                          
         BZ    *+8                                                              
         MVI   SHRSW,X'80'         INDICATE SHARE COMP REQUIRED                 
*                                                                               
         TM    SPLKOPT,SPLKOLOW    LOWER QH FEATURE IS INCOMPATABLE             
         BO    *+16                WITH 7 MIN OPTION                            
         TM    DBC7MIN,DBOE7MIN    TEST 7 MIN LAST QH OPTION                    
         BZ    *+8                                                              
         OI    AFDOPT,AFDO7MIN     YES                                          
*                                                                               
         MVI   DBFUNCT,DBGETDEM    RESTORE DEMO LOOK-UP FUNCTION                
*                                                                               
         TM    SPLKOPT,SPLKOPR1+SPLKOP2D   SPECIAL DEC REQUEST                  
         BZ    GD2                 NO                                           
*                                                                               
         MVI   DBFUNCT,DBGETDEM                                                 
         LA    RF,DEXTRA2                                                       
         USING DBXTTID,RF                                                       
         MVC   DBXTNEXT,DBEXTEND   LINK TO PREV  DBEXTEND                       
         STCM  RF,15,DBEXTEND                                                   
         MVC   DBXTID(4),=C'SPOT'                                               
         TM    SPLKOPT,SPLKOP2D    2-DECIMAL REQUEST                            
         BO    SET2DEC                                                          
         MVI   DBXTTRP,X'01'                                                    
         MVI   DBXTTSP,X'01'                                                    
         MVI   DBXTTIP,X'02'                                                    
         B     GD2                                                              
*                                                                               
SET2DEC  MVI   DBXTSCTL,C'2'       CHAR-YOU CAN ASK FOR 0-2DEC                  
         DROP  RF                                                               
         EJECT                                                                  
*=================================================================*             
* DUE TO THE UNTIMELY DEATH OF ARB, LOOK THROUGH THE BUY RECORD   *             
* FOR ANY SPOTS AFTER 26DEC93 AND FORCE RTGSVC TO NSI             *             
*=================================================================*             
         SPACE 1                                                                
GD2      TM    BDSTAT2,X'04'       TEST FORCE ARB                               
         BZ    *+12                                                             
         MVI   DBSELSRC,C'A'                                                    
         B     GD2B                                                             
*                                                                               
         CLI   DBSELSRC,C'R'       TEST RAD                                     
         BE    *+12                                                             
         CLI   DBSELSRC,C'A'       TEST ARB                                     
         BNE   GD2B                                                             
         CLI   DBSELMED,C'T'       TEST US DEMOS                                
         BNE   GD2B                                                             
         OC    SPLKABUY,SPLKABUY   AND A(BUY RECORD) PASSED                     
         BZ    GD2B                                                             
         TM    SPLKOPT,SPLKOARB    AND ARB IS NOT FORCED                        
         BO    GD2B                                                             
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
GD2A     BAS   RE,NEXTEL                                                        
         BNE   GD2B                                                             
         CLC   2(2,R6),=X'BB9A'                                                 
         BNH   GD2A                                                             
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
GD2B     CLI   DBSELMED,C'C'       TEST CANADA                                  
         BNE   GD2C                                                             
         OC    DBSELMK,DBSELMK     HAVE SPILL?                                  
         BNZ   GD2C                YES, DO NOT GET THE ALPHA, USE SPILL         
         BRAS  RE,GETSTOVR                                                      
*                                                                               
GD2C     CLI   DBSELMED,C'N'       ALLOW NETWORK TO GO THRU                     
         BE    GD3                                                              
*        BE    *+16                                                             
*        CLI   DBSELSRC,C'N'       IMP BASE FOR NSI/USTV ONLY                   
*        BNE   *+12                                                             
         CLI   DBSELMED,C'O'       TP OVERNIGHTS                                
         BE    *+8                                                              
         CLI   DBSELMED,C'T'                                                    
         BE    GD3                                                              
         MVI   TAPEOPT,C'N'        RESET FOR ALL OTHERS                         
         EJECT                                                                  
* CREATE ADDRESS CONSTANTS FOR NON-ADDRESSABLE WORK AREA FIELDS *               
         SPACE 1                                                                
GD3      DS    0H                                                               
***GD3      LA    R1,ADCONS                                                     
         BRAS  RE,SETADCON                                                      
*                                                                               
* SET FLAGS                                                                     
         DS    0H                                                               
         CLI   DBSELMED,C'C'       IF NOT CANADIAN MEDIA,                       
         BNE   STCANFGX             DON'T SET FLAG                              
                                                                                
         CLI   DBSELSRC,C'A'       ARBITRON / BBM                               
         BE    STCANFGW                                                         
*BPOOTEST                                                                       
         CLI   DBSELSRC,C'R'       RADAR                                        
         BE    STCANFGW                                                         
         CLI   DBSELSRC,C'N'       NIELSEN                                      
         BNE   STCANFGX            DON'T SET FLAG FOR OTHER SOURCES             
*                                                                               
STCANFGN DS    0H                  NIELSEN                                      
         OC    DBSELBK,DBSELBK                                                  
         BZ    STCANFGW             SET FLAG IF LATEST BOOK REQUESTED           
         CLC   DBSELBK,=X'6001'                                                 
         BNL   STCANFGW             SET FLAG IF BOOK >= 1ST WK OF 1996          
         B     STCANFGX            DON'T SET FLAG                               
         PRINT OFF                                                              
*&&DO                                                                           
         CLI   DBSELMED,C'C'                                                    
         BNE   *+16                                                             
         CLI   DBSELSRC,C'A'                                                    
         BNE   *+8                                                              
         OI    FLAGS,CANHPT        SET CANADIAN HPT'S                           
*&&                                                                             
         PRINT ON                                                               
STCANFGW DS    0H                  BRANCH HERE TO                               
         OI    FLAGS,CANHPT         SET CANADIEN HPT'S                          
STCANFGX EQU   *                                                                
         B     GD4                                                              
*&&DO                                                                           
ADCONS   DS    0A                                                               
         DC    A(DTUNVS-WORKD)                                                  
         DC    A(DTDEMS-WORKD)                                                  
         DC    A(SVIREC-WORKD)                                                  
         DC    A(IUNWK-WORKD)                                                   
         DC    A(IUNVS-WORKD)                                                   
         DC    A(IUNOLD-WORKD)                                                  
         DC    A(IRTGOLD-WORKD)                                                 
         DC    A(IPUTOLD-WORKD)                                                 
         DC    A(IIMPOLD-WORKD)                                                 
         DC    A(ITOTOLD-WORKD)                                                 
         DC    A(IUNOLDX-WORKD)                                                 
         DC    A(IUNNEW-WORKD)                                                  
         DC    A(IRTGNEW-WORKD)                                                 
         DC    A(IPUTNEW-WORKD)                                                 
         DC    A(IIMPNEW-WORKD)                                                 
         DC    A(ITOTNEW-WORKD)                                                 
         DC    A(IUNNEWX-WORKD)                                                 
         DC    A(IUNXTRA-WORKD)                                                 
         DC    A(DBLOCKS-WORKD)                                                 
         DC    A(SPDTTAB-WORKD)                                                 
         DC    A(GDEXSPDT-WORKD)                                                
         DC    A(DBLOCK2-WORKD)                                                 
         DC    A(COMEXT-WORKD)                                                  
ADCONSX  EQU   *                                                                
*&&                                                                             
         SPACE 1                                                                
GD4      TM    FLAGS,CANHPT        CANADIAN HPT'S?                              
         BNZ   GD4A                                                             
         CLI   SPLKMED,C'R'                                                     
         BNE   GD4X                                                             
GD4A     L     R1,ASVIREC          POINT TO AN I/O AREA                         
         XC    0(256,R1),0(R1)     AND CREATE A FAKE RECORD                     
         MVI   0(R1),C'R'                                                       
         LA    R0,DRFRSTEL+1-DRKEY                                              
         STCM  R0,3,DRRLEN-DRKEY(R1)                                            
                                                                                
         CLI   SPLKMED,C'R'        USE THIS OPPORTUNITY TO CREATE               
         BNE   GD4C                 DBEXTEND BLOCK FOR RADIO                    
         LA    RF,DEXTRA3                                                       
         USING DBEXTRAD,RF                                                      
         XC    0(L'DEXTRA3,RF),0(RF)                                            
         MVC   DBRID,=C'RADI'      RADIO EXTEND ID                              
                                                                                
         MVI   DBRCOPT,C'N'        GENERAL SPOT, NO CONDENSED SUPPORT           
*  SEE IF SPOT DESKTOP IS CALLER                                                
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GD4B                                                             
         USING SPLKXTD,RE                                                       
         NI    DBBSTFLG,X'FF'-DBBSTMBK                                          
         NI    DBBSTFLG,X'FF'-DBBSTLAT                                          
         TM    SPXTFLG2,SPXTBMBK   BEST MULTI BOOK AVG AVAILABLE                
         BZ    *+8                                                              
         OI    DBBSTFLG,DBBSTMBK                                                
*                                                                               
         TM    SPXTFLG2,SPXTLATB   LOOKUP LATEST BOOK WHEN NOT FOUND            
         BZ    *+8                                                              
         OI    DBBSTFLG,DBBSTLAT                                                
*                                                                               
         MVC   COMCSD,SPXTCSD      COMSCORE SURVEY DATE                         
         MVC   REQSDATE,SPXTSTDT   REPORT REQUEST START DATE                    
         MVC   REQEDATE,SPXTENDT   REPORT REQUEST END DATE                      
         MVC   ASPXT50E,SPXT50EL   A(50 ELEMENT) PASSED IN                      
***      TM    SPXTFLG2,SPXTSDEM   CALLER SPOT DESKTOP DEMO ENGINE?             
****     BZ    *+8                                                              
         MVI   DBRCOPT,C'Y'        ALLOW RADIO CONDENSE DEMOS FOR               
         DROP  RE                                                               
*                                                                               
GD4B     MVC   DBRNEXT,DBEXTEND    NEXT EXTENSION...PREPENDING THIS             
         DROP  RF                                                               
         STCM  RF,15,DBEXTEND       EXTENSION BLOCK ONTO "LINK-LIST"            
*                                                                               
GD4C     DS    0H                                                               
         EJECT                                                                  
* GET ADDRESSES OF REQUIRED DEMO TABLES *                                       
         SPACE 1                                                                
GD4X     XC    DUB,DUB                                                          
         MVC   DUB(5),=X'D0000000FF'                                            
         L     R4,DBCOMFCS                                                      
         L     RF,CDEMADDR-COMFACSD(R4)                                         
         GOTO1 (RF),P1,(X'FF',DUB),(R4)                                         
*                                                                               
         L     R1,DUB              POINT TO MASTER DISP TABLE                   
         USING DSPHDRD,R1                                                       
         XR    RE,RE                                                            
*                                                                               
GD6      CLC   0(2,R1),=XL2'00'    TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DSPFILE,C'I'        MATCH ON INTERNAL FILE CODE                  
         BE    GD7                                                              
GD6A     ICM   RE,7,DSPAET                                                      
         LA    R1,1(RE,R1)         NEXT TABLE HEADER                            
         B     GD6                                                              
*                                                                               
GD7      CLI   TAPEOPT,C'Y'                                                     
         BE    GD8                                                              
         CLC   DSPSBOOK,=X'B0F4'                                                
         BE    GD8                                                              
         B     GD6A                                                             
*                                                                               
GD8      AHI   R1,DSPHDRLN         BUMP PAST HEADER                             
         ST    R1,ADISPTAB         AND SAVE A(FIRST DEMO ENTRY)                 
         SPACE 1                                                                
         L     RE,SPLKALST                                                      
         MVC   DEMOLIST,0(RE)      MOVE CALLERS DEMO LIST                       
*                                                                               
******   BRAS  RE,INSPDEM             INSPECT DEMO LIST                         
*                                                                               
                                                                                
         CLI   DBSELMED,C'R'       BUILD 2ND DEMO LIST FOR RADIO                
         BNE   GD8BX                                                            
         XC    RDEMLIST,RDEMLIST                                                
         XC    DEMOLST2,DEMOLST2                                                
         LA    RF,RDEMLIST                                                      
         USING RDEMLSTD,RF                                                      
GD8A10   MVI   0(RF),X'FF'         ASSUME END OF DEMOLIST                       
         CLI   0(RE),X'FF'                                                      
         BE    GD8AX                                                            
         MVC   RDLDMOD,1(RE)       INPUT MODIFIER                               
         MVC   RDLDNUM,2(RE)       DEMO NUMBER                                  
         CLI   RDLDMOD,C'R'        IF DEMO IS A RATING,                         
         BNE   GD8A20                                                           
         MVI   RDLDMOD1,C'I'        GET SUB-ORDINATE MODIFIERS TOO              
         MVI   RDLDMOD2,C'U'                                                    
         MVI   RDLOPER,C'D'         OPERATION=DIVIDE                            
GD8A20   LA    RE,3(RE)                                                         
         LA    RF,RDEMLSTQ(RF)                                                  
         B     GD8A10                                                           
GD8AX    DS    0H                                                               
         DROP  RF                                                               
                                                                                
         LA    RF,RDEMLIST                                                      
         USING RDEMLSTD,RF                                                      
         LA    RE,DEMOLIST                                                      
         LA    R1,DEMOLST2         R1-->TARGET LIST                             
         SR    R0,R0               R0 COUNTS # OF DEMOS                         
GD8B10   MVI   0(R1),X'FF'         ASSUME FINISHED BUILDING THE LIST            
         CLI   0(RE),X'FF'                                                      
         BE    GD8B40                                                           
         MVC   0(3,R1),0(RE)                                                    
         CLI   RDLDMOD1,0                                                       
         BE    GD8B20                                                           
         LA    R1,3(R1)                                                         
         MVC   0(1,R1),0(RE)                                                    
         MVC   1(1,R1),RDLDMOD1                                                 
         MVC   2(1,R1),RDLDNUM                                                  
         BCT   R0,*+4                                                           
GD8B20   CLI   RDLDMOD2,0                                                       
         BE    GD8B30                                                           
         LA    R1,3(R1)                                                         
         MVC   0(1,R1),0(RE)                                                    
         MVC   1(1,R1),RDLDMOD2                                                 
         MVC   2(1,R1),RDLDNUM                                                  
         BCT   R0,*+4                                                           
GD8B30   LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         LA    RF,RDEMLSTQ(RF)                                                  
         BCT   R0,*+4                                                           
         B     GD8B10                                                           
GD8B40   LPR   R0,R0                                                            
         STC   R0,SPLKALST                                                      
GD8BX    DS    0H                                                               
         DROP  RF                                                               
                                                                                
         L     RF,SPLKAVAL         POINT TO OUTPUT AREA                         
         LA    RE,DEMOLIST                                                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    RE,DEMOLST2                                                      
*                                                                               
GD9      XC    0(8,RF),0(RF)       CLEAR DEMO VALUE/SVI                         
         LA    RF,8(RF)                                                         
         LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'         TEST EOL                                     
         BNE   GD9                                                              
*                                                                               
         DS    0H                                                               
         XC    TOTHMSHR,TOTHMSHR                                                
*                                                                               
         MVI   IUNTYPE,1           DEFAULT TO RTGS/IMPS ONLY                    
         LA    RE,DEMOLIST                                                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    RE,DEMOLST2                                                      
*                                                                               
GD9A     CLI   1(RE),C'R'                                                       
         BE    *+12                                                             
         CLI   1(RE),C'I'                                                       
         BNE   GD9B                                                             
         LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'         TEST EOL                                     
         BNE   GD9A                                                             
         B     GD9C                                                             
*                                                                               
GD9B     MVI   IUNTYPE,4                                                        
*                                                                               
GD9C     ICM   R6,15,SPLKAAFD      TEST SPECIFIC AFFID LOOKUP                   
         BZ    GD10                NO                                           
         BAS   RE,GETAFD           YES - LOOK UP DEMOS                          
         BAS   RE,RSTDQLNK                                                      
         B     EXIT                AND GET OUT                                  
*                                                                               
GD10     TM    SPLKOPT,SPLKOAFD    TEST AFFID LOOK-UP                           
         BZ    GD50                                                             
         MVC   AFDPRD,SPLKAFPR                                                  
         MVC   AFDSLN,SPLKAFLN                                                  
         MVC   AFDSDT,SPLKAFST                                                  
         MVC   AFDEDT,SPLKAFND                                                  
         B     GD100                                                            
         EJECT                                                                  
*===============================================================                
*  NON - AFFIDAVIT DEMO LOOK-UP                                                 
*===============================================================                
                                                                                
GD50     MVC   DBSELDAY,SPLKDAY                                                 
         MVC   DBSELTIM,SPLKTIM                                                 
         MVC   DBTAPEP,TAPEOPT                                                  
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVI   CALLERF,0                                                        
         TM    SPLKOPT,SPLKO1WB    IF ONE WEEK BUY                              
         BZ    *+10                                                             
         MVC   DBSEL1WK,SPLKADAT   SET THE DAY SO I CAN DO 1W PROGRAM           
*                                                                               
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GD50AA                                                           
         USING SPLKXTD,RE                                                       
         MVC   ESTEOW,SPXTEOWD                                                  
         TM    SPXTFLG2,SPXTSPWR                                                
         BZ    GD50AA                                                           
         OI    DBVOPT,X'40'        SET FLAG TO INDICATE SPOT POSTING            
         MVI   CALLERF,SPXTSPWR    SAVE CALLER INDICATOR                        
         MVC   COMCSD,SPXTCSD      COMSCORE SURVEY DATE                         
         MVC   ASPXT50E,SPXT50EL   A(50 ELEMENT)                                
         MVC   REQSDATE,SPXTSTDT   REPORT REQUEST START DATE                    
         MVC   REQEDATE,SPXTENDT   REPORT REQUEST END DATE                      
         DROP  RE                                                               
*&&DO                                                                           
* IF PASS ONE MODE THEN BYPASS ALL EXISTING DEMO SYSTEM CALLING                 
* CODE AND GO STRAIGHT TO CALL COMINTER                                         
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F')                      
         IF (CLI,COMPASS,E,X'01'),AND,(CLI,DBVOPT,E,X'40')                      
****     IF (TM,NTDFLAG,NTDCOMD,O) HAVE COMSCORE DEMOS                          
         IF (TM,SPLKOPT2,SPLKOPT2_COMS,O) HAVE COMSCORE DEMOS                   
         B     GDCOMS10                                                         
         J     EXIT       IF PASS ONE AND NO COMSCORE DEMOS JUST EXIT           
         ENDIF                                                                  
         ENDIF                                                                  
         ENDIF                                                                  
*&&                                                                             
*                                                                               
* IF PASS ONE MODE THEN BYPASS ALL EXISTING DEMO SYSTEM CALLING                 
* CODE AND GO STRAIGHT TO CALL COMINTER                                         
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),                     
            ANDIF,(CLI,COMPASS,E,X'01'),AND,(CLI,DBVOPT,E,X'40')                
               IF (TM,SPLKOPT2,SPLKOPT2_COMS,O) HAVE COMSCORE DEMOS             
                B     GDCOMS10                                                  
               ELSE                                                             
                J     EXIT       IF PASS ONE AND NO COMSCORE DEMOS              
               ENDIF                                                            
         ENDIF                                                                  
                                                                                
                                                                                
*                                                                               
*                                                                               
* IF PASS TWO MODE:                                                             
* IF ONLY COMSCORE DEMO PASSED- SKIP NIELSEN PROCESSING                         
*                                                                               
*&&DO                                                                           
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F')                      
         IF (CLI,COMPASS,E,X'02'),AND,(CLI,DBVOPT,E,X'40')                      
         IF (CLI,SPLKOPT2,E,SPLKOPT2_COMS) IF ONLY HAVE COMSCORE DEMO           
         B     GDCOMS10             DONT PROCESS NIELSEN                        
         ENDIF                                                                  
         ENDIF                                                                  
         ENDIF                                                                  
*&&                                                                             
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,COMPASS,E,X'02'),AND,(CLI,DBVOPT,E,X'40'),ANDIF,               
            (TM,SPLKOPT2,SPLKOPT2_NLSN,Z) DONT HAVE NIELSEN DEMOS               
***         (CLI,SPLKOPT2,E,SPLKOPT2_COMS) IF ONLY HAVE COMSCORE DEMO           
                                                                                
         B     GDCOMS10             DONT PROCESS NIELSEN                        
         ENDIF                                                                  
*                                                                               
GD50AA   MVI   DEMANDSW,0          RESET SWITCH                                 
*                                                                               
         MVC   SVDBBEST,DBBEST    SAVE AWAY DBBEST                              
         BRAS  RE,TOMONTHL        DEFAULT WEEKLY TO MONTHLY LOOKUPS             
*                                 AFTER MMW CUTOFF DATE.                        
*==================     CHECK POSTING OPTIONS   ======================          
         CLC   =C'YNN',LPMLKOPT   IF USER REQUESTED LPMWK OPTION  ONLY          
         BNE   *+14               AND WE ARE NOT DEALING WITH AN LPM            
         OC    LPMDTP,LPMDTP      MARKET, WE WANT TO READ MONTHLY               
         BZ    GD50VPHX                                                         
*                                                                               
         CLI   LPMLKOPT,C'Y'                                                    
         BE    GD50A                                                            
         CLI   WVPHOPT,C'Y'                                                     
         BE    GD50A                                                            
***      CLI   OVPHOPT,C'Y'                                                     
***      BNE   GD50VPHX                                                         
         CLI   OVPHOPT,C'Y'                                                     
         BE    GD50A                                                            
* IF WE ARE POSTING MONTHLY -CHECK IF BOOK IS JUN09 FOR LPM MARKET              
* JUN09 IS THE DIGITAL TRANSITION WHERE NIELSEN HAS NOT RELEASED A              
* BOOK HENCE WE WANT TO TO OVERNIGHTS - IF NO OVERNIGHTS WE WANT TO             
* TO POST TO JUL_09                                                             
         MVI   DIGITALF,C'N'                                                    
*&&DO                                                                           
         TM    DBVOPT,X'40'        DIGITAL TRANSITION CODE ONLY                 
         BNO   GD50VPHX            VALID FOR POSTING                            
         CLC   DBSELBK,=AL2(JUN_09)                                             
         BNE   GD50VPHX                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   GD50VPHX                                                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   GD50VPHX                                                         
         OC    LPMDTP,LPMDTP     ONLY FOR LPM MARKETS-NON LPM POST              
         BZ    GD50VPHX          TO MONTHLY                                     
         OC    DBSELSYC,DBSELSYC  LPM CABLE JUN09 POST TO JUL09                 
         BNZ   GD50AB                                                           
         CLI   DBBTYPE,C'H'      HISPANIC/BLACK BOOKTYPES POST TO               
         BE    *+8               JUL09 BOOK                                     
         CLI   DBBTYPE,C'B'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'H'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'J'                                                     
         BNE   *+14                                                             
GD50AB   MVC   DBSELBK,=AL2(JUL_09)                                             
*&&                                                                             
         B     GD50VPHX                                                         
         MVI   DIGITALF,C'Y'     TURN ON DIGITAL TRANSITION FLAG                
*******************************************************************             
* YOU SHOULD ONLY BE HERE FOR POSTING WHERE POSTING OPTIONS ARE ON              
*******************************************************************             
                                                                                
GD50A    DS    0C                                                               
***      MVC   SVDBBEST,DBBEST    SAVE AWAY DBBEST                              
         CLI   DBBTYPE,0           ALWAYS ALLOW STANDARD                        
         BE    GD50H                                                            
         CLI   LPMLKOPT,C'Y'       IF WE ARE POSTING WEEKLY THEN CHECK          
         BE    *+8                 BOOKTYPE INDICATOR TO SEE IF WEEKLY          
         CLI   WVPHOPT,C'Y'        IS AVAILABLE FOR THIS BOOKTYPE               
         BNE   *+12                IF ITS NOT AN AVAILABLE WEEKLY               
         TM    BKTYPIND,SPBKWKLY   BOOKTYPE THEN CHECK TO SEE IF                
         BNZ   GD50H               OVERNIGHT POSTING OPTION IS ON               
         CLI   DIGITALF,C'Y'       IF DIGITAL TRANSITION WE DONT CARE           
         BE    GD50H               BOOKTYPE IS AVAILABLE FOR OVERNIGHT          
         CLI   OVPHOPT,C'Y'        OR NOT- WE WILL TRY OVERNIGHTS 1ST           
         BNE   GD50G               IF NOT FOUND- WE GO TO JUL09 BOOK            
         TM    BKTYPIND,SPBKOVN    BOOKTYPE AVAILABLE FOR OVERNIGHT?            
         BNZ   GD50H                                                            
GD50G    MVI   DBSELMED,C'T'       IF NOT AVAILABLE-GO READ MONTHLY             
* RESET WEEKLY AND OVERNIGHT VALUES FROM PREVIOUS READ                          
*&&DO                                                                           
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
*&&                                                                             
         BRAS  RE,CLRHOMEV                                                      
                                                                                
         MVI   NEXTYRLK,C'N'                                                    
         B     GD50VPHX                                                         
*                                                                               
GD50H    OC    DBSELSYC,DBSELSYC  CABLE IS ONLY MONTHLY FOR NOW                 
         BNZ   GD50VPHX                                                         
         OC    DBSELMK,DBSELMK                                                  
         BNZ   GD50VPHX                                                         
         MVI   DBSELMED,C'W'                                                    
         MVC   SVSELBK,DBSELBK     BOOK FORMAT IS YY00                          
         BRAS  RE,GETVDAT          GET WTP YEAR                                 
*******************************************************************             
*  CONVERT THE BOOK TO GET THE CORRECT YEAR -COULD BE END OF YR BUT             
*  ITS STILL CAN BE 1ST WEEK OF NEXT YR ON THE FILE                             
*******************************************************************             
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DUB),(0,DUB3)                                       
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 VNSIWEEK,DMCB,DUB3                                               
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
                                                                                
*                                                                               
         TM    CALLERF,SPXTSPWR    NON POSTING CALLERS LIKE $DEM                
         BZ    GD50L               ALWAYS GETS VPH IF WVPHOPT IS ON             
* IS THIS A TRUE LPM MARKET -                                                   
         BRAS  RE,SETMKTYP         SETS STABMKT                                 
         BRAS  RE,TRUELPM          IF NOT A TRUE LPM LPMDTP WILL CLEAR          
*                                                                               
* CHECK FOR LPM MARKET                                                          
         OC    LPMDTP,LPMDTP       ANY LPM DATE                                 
         BZ    GD50I               NO - USE WEEKLY HOMES TO ADJUST              
         CLC   DUB(2),LPMDTB       MONTH GREATER THAT LPM                       
         BL    GD50I                                                            
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0           IF LPM ALREADY NO BOOKTYPE                   
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
         MVI   MKTTYPE,MKTLPM                                                   
*                                                                               
*                                                                               
         B     WVPHLPM             YES - GET LPM DEMOS                          
GD50I    DS    0C                                                               
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION ONLY GO TO                
         BNE   *+12                OVERNIGHTS FOR LPM MARKETS                   
         MVI   DIGITALF,C'N'       ELSE WE WANT TO POST TO MONTHLY              
         B     GD50VPHX                                                         
*----------------   NON LPM SECTION -------------------------------             
* AT THIS POINT- NOT LPM - CHECK TO SEE IF WE ARE READING SET METERED           
* MARKET.  IF SET METERED THEN PROCEED ELSE WE ARE DIARY MARKET                 
* DIARY MARKETS IGNORE  WTP AND OTP OPTIONS AND PROCEED TO READ                 
* BOOK OPTION OR ACT BOOK.                                                      
         OC    LPMDTP,LPMDTP        IF WE HAVE LPM DATE WE MUST HAVE            
         BZ    GD50J                BEEN SET METERED BEFORE                     
* IF LOOKING UP A DATE BEFORE AN LPM MARKET'S LPM START BOOK THEN               
* WE WHOULD READ SET METERED - SINCE IT USED TO BE SET METERED BEFORE           
* GOING LPM - IF WTP/OVN OPTION TURNED ON -ELSE READ MONTHLY                    
         CLC   =C'NN',WVPHOPT                                                   
         BE    GD51V2               READ MONTHLY                                
         MVI   MKTTYPE,MKTSETM                                                  
         B     GD50K                READ SET METERED                            
*                                                                               
GD50J    BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
GD50K    CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BE    GD50L                                                            
         MVI   WVPHOPT,C'N'         DIARY MKT READ MONTHLY                      
         MVI   OVPHOPT,C'N'                                                     
* RESET WEEKLY AND OVERNGIGHT VALUES FROM PREVIOUS READ                         
*&&DO                                                                           
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
*&&                                                                             
         BRAS  RE,CLRHOMEV                                                      
*                                                                               
         MVI   NEXTYRLK,C'N'                                                    
         B     GD51V2                                                           
                                                                                
GD50L    MVI   MKTTYPE,MKTSETM                                                  
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0           IF LPM ALREADY NO BOOKTYPE                   
*                                                                               
         MVI   DBSELBK+1,0         READ WEEKLY ZERO RECORD                      
*                                                                               
*&&DO                                                                           
         CLI   DBSELBK,98                                                       
         BNE   *+14                                                             
         CLC   =C'WKMG',DBSELSTA   DOUBLE CALL LETTER SWITCH                    
         BE    GD50VPHX            NOT HANDLED                                  
*&&                                                                             
*&&DO                                                                           
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
*&&                                                                             
         BRAS  RE,CLRHOMEV                                                      
*                                                                               
***      BAS   RE,SETDQLNK                                                      
         BRAS  RE,SETDQLNK                                                      
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVC   SVSELDAY,SPLKDAY                                                 
         MVI   NEXTYRLK,C'N'                                                    
         MVI   DSTASPLT,C'N'                                                    
         MVI   LASTYRLK,C'N'                                                    
*                                                                               
                                                                                
GD51B    L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETVHOME                                        
*                                                                               
         TM    CALLERF,SPXTSPWR    NON POSTING CALLER SKIP NEW                  
         BZ    GD51V2              POSTING CODE                                 
*                                                                               
*                                                                               
         MVI   MISCOUNT,0          CLEAR MISSING DATA COUNT AND DAY             
         MVI   MISSDAY,0                                                        
         MVC   DBSELDAY,SVSELDAY   RESTORE ROTATION                             
         MVC   SPLKDAY,SVSELDAY    RESTORE ROTATION                             
*                                                                               
* CHECK IF WE NEED TO READ LAST YRS RECORD                                      
* THIS CAN HAPPEN IF WE FAILED READING AN AFFID LOOKUP                          
* AND THE AFFID SPOT DATE IS ON A SAT OR SUNDAY OF THE 1ST WEEK OF              
* THE YEAR BUT THE ROTATION WE WANT TO RERATE AGAINST STARTS BEFORE             
* SAT SO BELONGS IN THE LAST WEEK OF LAST YEARS RECORD                          
*                                                                               
*                                                                               
         CLI   VPHSWK+1,0          WE NEED LAST YEARS RECORD                    
         BNE   GD51C                                                            
         CLI   NEXTYRLK,C'N'       IF DID END OF YEAR SPLIT WEEK DONT           
         BNE   GD51C               DO THIS AGAIN                                
         CLI   LASTYRLK,C'Y'       DID WE ALREADY ADJUST  TO READ LAST          
         BE    GD51C               YEAR'S RECORD?                               
         LLC   RE,DBSELBK          BUMP TO NEXT YEAR'S 00 RECORD                
         SHI   RE,1                                                             
         STC   RE,DBSELBK                                                       
         MVI   DBSELBK+1,0                                                      
         MVI   LASTYRLK,C'Y'       SET FLAG THAT WE READ LAST YR                
         B     GD51B                                                            
*                                                                               
********************************************************************            
* SPLIT WEEK PROBLEM WHERE WEEK IS SPLIT ACROSS 2 DIFFERENT RECORDS             
* TEST END OF YEAR SPLIT  WEEK.  IF ITS A WEEK WHICH CROSSES THE END            
* OF YEAR AND WE DIDNT FIND THE SAT-SUN DATA THAT MEANS WE SHOULD TRY           
* TO READ THE NEXT YEARS RECORD FOR SAT-SUN DATA.                               
* OR OOWR REREADS FOR DAYS THAT SPLIT INTO NEXT WEEK                            
*                                                                               
                                                                                
GD51C    CLI   NEXTYRLK,C'N'       ALREADY PROCESSED END OF                     
         BNE   GD51CC              YEAR SPLIT WEEK.                             
         BRAS  RE,SMMSPLIT                                                      
         CLI   NEXTYRLK,C'Y'        RELOOK UP END OF YEAR SPLIT WEEK            
         BNE   GD51CC               ROTATION FOR NEXT YR                        
         B     GD51B                                                            
*******************************************************************             
* CHECK TO SEE IF WE HAVE CALL LETTER LINK                                      
* IF WE HAVE A CALL LETTER LINK CHECK TO SEE IF WE HAVE                         
* A M-SU SPLIT WEEK PROBLEM WHERE WE READ M-F FROM 1 STATION                    
* BUT THE SA-SU COULD BE ON THE CALL LETTER LINKED DEMO RECORD                  
*                                                                               
GD51CC   CLC   DBSELSTA,DSTACALL   CHECK FOR DSTATION LINK                      
         BE    GD51D                                                            
         CLI   DSTASPLT,C'Y'       IF ALREADY PROCESSED CALL LETTER             
         BE    GD51D               LINK SPLIT WEEK LOOKUP                       
         BRAS  RE,DSTASPLIT                                                     
         CLI   DSTASPLT,C'Y'                                                    
         BNE   GD51D                                                            
         B     GD51B                SPLIT WEEK SITUATION                        
********************************************************************            
                                                                                
* IF LPMWK WAS THE ONLY OPTION ENTERED THEN WE HAVE TO SET                      
* DMINDEX TO Y SO THE DEMO GETS INDEXED LATER ON                                
GD51D    MVI   DMINDEX,C'N'                                                     
         CLC   LPMLKOPT,=C'YNN'                                                 
         BNE  *+8                                                               
         MVI   DMINDEX,C'Y'        SET FORCE DEMO INDEX FLAG                    
                                                                                
                                                                                
GD51E    CLC   =C'NY',WVPHOPT       OVN=Y ONLY ALWAYS GRAB                      
         BNE   *+16                                                             
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
                                                                                
* ********OVERNIGHT LOGIC*************************************                  
                                                                                
         OC    VPHHOMES,VPHHOMES   IF ANY HOMES                                 
         BNZ   GD51V2              IT'S OK                                      
*                                                                               
* NO VPHHOMES.  CHECK TO SEE IF WTP OPTION IS ON                                
* FOR WEEKLY RETRY TOR STATION CALL LETTER LINK                                 
*                                                                               
         CLI   WVPHOPT,C'Y'                                                     
         BNE   GD51F                                                            
         CLC   DBSELSTA,DSTACALL   DO WE HAVE A STATION LINK                    
         BE    GD51F               TO LOOK AT?                                  
         MVC   DBSELSTA,DSTACALL   LOOK FOR LINKED STATION                      
         MVC   DSTACALL,SVSELSTA     SWAP LINK AND  CALL LETTERS                
         MVI   DBERROR,0                                                        
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         B     GD51B                                                            
*                                                                               
*  NO SET METER WEEKLY DATA HAS BEEN FOUND                                      
GD51F    DS    0X                                                               
         CLI   OVPHOPT,C'Y'        IF NO WEEKLY METER THEN                      
         BNE   GD51I               OVN NO ASK FOR GO TO ERROR - B.E             
                                                                                
*                                                                               
GD51FA   CLI   SMMMONTH,C'Y'       SET METER READ MONTHLY OPTION?               
         JNE   GD51G                                                            
GD51FB   XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         MVI   DAYREAD,0                                                        
         XC    VPHFACT,VPHFACT                                                  
         J     GD51V2                                                           
*                                                                               
GD51G    MVI   DBSELMED,C'O'       SET THE OVERNIGHT FLAG                       
         BRAS  RE,GETVHSWK                                                      
         MVC   DBSELBK,VPHSWK      WEEKLY BOOK                                  
         MVI   ANYOVFLG,C'N'                                                    
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
*                                                                               
* CHECK TO SEE IF OVERNIGHT BOOKTYPE IS ONLY AVAILABLE                          
* AFTER A CERTAIN EFFECTIVE BOOK - AS DEFINED BY DEDEMTABS SPBOOKTB             
* IF BOOK IS BEFORE EFFECTIVE DATE THEN KEEP POSTING AS MONTHLY                 
* SO OLD POSTS WILL REMAIN THE SAME                                             
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD51H                                                            
         CLC   DBSELBK,OVEFFBK     READ MONTHLY PRIOR TO EFFECTIVE BK           
         BL    GD51FB                                                           
*                                                                               
* CALL DEMAND AND RETURN OVERNIGHT VALUE IN VPHHOMES                            
GD51H    L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETOVHOM                                        
GD51I    CLI   DEMANDSW,0           ANY OVERNIGHTS FOUND?                       
         BE    POVPHX                                                           
*                                                                               
GD51V2   MVC   DBSELBK,SVSELBK                                                  
         MVI   DBSELMED,C'T'                                                    
* IF WE ARE PROCESSING MEDIA T MAKE SURE TO RECHECK IMPRESSION BASE             
* USTV ALWAYS IMPRESSION BASED CALCULATION                                      
*******  CLI   SV1WPROF+5,C'I'     IMP BASED CALCULATED DEMOS                   
*******  BNE   *+8                                                              
         MVI   TAPEOPT,C'Y'        CHECK FOR NSI/USTV BELOW                     
******************************************************************              
GD50VPHX DS    0C                                                               
** OLYMPIC EXCLUSION CODE                                                       
         TM    CALLERF,SPXTSPWR    NON POSTING CALLERS                          
         BZ    GD51010Z            OLYMPIC EXLUSION CODE                        
         TM    SPLKOPT,SPLKOEXO    OLYMPIC EXCLUSION                            
         BZ    GD51010Z                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   GD51010Z                                                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   GD51010Z                                                         
*                                                                               
         CLI   WVPHOPT,C'Y'        IF ASKING WEEKLY NON LPM THEN                
         BE    GD51010Z            DONT BOTHER W OLY EXCLUSION                  
         CLI   DBBTYPE,C'L'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,0                                                        
         BNE   GD51010Z                                                         
*                                                                               
         MVC   SVSELBK,DBSELBK                                                  
         BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
         CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BNE   GD51O10A                                                         
         CLI   SMMMONTH,C'Y'        IS MONTHLY OVNTS REQUESTED                  
         BE    GD51O10A             CAN GET OLYMPIC EXC                         
         CLI   OVPHOPT,C'Y'         ARE REQ OVNIGHTS REQUESTED                  
         BE    GD51010Z             NO OLYMPIC EXCLUSION                        
*                                                                               
GD51O10A LA    RE,OLMEXCDT                                                      
*                                                                               
GD51O10C CLI   0(RE),X'FF'                                                      
         BE    GD51O10X                                                         
         CLC   DBSELBK,0(RE)                                                    
         BE    GD51O10D                                                         
         LA    RE,6(RE)                                                         
         B     GD51O10C                                                         
*                                                                               
GD51O10D CLC   SPLKAUST,4(RE)        START AFTER END OF SWEEP EXIT              
         BH    GD51O10F                                                         
         CLC   SPLKAUST+2(2),2(RE)   AND ENDS BEFORE START OF SWEEP             
         BL    GD51O10F                                                         
         B     GD51O10X                                                         
*                                                                               
GD51O10F CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     GD51O10X                                                         
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     GD51O10X                                                         
* ADD IN WHEN _OS GETS DEFINED                                                  
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     GD51O10X                                                         
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O1                                              
         B     GD51O10X                                                         
         MVI   DBBTYPE,C'O'                                                     
         B     GD51O10X                                                         
*                                                                               
*&&DO                                                                           
GD51O10N DS    0H                                                               
         CLC   SPLKAFST,4(RE)     END OF SWEEP                                  
         BNH   GD51O10X                                                         
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     GD51O10X                                                         
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     GD51O10N                                                         
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     GD51O10X                                                         
         MVI   DBBTYPE,C'O'                                                     
*&&                                                                             
GD51O10X DS    0H                                                               
                                                                                
GD51010Z CLI   MKTTYPE,MKTLPM                                                   
         BE    *+8                                                              
         CLI   WVPHOPT,C'P'        PEOPLE METER LOOKUP?                         
         JNE   *+8                                                              
WVPHLPM  BRAS  RE,PWVPH                                                         
GD52     DS    0C                                                               
*                                                                               
         TM    SPLKOPT,SPLKO1WB    IF ONE WEEK BUY                              
         BZ    *+10                                                             
         MVC   DBSEL1WK,SPLKADAT   SET THE DAY SO I CAN DO 1W PROGRAM           
***      BAS   RE,SETDQLNK                                                      
         BRAS  RE,SETDQLNK                                                      
*                                                                               
         TM    FLAGS,WTPDAYOP      RESEARCH WTP                                 
         BZ    *+8                                                              
         MVI   DBBEST,C'R'         WEEK START ON SATURDAY                       
*RADIO                                                                          
         CLI   DBSELMED,C'R'                                                    
         BNE   *+10                                                             
         MVC   DBSELPRG,SPLKSELP                                                
         CLI   DBSELMED,C'D'                                                    
         BNE   GD59                                                             
         OC    SPLKSELP,SPLKSELP                                                
         BZ    GD59                                                             
         MVC   DBSELDPT(L'SPLKDAY),SPLKDAY                                      
         MVC   DBSELDPT+1(L'SPLKSELP),SPLKSELP                                  
* RADAR                                                                         
GD59     CLI   DBSELSRC,C'R'                                                    
         BNE   GD60                                                             
         MVC   DBFILE,=C'RTP'                                                   
         MVC   DBSELMK,=X'0001'                                                 
*                                                                               
GD60     DS    0H                                                               
*CHECK THE EXTENSION FOR SYSCODE FOR FUSION FILE                                
* ALLOW SYSCODE FOR NSI ALSO                                                    
         MVC   SVSELDAY,DBSELDAY   SAVE DAYTIME                                 
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   GD61                                                             
         XC    DBSELSYC,DBSELSYC                                                
         ICM   RF,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GD61                                                             
         USING SPLKXTD,RF                                                       
* HAVE TO ALWAYS PASS SYSCODE INTO DEGET TO GET AIUE TO WORK                    
         MVC   DBSELSYC,SPXTSYSC   DEGET MIGHT SWITCH TO BKTYP W                
         DROP  RF                                                               
*                                                                               
GD61     CLI   DBSELMED,C'C'       TEST CANADA                                  
         BNE   GD62                                                             
* CHECK TO SEE IF OVERNIGHT                                                     
* IF SO CHECK BOOK AGAINST OVEFFBK AND SEE IF BOOK FOR THE BOOKTYPE             
* WE ARE POSTING AGAINST IS BEFORE THE AVAILABLE START BOOK                     
GD62     DS    0C                                                               
         CLI   DIGITALF,C'Y'                                                    
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD62H                                                            
         CLI   DBSELMED,C'O'                                                    
         BNE   GD62H                                                            
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD62H                                                            
         CLC   DBSELBK,OVEFFBK       POST MONTHLY PRIOR TO EFFECTIVE BK         
         BNL   GD62H                                                            
* READ MONTHLY BOOK                                                             
         DS    0C                                                               
         MVI   DBSELMED,C'T'                                                    
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         MVC   DBSELBK,SVSELBK                                                  
*                                                                               
GD62H    MVI   DBNPMFLG,0                                                       
* IF NEW METHODOLOGY PROFILE AND POSTING                                        
         IF (TM,SPLKOPT2,SPLKOPT2_NPM,O),AND,                                   
            (CLI,DBVOPT,E,X'40'),AND,(CLI,DBSELMED,E,C'C')                      
         MVI   DBNPMFLG,DBNPMACH   SET FLAG TO NPM ACHEIVED                     
         BRAS  RE,SETSPDTS         SET SPOT DATES EXTENSION                     
*                                                                               
         ENDIF                                                                  
*                                                                               
*                                                                               
GD62HH   MVI   READCUNV,0                                                       
         CLI   DBSELMED,C'U'        RADIO COUNTY COVERAGE                       
         BNE   GD62I                                                            
         XC    SVDIVSOR,SVDIVSOR                                                
         MVI   READCUNV,C'N'                                                    
GD62I    L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETDEMHK                                        
*                                                                               
* REREAD FOR COUNTY LEVEL COUNTY RECORDS                                        
         CLI   DBSELMED,C'U'                                                    
         BNE   GD62K                                                            
         CLI   READCUNV,C'Y'                                                    
         BNE   GD62J                                                            
         MVC   DBDIVSOR,SVDIVSOR                                                
         B     GD62K                                                            
GD62J    XC    DBSELSTA,DBSELSTA                                                
         MVI   READCUNV,C'Y'                                                    
         B     GD62I                                                            
*                                                                               
GD62K    MVC   DBSELDAY,SVSELDAY    RESTORE DAYTIME                             
*                                                                               
         CLI   DBSELMED,C'W'       WEEKLY JUST LOOKED UP                        
         BNE   POVPHX               NO - TREAT AS NORMAL                        
         TM    CALLERF,SPXTSPWR    NON POSTING CALLERS SKIP NEW                 
         BZ    POVPHX              POSTING CODE                                 
*************************************************                               
*  CHECK FOR POSTING OPTIONS, LPMWK, WTP, OVN   *                               
*************************************************                               
         CLI   LPMLKOPT,C'Y'                                                    
         BE    GD63                                                             
         CLI   WVPHOPT,C'Y'                                                     
         BE    GD63                                                             
         CLI   DIGITALF,C'Y'                                                    
         BE    GD63                                                             
         CLI   OVPHOPT,C'Y'                                                     
         BNE   POVPHX                                                           
* CHECK POSSIBLE SPLIT WEEK SCENARIO                                            
* TO PASS BACK WHAT WE HAVE IN THE HOOK EVEN IF ITS INOMPLETE                   
* EVEN IF WE FAILED TO GET END OF ROTATION DATA                                 
GD63     MVC   DBBEST,SVDBBEST     RESTORE DBBEST                               
         CLI   DBERROR,X'10'       IF WE GET BACK NOT FOUND ERROR               
         BNE   GD66                                                             
         CLI   ESTEOW,X'01'        FOR OOWR PASS BACK ANYTHING WE               
         BH    GD63A               GOT IN HOOK                                  
         TM    SPLKDAY,B'01111100' ATLEAST ONE DAY HAS TO BE ON                 
         BZ    GD66                BETWEEN M-F                                  
         CLI   SPLKDAY,X'03'       AND  EITHER SAT OR SUNDAY                    
         BZ    GD66                ALSO ON                                      
GD63A    CLI   DEMANDSW,C'Y'       PASS BACK WHAT WE GOT IN HOOK                
         BNE   GD66                                                             
         MVI   DBERROR,X'80'       RESET TO NO ERROR                            
***************************************************************                 
* MEDIA ALWAYS = W  WHETHER THEY ASKED FOR OVN OR LPMWK=Y                       
* OVERNIGHTS READ AFTER WEEKLY IF NECESSARY                                     
* CHECK TO SEE IF WE NEED TO READ FOR OVERNIGHTS                                
*                                                                               
GD66     CLC   =C'NNY',LPMLKOPT    OVN=Y ONLY ALWAYS GRAB                       
         BE    GD67                LPM OVERNIGHTS                               
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION WE WANT                   
         BE    GD67                LPM OVERNIGHTS                               
         CLI   OVPHOPT,C'Y'        ELSE CHECK IF WE GOT WEEKLY                  
         BNE   POVPHX                                                           
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BNE   POVPHX                                                           
GD67     MVI   DBSELMED,C'O'       NOTHING FOUND - TRY OVERNITE                 
         MVI   DEMANDSW,0          RESET SWITCH                                 
         BRAS  RE,GETVHSWK         CALL NSIWEEK TO CONVERT BK AGAIN             
         MVC   DBSELBK,VPHSWK      CONVERTED BOOK                               
         MVI   TAPEOPT,C'Y'        OVERNIGHTS IS IMPRESSION BASED               
* TO REREAD FOR OVERNIGHTS - CLEAR IUN VALUES FROM  WHAT                        
* WAS STORED FROM WEEKLY READ IF ANY                                            
GD68     LA    R0,4                RTGS/IMPS/PUTS/TOTS                          
         L     R1,ADTUNVS                                                       
         XC    0(IUNDEMS*4,R1),0(R1)                                            
         LA    R1,IUNDEMS*4(R1)                                                 
         BCT   R0,*-10                                                          
         B     GD62                                                             
***************************************************************                 
*                                                                               
POVPHX   MVC   DBBEST,SVDBBEST     RESTORE DBBEST                               
*****    MVC   SVNSIERR,DBERROR                                                 
*&&DO                                                                           
* REMOVE NO MORE DIGITAL TRANSTION                                              
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION IF OVERNIGHT              
         BNE   POVPHXX             NOT FOUND - WE WANT JUL09                    
         CLI   DBSELMED,C'O'                                                    
         BNE   POVPHXX                                                          
         CLI   DBERROR,X'80'                                                    
         BNE   *+12                                                             
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BNE   GD69                                                             
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELBK,=AL2(JUL_09)                                             
         B     GD68                                                             
*&&                                                                             
POVPHXX  DS    0C                                                               
* IF NEW METHODOLOGY PROFILE AND POSTING                                        
* AS LONG AS WE HAVE PARTIAL WEEKLY DATA FOUND- WE WANT TO GIVE BACK            
* A DEMO VALUE.                                                                 
*&&DO                                                                           
         IF (TM,SPLKOPT2,SPLKOPT2_NPM,O),AND,                                   
            (CLI,DBVOPT,E,X'40'),AND,(CLI,DBSELMED,E,C'C')                      
         IF (CLI,DEMANDSW,E,C'Y')                                               
         OC    DBDIVSOR,DBDIVSOR                                                
         BNZ   GD68K                                                            
         ENDIF                                                                  
         ENDIF                                                                  
*&&                                                                             
         IF (TM,SPLKOPT2,SPLKOPT2_NPM,O),AND,                                   
            (CLI,DBVOPT,E,X'40'),AND,(CLI,DBSELMED,E,C'C'),AND,                 
            (CLI,DEMANDSW,E,C'Y')                                               
         OC    DBDIVSOR,DBDIVSOR                                                
         BNZ   GD68K                                                            
         ENDIF                                                                  
*&&DO                                                                           
         CLI   DBERROR,X'80'       TEST EOF RETURN                              
         BNE   GDERR2                                                           
GD68K    CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BE    GDERR1              NO                                           
         SPACE 1                                                                
*&&                                                                             
         CLI   DBERROR,X'80'       TEST EOF RETURN                              
         BNE   GD68I                                                            
GD68K    CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BE    GD68I               NO                                           
         B     GD69                                                             
         SPACE 1                                                                
*                                                                               
                                                                                
* REGARDLESS IF WE GOT AN ERROR FOR NIELSEN LOOKUP                              
* ALWAYS CALL COMSCORE IF THERE ARE COMSCORE DEMOS                              
* SET THE CALLERS ERROR TO BE RETURNED FOR NIELSEN FIRST                        
*                                                                               
GD68I    DS    0H                                                               
         IF (CLI,COMPASS,E,X'01'),OR,(CLI,COMPASS,E,X'02'),ANDIF,               
            (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,DBVOPT,E,X'40'),ANDIF,                                         
            (TM,SPLKOPT2,SPLKOPT2_COMS,O)   HAVE COMSCORE DEMOS                 
         BRAS  RE,GDERRNSI                                                      
         B     GDCOMS10                                                         
         ENDIF                                                                  
         B     GDERR1                                                           
*                                                                               
                                                                                
*                                                                               
         SPACE 1                                                                
* EXTRACT REQUESTED DEMOS *                                                     
         SPACE 1                                                                
GD69     MVC   DBTAPEP,TAPEOPT                                                  
         BAS   RE,GETIUN                                                        
*                                                                               
         MVC   LPMLKOPT,SVLPMLKOPT RESTORE ORIGINAL OPTIONS                     
         MVC   WVPHOPT,SVWVPHOPT                                                
* CALL COMINTER IF PASS1 OR PASS2 , CALLER IS WRITER AND POSTING                
* AGAINST NIELSEN                                                               
GDCOMS10 DS    0C                                                               
         IF (CLI,COMPASS,E,X'01'),OR,(CLI,COMPASS,E,X'02'),ANDIF,               
            (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,DBVOPT,E,X'40'),ANDIF,                                         
            (TM,SPLKOPT2,SPLKOPT2_COMS,O)   HAVE COMSCORE DEMOS                 
         GOTOR COMSCORE,DMCB,=C'ACHVD',DBLOCK                                   
         L     R1,SAVER1                      A(USER'S PARMS)                   
         L     RF,0(R1)                       GET A(SPDEMLK)                    
         IF    (TM,SPLKOPT2,SPLKOPT2_NLSN,Z)                                    
         MVC   SPLKPRG-SPDEMLK(L'STNAME,RF),STNAME                              
         ENDIF                                                                  
*                                                                               
         CLI   COMERRF,0                                                        
         BE    GDCOMS20                                                         
         OI    SPLKCERR,SPLKCBAD                                                
         MVI   SPLKCERR-SPDEMLK(RF),SPLKCBAD  SET COMSCORE ERROR                
* CHECK COMINTER ERROR CODE.                                                    
*                                                                               
GDCOMS20 BAS   RE,GETADJ             GET SVI VALUES                             
         LA    R1,DTDEMSA                                                       
         BAS   RE,GDXSVI                                                        
         ENDIF                                                                  
*                                                                               
*                                                                               
         CLI   DBSELMED,C'R'                                                    
         BNE   GD70                                                             
         MVC   DTDEMSA,RADEMSA                                                  
         MVC   DTDEMSU,RADEMSU                                                  
         EJECT                                                                  
* SET BUY VALUES = DAY TIME VALUES *                                            
         SPACE 1                                                                
*                                                                               
GD70     MVC   BUDEMSA,DTDEMSA                                                  
         MVC   BUDEMSU,DTDEMSU                                                  
         SPACE 1                                                                
* RETURN DEMO AND SVI VALUES *                                                  
         SPACE 1                                                                
GD72     L     R1,SAVER1                                                        
         L     RF,0(R1)                       GET A(SPLK BLOCK)                 
         MVC   SPLKABK-SPDEMLK(2,RF),SVACTBK  SET ACTUAL BOOK                   
                                                                                
         TM    DBVOPT,X'40'        SPOT POSTING?                                
         BZ    GD75                                                             
         OC    STNAMEOV,STNAMEOV   USE OVERNIGHTS PROGRAM NAMES                 
         BZ    GD75                                                             
         MVC   STNAME,STNAMEOV                                                  
         MVC   ENDNAME,ENDNAMEO                                                 
GD75     DS    0H                                                               
*                                                                               
         LA    RE,SPLKPRG-SPDEMLK(RF)                                           
         MVC   0(15,RE),STNAME                                                  
         CLC   STNAME,ENDNAME                                                   
         BE    *+14                                                             
         MVI   7(RE),C'/'                                                       
         MVC   8(7,RE),ENDNAME                                                  
         SPACE 1                                                                
* DIVIDE ADJ DEMS BY UNADJ DEMS TO GET ACTUAL SVIS *                            
         SPACE 1                                                                
         LA    R1,BUDEMSA                                                       
         BRAS  RE,GDSVI                                                         
         SPACE 1                                                                
* NOW RETURN ==ADJ== DEMO VALUES AND SVIS IN USER AREA *                        
         SPACE 1                                                                
         LA    R1,BUDEMSA                                                       
**       BAS   RE,SETVALS                                                       
         BRAS  RE,SETVALS                                                       
         SPACE 1                                                                
* CALCULATE WEIGHTED DEMO IF REQUIRED *                                         
         SPACE 1                                                                
GD76     OC    SAVEWGTS,SAVEWGTS   TEST ANY WEIGHTS PRESENT                     
         BZ    GD78                NO - SKIP WEIGHTED DEMO CALC                 
****     BAS   RE,GETWD                                                         
         BRAS  RE,GETWD                                                         
         SPACE 2                                                                
GD78     DS    0H                                                               
         BAS   RE,RSTDQLNK                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
*                                                                 *             
*                      AFFIDAVIT LOOK-UPS                         *             
*                                                                 *             
*******************************************************************             
         SPACE 1                                                                
GD100    OC    SPLKASPT,SPLKASPT   TEST FOR SPOT TABLE LOOKUP                   
         BNZ   GD200               YES                                          
         CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BNE   GD130                                                            
         SPACE 1                                                                
* POL AFFIDAVIT PROCESSING *                                                    
         SPACE 1                                                                
         LA    R6,BDELEM                                                        
GD110    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
GD112    CLI   0(R6),0                                                          
         BE    GD150                                                            
         CLI   0(R6),X'0B'                                                      
         BL    GD110                                                            
         CLI   0(R6),X'0D'                                                      
         BH    GD110                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   GD110                                                            
         TM    6(R6),X'C4'         TEST HIATUS,MINUS,MINUSSED                   
         BNZ   GD110                                                            
         SPACE 1                                                                
* TEST IN REQUEST PERIOD *                                                      
         SPACE 1                                                                
         CLC   2(2,R6),AFDSDT                                                   
         BL    GD110                                                            
         CLC   2(2,R6),AFDEDT                                                   
         BH    GD110                                                            
*                                                                               
         LA    R1,BDSEC                                                         
         CLI   AFDPRD,X'FF'        TEST POL REQ                                 
         BE    GD114               YES                                          
         LA    R1,11(R6)           POINT TO PRD 1 SLN                           
         CLC   AFDPRD,10(R6)       MATCH PRD 1                                  
         BE    GD114                                                            
         CLI   1(R6),18            TEST P/B SPOT                                
         BL    GD110               NO                                           
         LA    R1,15(R6)           POINT TO PRD 2 SLN                           
         CLC   AFDPRD,14(R6)                                                    
         BNE   GD110                                                            
         EJECT                                                                  
GD114    CLI   AFDSLN,0            TEST ALL SLN REQ                             
         BE    *+14                                                             
         CLC   AFDSLN,0(R1)                                                     
         BNE   GD110                                                            
         SPACE 2                                                                
* TEST FOR AFFIDAVIT *                                                          
         SPACE 1                                                                
GD116    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GD120                                                            
         CLI   0(R6),X'0B'         TEST REGEL (0B-0D)                           
         BL    GD116                                                            
         CLI   0(R6),X'0D'                                                      
         BNH   GD120               YES - NO AFFID                               
         CLI   0(R6),X'10'                                                      
         BNE   GD116                                                            
         SPACE 1                                                                
* AFFID PRESENT - ADD TO DAY/TIME LIST AND COUNTER *                            
         SPACE 1                                                                
         BAS   RE,GETAFD           GET DEMOS                                    
         LH    RE,AFDCNT                                                        
         LA    RE,1(RE)                                                         
         STH   RE,AFDCNT                                                        
         B     GD110                                                            
         SPACE 1                                                                
* NO AFFID - ADD TO COUNTER *                                                   
         SPACE 1                                                                
GD120    LH    RE,NOAFDCNT                                                      
         LA    RE,1(RE)                                                         
         STH   RE,NOAFDCNT                                                      
         B     GD112               NOTE - POINTING AT NEXT REGEL                
         EJECT                                                                  
* NON-POL AFFIDAVIT PROCESSING *                                                
         SPACE 2                                                                
GD130    CLI   BDTIME,0            TEST PIGGYBACK RECORD                        
         BE    GD140               NO                                           
         SPACE 1                                                                
* FIND PBELEM *                                                                 
         SPACE 1                                                                
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4                                                         
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   R0,1(R6)                                                         
         SRDA  R0,32                                                            
         D     R0,=F'7'                                                         
         LR    R0,R1               SET R1 FOR BCT                               
         LA    R6,2(R6)            POINT TO PTR 1                               
*                                                                               
GD132    LA    R1,2(R6)            POINT TO SLN (TIME SHARE)                    
         CLC   AFDPRD,0(R6)        TEST RIGHT PRD                               
         BE    GD134                                                            
         LA    R6,7(R6)                                                         
         BCT   R0,GD132                                                         
         SPACE 1                                                                
* MUST BE PROCESSING ACTIVE PARTNER *                                           
         SPACE 1                                                                
         LA    R1,BDTIME                                                        
         CLC   AFDPRD,BUYREC+3                                                  
         BE    GD134                                                            
         DC    H'0'                                                             
*                                                                               
GD134    CLI   AFDSLN,0            TEST ALL SLN REQUEST                         
         BE    GD140                                                            
         CLC   AFDSLN,0(R1)        TEST RIGHT SLN                               
         BNE   EXIT                                                             
         EJECT                                                                  
GD140    LA    R6,BDELEM                                                        
*                                                                               
GD141    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
GD142    CLI   0(R6),0                                                          
         BE    GD150                                                            
         CLI   0(R6),6                                                          
         BL    GD141                                                            
         CLI   0(R6),8                                                          
         BH    GD141                                                            
         SPACE 1                                                                
* TEST ELEMENT IN REQUESTED PERIOD *                                            
         SPACE 1                                                                
         CLC   2(2,R6),AFDSDT      TEST BEFORE PERIOD START                     
         BL    GD141                                                            
         CLC   2(2,R6),AFDEDT      OR AFTER PERIOD END                          
         BH    GD141                                                            
*                                                                               
         LLC   R0,7(R6)            GET NUMBER OF SPOTS                          
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AH    R0,NOAFDCNT                                                      
         STH   R0,NOAFDCNT                                                      
         SPACE 1                                                                
* NOW LOOK FOR AFFIDAVITS FOR THIS ELEMENT *                                    
         SPACE 1                                                                
GD144    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GD150                                                            
         CLI   0(R6),6             TEST REGEL (06-08)                           
         BL    GD144                                                            
         CLI   0(R6),8                                                          
         BNH   GD142               YES - NO MORE AFFIDS                         
         CLI   0(R6),X'10'                                                      
         BNE   GD144                                                            
         SPACE 1                                                                
* AFFID PRESENT - LOOK UP DEMOS *                                               
         SPACE 1                                                                
         BAS   RE,GETAFD           GET DEMOS                                    
*                                                                               
         LH    RE,AFDCNT           ADD TO AFFID COUNT                           
         AHI   RE,1                                                             
         STH   RE,AFDCNT                                                        
         LH    R0,NOAFDCNT         REDUCE MISSING AFFID COUNT                   
         BCTR  R0,0                                                             
         STH   R0,NOAFDCNT                                                      
         B     GD144                                                            
         EJECT                                                                  
* ALL AFFIDS COMPLETE - TEST FOR MISSING AFFIDS *                               
         SPACE 1                                                                
GD150    DS    0H                                                               
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GD150AA                                                          
         USING SPLKXTD,RE                                                       
         MVC   ESTEOW,SPXTEOWD                                                  
         TM    SPXTFLG2,SPXTSPWR                                                
         BZ    *+12                                                             
         OI    DBVOPT,X'40'        SET FLAG TO INDICATE SPOT POSTING            
         MVI   CALLERF,SPXTSPWR    SAVE CALLER INDICATOR                        
         MVC   COMCSD,SPXTCSD      COMSCORE SURVEY DATE                         
         MVC   ASPXT50E,SPXT50EL   A(50 ELEMENT)                                
         MVC   REQSDATE,SPXTSTDT   REPORT REQUEST START DATE                    
         MVC   REQEDATE,SPXTENDT   REPORT REQUEST END DATE                      
         DROP   RE                                                              
*                                                                               
GD150AA  MVC   BUDEMSA,AFDEMSA     SET BUY DEMS = AFD DEMS                      
         MVC   BUDEMSU,AFDEMSU                                                  
         XC    DBSEL1WK,DBSEL1WK                                                
         MVI   DBSELWKN,0                                                       
         SR    R0,R0                                                            
         ICM   R0,3,NOAFDCNT                                                    
         BZ    GD160               NONE MISSING                                 
         SPACE 1                                                                
* AFFIDS MISSING - DO DEMO LOOK-UP FOR BUY DAY/TIME *                           
         SPACE 1                                                                
         MVC   DBSELDAY,BDDAY      SET DAY(S)                                   
         MVC   DBSELTIM,BDPROG     SET START/END TIMES                          
         MVI   DEMANDSW,0          RESET SWITCH                                 
         MVC   DBTAPEP,TAPEOPT                                                  
*                                                                               
         TM    FLAGS,WTPDAYOP      RESEARCH WTP                                 
         BZ    *+8                                                              
         MVI   DBBEST,C'R'         WEEK START ON STAURDAY                       
*                                                                               
         CLI   DBSELMED,C'R'                                                    
         BNE   GD150A                                                           
         XC    RADEMSA,RADEMSA                                                  
         XC    RADEMSU,RADEMSU                                                  
         XC    RADEMSU2,RADEMSU2                                                
*                                                                               
GD150A   DS    0H                                                               
*============== CHECK POSTING OPTIONS ============================              
*                                                                               
         CLC   =C'YNN',LPMLKOPT   IF USER REQUESTED LPMWK OPTION  ONLY          
         BNE   *+14               AND WE ARE NOT DEALING WITH AN LPM            
         OC    LPMDTP,LPMDTP      MARKET, WE WANT TO READ MONTHLY               
         BZ    GD150VPX                                                         
         CLI   LPMLKOPT,C'Y'                                                    
         BE    GD150B                                                           
         CLI   WVPHOPT,C'Y'                                                     
         BE    GD150B                                                           
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD150VPX                                                         
******************************************************************              
* YOU SHOULD ONLY BE HERE FOR POSTING                                           
******************************************************************              
*                                                                               
GD150B   DS    0H                                                               
*                                                                               
         CLI   DBBTYPE,0           ALWAYS ALLOW STANDARD                        
         BE    GD150BH                                                          
         CLI   LPMLKOPT,C'Y'       IF WE ARE POSTING WEEKLY THEN CHECK          
         BE    *+8                 BOOKTYPE INDICATOR TO SEE IF WEEKLY          
         CLI   WVPHOPT,C'Y'        IS AVAILABLE FOR THIS BOOKTYPE               
         BNE   *+12                IF ITS NOT AN AVAILABLE WEEKLY               
         TM    BKTYPIND,SPBKWKLY   BOOKTYPE THEN CHECK TO SEE IF                
         BNZ   GD150BH             OVERNIGHT POSTING OPTION IS ON               
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD150BG                                                          
         TM    BKTYPIND,SPBKOVN    BOOKTYPE AVAILABLE FOR OVERNIGHT?            
         BNZ   GD150BH                                                          
GD150BG  MVI   DBSELMED,C'T'       IF NOT AVAILABLE-GO READ MONTHLY             
* RESET WEEKLY AND OVERNGIGHT VALUES FROM PREVIOUS READ                         
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         B     GD150VPX                                                         
*                                                                               
*                                                                               
GD150BH  OC    DBSELSYC,DBSELSYC  CABLE IS ONLY MONTHLY FOR NOW                 
         BNZ   GD150VPX                                                         
         OC    DBSELMK,DBSELMK                                                  
         BNZ   GD150VPX                                                         
         MVI   DBSELMED,C'W'                                                    
         MVC   SVSELBK,DBSELBK     BOOK FORMAT IS YY00                          
         BRAS  RE,GETVDAT          GET WTP YEAR                                 
*******************************************************************             
*  CONVERT THE BOOK TO GET THE CORRECT YEAR,,COULD BE END OF YR BUT             
*  ITS STILL CAN BE 1ST WEEK OF NEXT YR ON THE FILE                             
*******************************************************************             
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DUB),(0,DUB3)                                       
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 VNSIWEEK,DMCB,DUB3                                               
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
*                                                                               
* CHECK FOR LPM MARKET                                                          
         TM    CALLERF,SPXTSPWR    NON POSTING CALLERS LIKE $DEM                
         BZ    GD150C              ALWAYS GETS VPH IF WVPHOPT IS ON             
         OC    LPMDTP,LPMDTP       ANY LPM DATE                                 
         BZ    GD150C               NO - USE WEEKLY HOMES TO ADJUST             
         CLC   DUB(2),LPMDTB        MONTH GREATER THAT LPM                      
         BL    GD150C                                                           
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0           IF LPM ALREADY NO BOOKTYPE                   
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
         MVI   MKTTYPE,MKTLPM                                                   
         B     GD150VPX             YES - GET LPM DEMOS                         
*----------------   NON LPM SECTION -------------------------------             
* AT THIS POINT- NOT LPM - CHECK TO SEE IF WE ARE READING SET METERED           
* MARKET.  IF SET METERED THEN PROCEED ELSE WE ARE DIARY MARKET                 
* DIARY MARKETS IGNORE  WTP AND OTP OPTIONS AND PROCEED TO READ                 
* BOOK OPTION OR ACT BOOK.                                                      
GD150C   OC    LPMDTP,LPMDTP        IF WE HAVE LPM DATE WE MUST HAVE            
         BZ    GD150CC                                                          
* IF WTP/OVN OPTION TURNED ON READ SET METERED-ELSE READ MONTHLY                
         CLC   =C'NN',WVPHOPT                                                   
         BE    GD150I               READ MONTHLY                                
         MVI   MKTTYPE,MKTSETM                                                  
         B     GD150D               BEEN SET METERED BEFORE                     
GD150CC  BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
GD150D   CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BE    GD150DD                                                          
         MVI   WVPHOPT,C'N'         DIARY MKT READ MONTHLY                      
         MVI   OVPHOPT,C'N'                                                     
* RESET WEEKLY AND OVERNGIGHT VALUES FROM PREVIOUS READ                         
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         B     GD150I                                                           
*                                                                               
GD150DD  CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0           IF LPM ALREADY NO BOOKTYPE                   
*                                                                               
         MVI   DBSELBK+1,0         READ WEEKLY ZERO RECORD                      
*                                                                               
*                                                                               
*                                                                               
*&&DO                                                                           
         CLI   DBSELBK,98                                                       
         BNE   *+14                                                             
         CLC   =C'WKMG',DBSELSTA   DOUBLE CALL LETTER SWITCH                    
         BE    GD150VPX            NOT HANDLED                                  
*&&                                                                             
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         MVI   NEXTYRLK,C'N'                                                    
***      BAS   RE,SETDQLNK                                                      
         BRAS  RE,SETDQLNK                                                      
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
*                                                                               
GD150DF  L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETVHOME                                        
         TM    CALLERF,SPXTSPWR    NON POSTING CALLER SKIP                      
         BZ    GD150I              CODE                                         
                                                                                
         MVI   MISCOUNT,0          CLEAR MISSING DATA COUNT AND DAY             
         MVI   MISSDAY,0                                                        
* IF LPMWK WAS THE ONLY OPTION ENTERE THAN WE HAVE TO SET                       
* DMINDEX TO 'Y' SO THE DEMO GETS INDEXED LATER ON                              
                                                                                
         MVI   DMINDEX,C'N'                                                     
         CLC   LPMLKOPT,=C'YNN'                                                 
         BNE  *+8                                                               
         MVI   DMINDEX,C'Y'        SET FORCE DEMO INDEX FLAG                    
*                                                                               
* ********OVERNIGHT LOGIC*************************************                  
*                                                                               
         CLC   =C'NY',WKOVOPT      OVN=Y ONLY ALWAYS GRAB                       
         BNE   *+16                                                             
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
                                                                                
         OC    VPHHOMES,VPHHOMES   IF ANY HOMES                                 
         BNZ   GD150I              IT'S OK                                      
         CLI   OVPHOPT,C'Y'        IF NO WEEKLY GET OVERNIGHTS                  
         BNE   GD150H              NO OVN ASK FOR GO TO B.E                     
*                                                                               
         CLI   SMMMONTH,C'Y'       SET METER READ MONTHLY OPTION?               
         JNE   GD150E                                                           
GD150DG  XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         MVI   DAYREAD,0                                                        
         XC    VPHFACT,VPHFACT                                                  
         J     GD150I                                                           
*                                                                               
GD150E   MVI   DBSELMED,C'O'       SET THE OVERNIGHT FLAG                       
         BRAS  RE,GETVHSWK                                                      
         MVC   DBSELBK,VPHSWK      WEEKLY BOOK                                  
* GETVHSWK NOW CALLS NSIWEEK AND TRANSLATE THE WEEKS CORRECTLY                  
* FOR OVERNIGHTS                                                                
         DS    0X                                                               
         MVI   DBSELMED,C'O'       SET THE OVERNIGHT FLAG                       
         MVI   ANYOVFLG,C'N'                                                    
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
*                                                                               
* CHECK TO SEE IF OVERNIGHT BOOKTYPE IS ONLY AVAILABLE                          
* AFTER A CERTAIN EFFECTIVE BOOK - AS DEFINED BY DEDEMTABS SPBOOKTB             
* IF BOOK IS BEFORE EFFECTIVE DATE THEN KEEP POSTING AS MONTHLY                 
* SO OLD POSTS WILL REMAIN THE SAME                                             
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD150G                                                           
         CLC   DBSELBK,OVEFFBK   READ MONTHLY IF PRIOR TO EFFECTIVE BK          
         BL    GD150DG                                                          
*                                                                               
* CALL DEMAND AND RETURN OVERNIGHT VALUE IN VPHHOMES                            
GD150G   L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETOVHOM                                        
GD150H   CLI   DEMANDSW,0         IF NO OVERNIGHTS FOUND                        
         BE    POVPHX3            GO TO B.E                                     
                                                                                
GD150I   MVC   DBSELBK,SVSELBK                                                  
         MVI   DBSELMED,C'T'                                                    
*                                                                               
GD150VPX DS    0C                                                               
******************************************************************              
         CLI   MKTTYPE,MKTLPM                                                   
         BE    *+8                                                              
         CLI   WVPHOPT,C'P'        PEOPLE METER LOOKUP?                         
         JNE   *+8                                                              
WVPHLPM2 BRAS  RE,PWVPH                                                         
GD150ORT L     R0,SPLKHOOK                                                      
         XC    SPLKHOOK,SPLKHOOK   SUPPRESS DEMAND HOOK                         
***      BAS   RE,SETDQLNK                                                      
         BRAS  RE,SETDQLNK                                                      
*                                                                               
* CHECK TO SEE IF OVERNIGHT                                                     
* IF SO CHECK BOOK AGAINST OVEFFBK AND SEE IF BOOK FOR THE BOOKTYPE             
* WE ARE POSTING AGAINST IS BEFORE THE AVAILABLE START BOOK                     
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD150L                                                           
         CLI   DBSELMED,C'O'                                                    
         BNE   GD150L                                                           
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD150L                                                           
         CLC   DBSELBK,OVEFFBK       POST MONTHLY PRIOR TO EFFECTIVE BK         
         BNL   GD150L                                                           
* READ MONTHLY BOOK                                                             
         DS    0C                                                               
         MVI   DBSELMED,C'T'                                                    
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         MVC   DBSELBK,SVSELBK                                                  
         B     GD150L                                                           
*                                                                               
GD150L   L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETDEMHK                                        
         ST    R0,SPLKHOOK         RESTORE USER HOOK ADDRESS                    
*****************************************************************               
* CHECK TO SEE IF WE SHOULD READ FOR OVERNIGHTS                                 
*****************************************************************               
         CLI   DBSELMED,C'W'       WEEKLY JUST LOOKED UP                        
         BNE   POVPHX3              NO - TREAT AS NORMAL                        
         TM    CALLERF,SPXTSPWR    NON POSTING CALLER SKIP NEW                  
         BZ    POVPHX3             POSTING CODE                                 
*                                                                               
* SEE IF POSSIBLE SPLIT WEEK SCENARIO                                           
         CLI   DBERROR,X'10'       IF WE GET BACK NOT FOUND ERROR               
         BNE   GD150M                                                           
         TM    SPLKDAY,B'01111100' ATLEAST ONE DAY HAS TO BE ON                 
         BZ    GD150M              BETWEEN M-F                                  
         CLI   SPLKDAY,X'03'       AND  EITHER SAT OR SUNDAY                    
         BZ    GD150M              ALSO ON                                      
         CLI   DEMANDSW,C'Y'       PASS BACK WHAT WE GOT IN HOOK                
         BNE   GD150M                                                           
         MVI   DBERROR,X'80'       RESET TO NO ERROR                            
         B     POVPHX3                                                          
*                                                                               
GD150M   CLC   =C'NNY',LPMLKOPT    OVN=Y ONLY ALWAYS GRAB                       
         BE    GD151               LPM OVERNIGHTS                               
         CLI   OVPHOPT,C'Y'        OVN=Y OPTION                                 
         BNE   POVPHX3                                                          
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BNE   POVPHX3                                                          
GD151    MVI   DBSELMED,C'O'       NOTHING FOUND - TRY OVERNITE                 
         MVI   DEMANDSW,0          RESET SWITCH                                 
         BRAS  RE,GETVHSWK         CALL NSIWEEK TO CONVERT BK AGAIN             
         MVC   DBSELBK,VPHSWK      ONVERTED BOOK                                
         MVI   TAPEOPT,C'Y'        OVERNIGHTS IS IMPRESSION BASED               
* TO REREAD FOR OVERNIGHTS - CLEAR IUN VALUES FROM  WHAT                        
* WAS STORED FROM WEEKLY READ IF ANY                                            
         LA    R0,4                RTGS/IMPS/PUTS/TOTS                          
         L     R1,ADTUNVS                                                       
         XC    0(IUNDEMS*4,R1),0(R1)                                            
         LA    R1,IUNDEMS*4(R1)                                                 
         BCT   R0,*-10                                                          
         B     GD150ORT                                                         
********************************************************************            
POVPHX3  CLI   DBERROR,X'80'       TEST EOF RETURN                              
         BNE   GDERR2                                                           
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BE    GDERR1              NO                                           
         SPACE 1                                                                
* EXTRACT REQUESTED DEMOS *                                                     
         SPACE 1                                                                
         MVC   DBTAPEP,TAPEOPT                                                  
         BAS   RE,GETIUN                                                        
                                                                                
         CLI   DBSELMED,C'R'                                                    
         BNE   *+16                                                             
         MVC   DTDEMSA,RADEMSA                                                  
         MVC   DTDEMSU,RADEMSU                                                  
         SPACE 1                                                                
* WEIGHT BUY DEMOS BY NUMBER OF SPOTS AND ADD TO AFFID TOTALS *                 
         SPACE 1                                                                
         LA    R0,MAXDEMS*2                                                     
         SR    R1,R1                                                            
*                                                                               
GD152    L     RE,DTDEMSA(R1)      DEMOS JUST LOOKED UP                         
         MH    RE,NOAFDCNT                                                      
         A     RE,AFDEMSA(R1)      ADD TO AFD TOTALS                            
         ST    RE,BUDEMSA(R1)      SET IN BUY TOTALS                            
         LA    R1,4(R1)                                                         
         BCT   R0,GD152                                                         
         SPACE 1                                                                
* SET DATA FOR USER *                                                           
         SPACE 1                                                                
         MVI   SPLKADAT,X'FF'      SET AFFID DATE = FF                          
         MVI   SPLKADAT+1,X'FF'                                                 
         MVC   SPLKABK,SVACTBK     SET ACTUAL BOOK                              
         LA    RE,SPLKPRG                                                       
         MVC   0(15,RE),STNAME                                                  
         CLC   STNAME,ENDNAME                                                   
         BE    *+14                                                             
         MVI   7(RE),C'/'                                                       
         MVC   8(7,RE),ENDNAME                                                  
         MVI   SPLKDAY,X'FF'       SET DAY = X'FF'                              
         MVC   SPLKSPTS,NOAFDCNT   AND NUMBER OF SPOTS                          
         MVI   SPLKWKLY,C'N'       CERTAINLY NOT WEEKLY                         
*                                                                               
         L     R1,SAVER1                                                        
         L     RF,0(R1)                       GET A(SPLK BLOCK)                 
         MVC   0(L'SPDEMLK,RF),SPDEMLK        COPY BLOCK TO USER'S              
         SPACE 1                                                                
* DIVIDE ADJ DEMS BY UNADJ DEMS TO GET ACTUAL SVIS *                            
         SPACE 1                                                                
         LA    R1,DTDEMSA                                                       
         BRAS  RE,GDSVI                                                         
         SPACE 1                                                                
* NOW RETURN ==ADJ== DEMO VALUES AND SVIS IN USER AREA *                        
         SPACE 1                                                                
         LA    R1,DTDEMSA                                                       
         BRAS  RE,SETVALS                                                       
         SPACE 1                                                                
* CALCULATE WEIGHTED DEMO IF REQUIRED *                                         
         SPACE 1                                                                
         OC    SAVEWGTS,SAVEWGTS   TEST ANY WEIGHTS PRESENT                     
         BZ    *+8                                                              
***      BAS   RE,GETWD                                                         
         BRAS  RE,GETWD                                                         
*                                                                               
         BAS   RE,CALLHOOK                                                      
         SPACE 1                                                                
* NOW RETURN TOTAL VALUES IN USER AREA *                                        
         SPACE 1                                                                
GD160    LH    R0,AFDCNT                                                        
         AH    R0,NOAFDCNT                                                      
         L     R1,SAVER1                                                        
         L     RF,0(R1)                    GET A(SPLK BLOCK)                    
         STCM  R0,3,SPLKSPTS-SPDEMLK(RF)   SET NUMBER OF SPOTS                  
*                                                                               
         LA    R1,BUDEMSA                                                       
         BRAS  RE,GDSVI                                                         
*                                                                               
         LA    R1,BUDEMSA                                                       
         BRAS  RE,SETVALS                                                       
         BAS   RE,RSTDQLNK                                                      
         B     EXIT                        AND EXIT                             
         EJECT                                                                  
* LOOK UP AFFIDAVIT DEMOS USING SPOT TABLE                                      
*                                                                               
GD200    L     R6,SPLKASPT                                                      
         USING SPTTABD,R6                                                       
         LH    R0,SPLKNSPT         R0=N'TABLE ENTRIES                           
         LLC   R5,SPLKLSPT         R5=L'TABLE ENTRY                             
*                                                                               
GD202    OC    SPTADATE,SPTADATE   TEST FOR AFFIDAVIT DATE                      
         BZ    GD204               NO                                           
         TM    SPTIND,SPTNODEM     TEST FOR SUPPRESSED DEMO LOOKUP              
         BO    GD204               YES                                          
         CLI   SPLKNTI,C' '        SUPPORT FOR NTI CABLE LOOKUPS                
         BH    GD202B                                                           
*                                                                               
* THE FOLLLOWING STATION ONLY HAVE MONTHLY DATA                                 
* BECAUSE THERE IS NO OVERNIGHT DATA SPECIAL CODE IS NEEDED                     
*                                                                               
         CLC   SPLKSTA(4),=CL4'PACO'  SPECIAL CODE TO HANDLE UNIQUE             
         BE    GD202A                                                           
         CLC   SPLKSTA(4),=CL4'PACB'  SPECIAL CODE TO HANDLE UNIQUE             
         BE    GD202A                                                           
*&&DO                                                                           
* COMSCORE WE DONT CARE ABOUT THE ACTBOOK- ALWAYS PROCESS                       
* THE REQUEST                                                                   
         IF (CLI,COMPASS,E,X'01'),AND,(CLI,DBVOPT,E,X'40')                      
         B     GD202A                                                           
         ENDIF                                                                  
*&&                                                                             
         MVI   ANYACTBK,C'Y'                                                    
         OC    SPTACTBK,SPTACTBK   TEST FOR ACTUAL BOOK                         
         BNZ   GD202A              YES                                          
         MVI   ANYACTBK,C'N'                                                    
         CLC   SPLKSTA,=C'PACAT'   DUMMY CALL LETTER PACAT?                     
         BNE   GD204               NO                                           
GD202A   MVC   DBSELSTA,SPLKSTA    RESET STATION EACH TIME                      
         B     GD203                                                            
*                                                                               
GD202B   MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'C'                                                    
         MVC   DBSELSTA,SPLKNTI                                                 
         MVI   DBSELSTA+4,C'C'                                                  
* PREPARE TO CALL NETWEEK                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,SPTADATE),WORK                                      
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     R1,CGETDAY-COMFACSD(RF)                                          
         ST    R1,DMCB+4                                                        
         L     R1,CADDAY-COMFACSD(RF)                                           
         ST    R1,DMCB+8                                                        
         CLC   WORK(6),=C'      '  HAVE A DATE                                  
         BNE   *+10                 YES - OK                                    
         MVC   WORK,=C'840101'      NO - FORCE A MISS                           
         GOTO1 VNETWEEK,DMCB,WORK                                               
*                                                                               
*                                                                               
         MVC   DBSELBK(1),DMCB+4   MOVE YEAR                                    
         MVC   DBSELBK+1(1),DMCB+8 MOVE WEEK NUMBER                             
         XC    DBSEL1WK,DBSEL1WK   CLEAR WEEKLY REQUEST FIELDS                  
*                                                                               
GD203    BAS   RE,GETAFD           GET THE AFFIDAVIT DEMOS                      
*                                                                               
GD204    LA    R6,0(R5,R6)         POINT TO NEXT ENTRY                          
         BCT   R0,GD202                                                         
         BAS   RE,RSTDQLNK                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* LOOK UP DEMOS FOR AFFIDAVIT                                                   
* AT ENTRY, R6=A(AFFIDAVIT ELEMENT) OR A(SPOT TABLE ENTRY)                      
*                                                                               
         SPACE 1                                                                
GETAFD   NTR1                                                                   
* ALWAYS RESET DBBTYPE TO REQUESTED BOOKTYPE BECAUSE DBBTYPE                    
* COULD HAVE BEEN SWITCHED IN DEMO SYSTEM FROM PREVIOUS AFFID CALL              
         MVC   DBBTYPE,SPLKBTYP                                                 
*                                                                               
*                                                                               
         MVC   SVDBBEST,DBBEST    SAVE AWAY DBBEST                              
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVI   AFDSW,C'Y'          SET INDICATOR                                
         TM    AFDOPT,AFDO7MIN     TEST 7 MINUTE OPTION                         
         BZ    *+8                                                              
         BAS   RE,SET7                                                          
*                                                                               
         OC    SPLKASPT,SPLKASPT   TEST SPOT TABLE LOOKUP                       
         BNZ   GETAFD0             YES                                          
AF       USING AFFELEM,R6                                                       
         MVC   AFDATE,AF.ADATE     EXTRACT AFFID DATE AND TIME                  
         MVC   AFDTIME,AF.ATIME    FROM ELEMENT                                 
         NI    AFDTIME,X'0F'       DROP HOB                                     
         MVC   AFDBOOK,DBSELBK                                                  
         B     GETAFD0A                                                         
         DROP  AF                                                               
*                                                                               
         USING SPTTABD,R6                                                       
GETAFD0  MVC   AFDATE,SPTADATE     EXTRACT AFFID DATA FROM                      
         MVC   AFDTIME,SPTATIME    TABLE ENTRY                                  
         MVC   AFDBOOK,SPTACTBK                                                 
*                                                                               
GETAFD0A TM    SPLKOPT,SPLKOLOW    TEST LOOKUP LOWER QH FEATURE                 
         BZ    GETAFD0B                                                         
         GOTO1 =A(LQHSET),DMCB,(RC),RR=RELO  YES-CHANGE QH TIMES                
         B     GETAFD1                                                          
*                                                                               
GETAFD0B TM    AFDOPT,AFDO7MIN     TEST 7 MINUTE OPTION IN PLAY                 
         BZ    GETAFD1                                                          
         OC    AF7QHSET,AF7QHSET                                                
         BZ    GETAFD1                                                          
         CLC   AFDTIME,AF7QHST     YES-COMPARE TO LAST QH START                 
         BL    GETAFD1             PRIOR-IGNORE                                 
         CLC   AFDTIME,AF7QHEND    COMPARE TO LAST QH END                       
         BH    GETAFD1             AFTER-IGNORE                                 
         MVC   AFDTIME,AF7QHSET    MOVE 'SET' TIME VALUE                        
*                                                                               
GETAFD1  L     RF,DBCOMFCS            GET YYMMDD VIA DATCON                     
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,AFDATE),WORK                                        
*                                                                               
         L     RF,DBCOMFCS              GET DAY VIA GETDAY                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,WORK,WORK+6                                            
*                                                                               
         LLC   R5,0(R1)            GET DAY                                      
         IC    R5,DAYTBL-1(R5)                                                  
         STC   R5,DBSELDAY                                                      
         MVC   DBSELTIM(2),AFDTIME SET START TIME ONLY                          
*                                                                               
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GETAFDAA                                                         
         USING SPLKXTD,RE                                                       
         MVC   ESTEOW,SPXTEOWD                                                  
         TM    SPXTFLG2,SPXTSPWR                                                
         BZ    *+12                                                             
         OI    DBVOPT,X'40'        SET FLAG TO INDICATE SPOT POSTING            
         MVI   CALLERF,SPXTSPWR    SAVE CALLER INDICATOR                        
         MVC   COMCSD,SPXTCSD      COMSCORE SURVEY DATE                         
         MVC   ASPXT50E,SPXT50EL   A(50 ELEMENT)                                
         MVC   REQSDATE,SPXTSTDT   REPORT REQUEST START DATE                    
         MVC   REQEDATE,SPXTENDT   REPORT REQUEST END DATE                      
         DROP   RE                                                              
*                                                                               
*                                                                               
GETAFDAA TM    FLAGS,WTPDAYOP      RESEARCH WTP                                 
         BZ    *+8                                                              
         MVI   DBBEST,C'R'         WEEK START ON STAURDAY                       
*                                                                               
         CLI   DBSELMED,C'N'                                                    
         BE    *+10                                                             
         MVC   DBSELBK,AFDBOOK     SET BOOK                                     
         CLI   SPLKDBK,X'FF'       N BOOK AVERAGING?                            
         BNE   *+16                                                             
         MVC   DBSELBK,SPLKDBK                                                  
         MVC   DBSELDAT,AFDBOOK                                                 
*                                                                               
* SINCE THIS IS THE SECOND TIME WE ARE CALLING LPM, WE WANT TO MAKE             
* SURE WE ARE GETTING THE RIGHT BOOK.  DBBTYPE MAY HAVE BEEN SET TO             
* P OR I ON THE FIRST CALL BECAUSE DBSELBK MAY HAVE NOT BEEN SET YET            
* (IT SHOULD BE SET NOW) AND IT MAY HAVE CHECKED THE LPM START DATE             
* AGAINST THE BUY END DATE. WE CAN MAKE SURE WE ARE GETTING THE RIGHT           
* BOOK BY RESETTING THE BOOK IF IT HAS BEEN CHANGED TO LPM ALREADY              
*                                                                               
         CLI   DBBTYPE,C'P'        GETTING LPM BOOK?                            
         BE    GETAFD1A            YES                                          
         CLI   DBBTYPE,C'I'        GETTING LPM BOOK                             
         BNE   GETAFLPM            NO                                           
*                                                                               
GETAFD1A MVC   DBBTYPE,SPLKBTYP                                                 
*                                                                               
         L     RF,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RF                                                      
*                                                                               
GETAFD1B CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+12                                                             
         MVI   DBBTYPE,0           YES: SUPPRESS INVALID BOOKTYPES              
         B     GETAFD1C                                                         
*                                                                               
         MVC   BKTYPIND,SPBTYIND   SAVE BOOKTYPE INDICATOR IN TABLE             
         MVC   OVEFFBK,SPOVEFFB    OVERNIGHT EFFECTIVE BOOK FOR BKTYPE          
* PROTECTIVE CODE IN CASE DEMTABS GETS BACKED OUT                               
* COMPARE NEW ELONGATED TABLE LENGTH AGAINST TABLE LENGTH RETURNED              
* BY DEMTABS.  IF NOT EQUAL IGNORE NEW CODE.                                    
* DELETE THIS CODE NEXT TIME TABLE GETS ELONGATED.  DUPLICATE LOGIC             
* NEXT TIME TABLE GETS ELONGATED FOR NEW FIELDS.                                
         LA    R0,SPBKTYLQ                                                      
         CH    R0,BKTYPTBL                                                      
         BE    *+10                                                             
         XC    OVEFFBK,OVEFFBK                                                  
*                                                                               
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BE    GETAFD1C                                                         
         AH    RF,BKTYPTBL         NO: TRY NEXT                                 
         B     GETAFD1B                                                         
         DROP  RF                                                               
*                                                                               
GETAFD1C CLI   DBBTYPE,C'S'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,0                                                        
         CLI   DBSELMED,C'N'       FOR NETWORK                                  
         BNE   *+10                                                             
         MVC   DBBTYPE,SPLKBTYP    MOVE IN STRAIGHT FROM CALLER                 
*                                                                               
GETAFLPM CLI   DBSELSRC,0          IF SOURCE WAS KILLED BY PREVIOUS             
         BNE   *+16                LPM ROUTINE CALL RESTORE                     
         MVC   DBSELSRC,SVSELSRC   DBSELSRC,DBSELSYC BEFORE CALLING LPM         
         MVC   DBSELSYC,SVSELSYC   ROUTINE AGAIN                                
         BRAS  RE,LPM              LPM SUPPORT                                  
*                                                                               
         MVI   WKLYOPT,X'80'       SET WEEKLY LOOK-UP FLAG                      
         CLC   SV1WPR5,BDPROGRM    NO WEEKLY DATA IF MATCH PROG NAME            
         BE    GETAFD1N                                                         
*                                                                               
         CLI   BDPROGT-1,0         TEST -S BUY                                  
         BNE   GETAFD1J            NO                                           
         CLI   SV1WPR2,C'Y'        TEST -S BUYS GET WEEKLY                      
         BE    GETAFD1X            YES                                          
         B     GETAFD1N                                                         
*                                                                               
GETAFD1J CLI   BDPROGRM,C' '       TEST SPECIAL CHAR VALID                      
         BNH   GETAFD1L                                                         
         CLC   SV1WPR3,BDPROGRM    MATCH SPECIAL CHAR TO PROG NAME              
         BE    GETAFD1X            YES                                          
*                                                                               
GETAFD1L CLI   SV1WPR1,C'Y'        TEST ALL BUYS GET WEEKLY                     
         BE    GETAFD1X                                                         
*                                                                               
GETAFD1N MVI   WKLYOPT,0           SUPPRESS WEEKLY LOOK-UP                      
*                                                                               
GETAFD1X XC    DBSEL1WK,DBSEL1WK                                                
         MVI   DBSELWKN,0                                                       
         CLI   SV1WPROF+15,C'Y'    TRUE WEEKLY?                                 
         BE    GTAFD1XY                                                         
         TM    WKLYOPT,X'80'                                                    
         BZ    *+10                                                             
GTAFD1XY MVC   DBSEL1WK,AFDATE     SET WEEK DATE                                
         MVI   WKLYIND,0           RESET WEEKLY DATA FOUND                      
*                                                                               
         TM    FLAGS,CANHPT        CANADIAN HPT'S?                              
         BNZ   *+12                                                             
         CLI   SPLKMED,C'R'        MEDIA RADIO?                                 
         BNE   GTAFD1XX                                                         
         L     RE,ASVIREC                                                       
         XC    0(128,RE),0(RE)                                                  
GTAFD1XX DS    0H                                                               
*Y2K                                                                            
         MVC   SVAFFMED,DBSELMED                                                
         CLC   DBSELBK,=X'7602'                                                 
         BE    OLYEX2F                                                          
         CLC   DBSELBK,=X'7202'                                                 
         BE    OLYEX2F                                                          
**       CLC   DBSELBK,=X'6E02'                                                 
**       BE    OLYEX2F                                                          
**       CLC   DBSELBK,=X'6A02'                                                 
**       BE    OLYEX2F                                                          
**       CLC   DBSELBK,=X'6602'    DONT NEED CODE FOR OLD BOOKS                 
**       BE    OLYEX2F             ANYMORE                                      
**       CLC   DBSELBK,=X'6202'                                                 
**       BE    OLYEXCF                                                          
         TM    SPLKOPT,SPLKOEXO    OLYMPIC EXCLUSION                            
         BZ    *+8                                                              
         MVI   DBBEST,C'O'         SET UP FOR GETTP                             
*                                                                               
OLYEXCF  DS    0H                                                               
         CLI   DBBTYPE,C'O'        BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*&&DO                                                                           
* BPOO 07/30/2018 THIS DOESNT SEEM TO DO ANYTHING.  THE CODE LOOKS LIKE         
* IT WILL ALWAYS DROP DOWN TO OLYEXCFX                                          
         CLC   DBSELBK,=X'6202'                                                 
         BNE   OLYEXCFX                                                         
*&&                                                                             
OLYEXCFX B     OLYEX2FX                                                         
*                                                                               
OLYEX2F  DS    0H                  FEB/02 ROUTINE                               
         CLI   DBBTYPE,BOOKTYPE_I  LPM MKTS NO OLYMPIC EXCLUSION                
         BE    OLYEX4FX                                                         
         CLI   DBBTYPE,BOOKTYPE_J  LPM MKTS NO OLYMPIC EXCLUSION                
         BE    OLYEX4FX                                                         
         CLI   DBBTYPE,BOOKTYPE_H  LPM MKTS NO OLYMPIC EXCLUSION                
         BE    OLYEX4FX                                                         
         CLI   DBBTYPE,BOOKTYPE_H3 LPM MKTS NO OLYMPIC EXCLUSION                
         BE    OLYEX4FX                                                         
         CLI   DBBTYPE,BOOKTYPE_HS LPM MKTS NO OLYMPIC EXCLUSION                
         BE    OLYEX4FX                                                         
         CLI   DBSELSRC,C'N'       ONLY NSI SWEEP HAS OLYMPIC EXLUSION          
         BNE   OLYEX4FX            DATA                                         
         CLI   DBSELMED,C'T'       ONLY LPM WEEKLY COMES IN WITH                
         BNE   OLYEX4FX            SELMED=W, NONLPM LOOKS LIKE T                
*                                                                               
         MVC   SVSELBK,DBSELBK                                                  
         BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
         CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BNE   OLYEX2G                                                          
* SETMETER MKT                                                                  
         CLI   SMMMONTH,C'Y'        IS MONTHLY OVNTS REQUESTED                  
         BE    OLYEX2G              CAN GET OLYMPIC EXC                         
         CLI   OVPHOPT,C'Y'         ARE REQ OVNIGHTS REQUESTED                  
         BE    OLYEX4FX             NO OLYMPIC EXCLUSION                        
*                                                                               
OLYEX2G  CLI   WVPHOPT,C'Y'        IF ASKING FOR WEEKLY NON LPM                 
         BE    OLYEX4FX            THEN DONT BOTHER WITH O EXCLUSION            
* 07/31/2018 THIS WAS COMMENTED OUT 20 VERSION CHANGES AGO                      
* THIS BREAKS LPM MARKET OVN POSTING.  WE ENDED UP READING                      
* FOR THE OLYMPIC  EXCLUSION DATA FOR OVERNIGHTS WHICH DOESNT EXIST,            
* WE AHVE TO ALLOW THIS OPTION FOR DIARY MARKET BECAUSE WE NEED                 
* TO SET THE OLYMPIC BOOKTYPE FOR DIARY.  WE WILL END UP READING FOR            
* MONTHLY BOOK DOWNSTREAM BECAUSE WE READ MONTHLY DATA FOR                      
* DIARY IN THE CODE DOWNSTREAM.                                                 
*                                                                               
******   CLI   OVPHOPT,C'Y'        IF ASKING FOR OVERNIGHTS NON LPM             
******   BE    OLYEX4FX            THEN DONT BOTHER WITH O EXCLUSION            
*&&DO                                                                           
         OC    LPMDTP,LPMDTP       DIARY MARKETS ALLOW OLYPMIC EXCLUS           
         BZ    *+12                LPM MARKETS DO NOT SET OLYMPIC EXC           
         CLI   OVPHOPT,C'Y'        IF ASKING FOR OVERNIGHTS NON LPM             
         BE    OLYEX4FX            THEN DONT BOTHER WITH O EXCLUSION            
*&&                                                                             
* EVERYONE HAS LPMDTP ON NOW..MAKE SURE ITS REALLY LPM                          
         BRAS  RE,TRUELPM          IF NOT A TRUE LPM LPMDTP WILL CLEAR          
         CLI   MKTTYPE,MKTSETM                                                  
         BE    *+14                                                             
         OC    LPMDTP,LPMDTP       DIARY MARKETS ALLOW OLYPMIC EXCLUS           
         BZ    OLNONLPM            LPM MARKETS DO NOT SET OLYMPIC EXC           
*                                                                               
         CLI   OVPHOPT,C'Y'        IF ASKING FOR OVERNIGHTS NON LPM             
         BE    OLYEX4FX            THEN DONT BOTHER WITH O EXCLUSION            
*                                                                               
OLNONLPM DS    0H                                                               
         CLI   DBBTYPE,C'O'        BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBBTYPE,BOOKTYPE_OL BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,C'L'                                                     
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_O1 BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_L1                                              
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_OS BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_LS                                              
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_O3 BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_L3                                              
*                                                                               
         L     RF,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RF                                                      
OLYEX2H  CLI   0(RF),X'FF'         EOT?                                         
         BE    OLYEX2J             YES: CHECK OLYMPIC EXCLUSION                 
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BE    OLYEX2FX            YES: NO EXCLUSION FOR BOOK TYPES             
         AH    RF,BKTYPTBL         NO: TRY NEXT                                 
         B     OLYEX2H                                                          
         DROP  RF                                                               
*                                                                               
OLYEX2J  DS    0H                                                               
         TM    SPLKOPT,SPLKOEXO    OLYMPIC EXCLUSION                            
         BZ    OLYEX2FX                                                         
*        CLC   DBSEL1WK(2),=X'CC3F' 020131                                      
***      CLC   AFDATE(2),=X'CC3E' 020131                                        
***      BH    *+8                                                              
***      MVI   DBBTYPE,C'O'                                                     
*        CLC   DBSEL1WK(2),=X'CC5B' 020227                                      
         CLC   AFDATE(2),=X'CC5B' 020227                                        
         BNH   *+8                                                              
         MVI   DBBTYPE,C'O'                                                     
*Y2K                                                                            
OLYEX2FX DS    0H                                                               
         CLC   DBSELBK,=X'7602'    FEB2018                                      
         BE    *+10                                                             
         CLC   DBSELBK,=X'7202'    FEB2014                                      
***      BE    *+10                                                             
**       CLC   DBSELBK,=X'6E02'    FEB2010                                      
**       BE    *+10                                                             
**       CLC   DBSELBK,=X'6602'    FEB2002                                      
**       BE    *+10                                                             
***      CLC   DBSELBK,=X'6A02'    FEB2006                                      
         BNE   OLYEX4FX                                                         
*                                                                               
         CLI   DBBTYPE,0           ALLOW STANDARD SURVEY                        
         BE    OLYEX2N             FOR OLYMPICS                                 
         CLI   DBBTYPE,C'L'        ALLOW LIVE ONLY AND LIVE+SAME DAY            
         BE    OLYEX2N             AND LIVE+3 FOR OLYMPICS                      
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BE    OLYEX2N                                                          
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    OLYEX2N                                                          
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BE    OLYEX2N                                                          
         L     RF,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RF                                                      
OLYEX2L  CLI   0(RF),X'FF'         EOT?                                         
         BE    OLYEX2N             YES: CHECK OLYMPIC EXCLUSION                 
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BE    OLYEX4FX            YES: NO EXCLUSION FOR BOOK TYPES             
         AH    RF,BKTYPTBL         NO: TRY NEXT                                 
         B     OLYEX2L                                                          
         DROP  RF                                                               
*                                                                               
OLYEX2N  DS    0H                                                               
         TM    SPLKOPT,SPLKOEXO    OLYMPIC EXCLUSION                            
         BZ    OLYEX4FX                                                         
*&&DO                                                                           
* REMOVED TO FREE UP ADDRESSIBILITY. OLD CODE.                                  
         CLC   DBSELBK,=X'6A02'    FEB2006                                      
         BNE   OLYEX2O                                                          
         CLC   AFDATE(2),=X'D43D' JAN302006                                     
         BH    *+8                                                              
         MVI   DBBTYPE,C'O'                                                     
         CLC   AFDATE(2),=X'D45B' FEB262006                                     
         BNH   *+8                                                              
         MVI   DBBTYPE,C'O'                                                     
         B     OLYEX4FX                                                         
*&&                                                                             
*                                                                               
OLYEX2O  LA    RE,OLMEXCDT                                                      
*                                                                               
OLYEX2P  CLI   0(RE),X'FF'                                                      
         BE    OLYEX4FX                                                         
         CLC   DBSELBK,0(RE)                                                    
         BE    OLYEX2Q                                                          
         LA    RE,6(RE)                                                         
         B     OLYEX2P                                                          
*                                                                               
OLYEX2Q  CLC   AFDATE(2),2(RE)    START OF SWEEP                                
         BNL   OLYEX2R                                                          
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     OLYEX2R                                                          
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     OLYEX2R                                                          
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     OLYEX2R                                                          
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O1                                              
         B     OLYEX2R                                                          
         MVI   DBBTYPE,C'O'                                                     
OLYEX2R  CLC   AFDATE(2),4(RE)    END OF SWEEP                                  
         BNH   OLYEX4FX                                                         
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     OLYEX4FX                                                         
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     OLYEX4FX                                                         
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     OLYEX4FX                                                         
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O1                                              
         B     OLYEX4FX                                                         
         MVI   DBBTYPE,C'O'                                                     
OLYEX4FX DS    0H                                                               
*                                                                               
         MVI   DEMANDSW,0                                                       
         MVI   SPLKAIND,0                                                       
*                                                                               
AFFWTP   DS    0H                                                               
*&&DO                                                                           
* IF PASS ONE MODE THEN BYPASS ALL EXISTING DEMO SYSTEM CALLING                 
* CODE AND GO STRAIGHT TO CALL COMINTER                                         
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F')                      
         IF (CLI,COMPASS,E,X'01'),AND,(CLI,DBVOPT,E,X'40')                      
****     IF (TM,NTDFLAG,NTDCOMD,O) HAVE COMSCORE DEMOS                          
         IF (TM,SPLKOPT2,SPLKOPT2_COMS,O) HAVE COMSCORE DEMOS                   
         B     COMAFD10                                                         
         ENDIF                                                                  
         ENDIF                                                                  
         ENDIF                                                                  
*&&                                                                             
* IF PASS ONE MODE THEN BYPASS ALL EXISTING DEMO SYSTEM CALLING                 
* CODE AND GO STRAIGHT TO CALL COMINTER                                         
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,COMPASS,E,X'01'),AND,(CLI,DBVOPT,E,X'40')                      
****        (CLI,COMPASS,E,X'01'),AND,(CLI,DBVOPT,E,X'40'),ANDIF,               
****        (TM,SPLKOPT2,SPLKOPT2_COMS,O) HAVE COMSCORE DEMOS                   
         B     COMAFD10                                                         
         ENDIF                                                                  
*&&DO                                                                           
* IF PASS TWO MODE:                                                             
* IF ONLY COMSCORE DEMO PASSED- SKIP NIELSEN PROCESSING                         
*                                                                               
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F')                      
         IF (CLI,COMPASS,E,X'02'),AND,(CLI,DBVOPT,E,X'40')                      
****     IF (CLI,NTDFLAG,E,NTDCOMD) IF ONLY HAVE COMSCORE DEMO                  
         IF (CLI,SPLKOPT2,E,SPLKOPT2_COMS) IF ONLY HAVE COMSCORE DEMO           
         B     COMAFD10             DONT PROCESS NIELSEN                        
         ENDIF                                                                  
         ENDIF                                                                  
         ENDIF                                                                  
*&&                                                                             
* IF PASS TWO MODE:                                                             
* IF ONLY COMSCORE DEMO PASSED- SKIP NIELSEN PROCESSING                         
*                                                                               
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,COMPASS,E,X'02'),AND,(CLI,DBVOPT,E,X'40'),ANDIF,               
            (TM,SPLKOPT2,SPLKOPT2_NLSN,Z) DONT HAVE NIELSEN DEMOS               
*****       (CLI,SPLKOPT2,E,SPLKOPT2_COMS) IF ONLY HAVE COMSCORE DEMO           
         B     COMAFD10             DONT PROCESS NIELSEN                        
         ENDIF                                                                  
*&&DO                                                                           
* THIS CODE IS NOT GOING IN 18.1  THIS IS RELATED TO   SPEC-20302               
*  IF NOT ACTBOOK - DO NOT CALL DEMO SYSTEM TO LOOKUP DEMOS                     
* ONLY PROCESS COMSCORE                                                         
         CLI   ANYACTBK,C'N'                                                    
         BE    COMAFD10                                                         
*&&                                                                             
         BRAS  RE,TOMONTHL        DEFAULT WEEKLY TO MONTHLY LOOKUPS             
*                                 AFTER MMW CUTOFF DATE.                        
         TM    CALLERF,SPXTSPWR                                                 
         BZ    AFFWTPP                                                          
         BRAS  RE,SETMKTYP         SETS STABMKT                                 
         BRAS  RE,TRUELPM          IF NOT A TRUE LPM LPMDTP WILL CLEAR          
************** CHECK FOR POSTNG OPTIONS *************************               
AFFWTPP  CLC   =C'YNN',LPMLKOPT   IF USER REQUESTED LPMWK OPTION  ONLY          
         BNE   *+14               AND WE ARE NOT DEALING WITH AN LPM            
         OC    LPMDTP,LPMDTP      MARKET, WE WANT TO READ MONTHLY               
         BZ    AFFWTPX                                                          
                                                                                
*  EVEN THOUGH MEDIA IS W OR O  THE BOOK SOMETIMES CAN BE MONTHLY               
*  BOOK...THIS CAN LEAD TO TROUBLE AS THE BOOK CAN EITHER BE A WEEK             
*  OR MONTHLY FORMAT- WE CANT TELL.  THIS SITUATION ARISES WHEN                 
*  HAVE A MIX OF MONTHS AND WEEKS IN A BUYLINE                                  
*  THIS LPM OPTION IS TRICKY...WE CANT ASSUME THE MEDIA COMING IN               
*  MEANS ANYTHING IF LPMWK=Y.  DONT RELY ON THE BOOK FORMAT TO                  
*  MATCH THE MEDIA CODE COMING IN                                               
*                                                                               
         CLC   =C'NNN',LPMLKOPT                                                 
         BNE   *+28                                                             
         CLI   DBSELMED,C'W'                                                    
         BNE   *+20                                                             
         CLI   WVPHOPT,C'N'                                                     
         BNE   *+12                                                             
         CLI   DBSELBK,YR_2014                                                  
         BE    AFFWTP2O                                                         
*                                                                               
         CLI   LPMLKOPT,C'Y'                                                    
         BE    AFFWTP1                                                          
         CLI   WVPHOPT,C'Y'                                                     
         BE    AFFWTP1                                                          
****     CLI   OVPHOPT,C'Y'                                                     
****     BNE   AFFWTPX                                                          
         CLI   OVPHOPT,C'Y'                                                     
         BE    AFFWTP1                                                          
* IF WE ARE POSTING MONTHLY -CHECK IF BOOK IS JUN09 FOR LPM MARKET              
* JUN09 IS THE DIGITAL TRANSITION WHERE NIELSEN HAS NOT RELEASED A              
* BOOK HENCE WE WANT TO TO OVERNIGHTS - IF NO OVERNIGHTS WE WANT TO             
* TO POST TO JUL_09                                                             
         MVI   DIGITALF,C'N'                                                    
*****************************************************************               
* THIS BRANCH REPLACES THE DIGITAL TRANSTION CODE BELOW                         
* OLD DATA IS NO LONGER ON FILE SO WE SHOULD ALWAYS GO TO AFFWTPX               
         B     AFFWTPX ALWAYS GO TO AFFWTPX SINCE OLD DATA DELETED              
*****************************************************************               
*&&DO                                                                           
         TM    DBVOPT,X'40'        DIGITAL TRANSITION CODE ONLY                 
         BNO   AFFWTPX             VALID FOR POSTING                            
         CLI   DBSELMED,C'T'                                                    
         BNE   AFFWTPX                                                          
         CLI   DBSELSRC,C'N'                                                    
         BNE   AFFWTPX                                                          
* SHOULDNT NEED THIS CODE ANYMORE SINCE JUN_09 NOT AVAILABLE ANYMORE            
         CLC   DBSELBK,=AL2(JUN_09)                                             
         BNE   AFFWTPX                                                          
         OC    LPMDTP,LPMDTP     ONLY FOR LPM MARKETS-NON LPM POST              
         BZ    AFFWTPX           TO MONTHLY                                     
         OC    DBSELSYC,DBSELSYC  LPM CABLE JUN09 POST TO JUL09                 
         BNZ   AFFWTP0                                                          
         CLI   DBBTYPE,C'H'      HISPANIC/BLACK BOOKTYPES POST TO               
         BE    *+8               JUL09 BOOK                                     
         CLI   DBBTYPE,C'B'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'H'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'J'                                                     
         BNE   *+14                                                             
AFFWTP0  MVC   DBSELBK,=AL2(JUL_09)                                             
         B     AFFWTPX                                                          
         MVI   DIGITALF,C'Y'     TURN ON DIGITAL TRANSITION FLAG                
*&&                                                                             
********************************************************************            
*   YOU SHOULD ONLY BE HERE FOR POSTING                                         
********************************************************************            
****     CLI   DBSELMED,C'T'       ALWAYS FIGURE OUT THE BOOK FOR               
****     BNE   AFFWTPX             OVN,WEEKLY POSTING                           
*                                                                               
AFFWTP1  DS    0H                                                               
* 07/31/2018 GET RID OF THIS REALLY OLD CODE.  NO LONGER NEEDED                 
*&&DO                                                                           
         CLI   DBSELBK,98                                                       
         BNE   *+14                                                             
         CLC   =C'WKMG',DBSELSTA   DOUBLE CALL LETTER SWITCH                    
         BE    AFFWTPX             NOT HANDLED                                  
*&&                                                                             
         CLI   DBBTYPE,0           ALWAYS ALLOW STANDARD                        
         BE    AFFWTP1H                                                         
         CLI   LPMLKOPT,C'Y'       IF WE ARE POSTING WEEKLY THEN CHECK          
         BE    *+8                 BOOKTYPE INDICATOR TO SEE IF WEEKLY          
         CLI   WVPHOPT,C'Y'        IS AVAILABLE FOR THIS BOOKTYPE               
         BNE   *+12                IF ITS NOT AN AVAILABLE WEEKLY               
         TM    BKTYPIND,SPBKWKLY   BOOKTYPE THEN CHECK TO SEE IF                
         BNZ   AFFWTP1H            OVERNIGHT POSTING OPTION IS ON               
         CLI   DIGITALF,C'Y'       IF DIGITAL TRANSITION WE DONT CARE           
         BE    AFFWTP1H            BOOKTYPE IS AVAILABLE FOR OVERNIGHT          
         CLI   OVPHOPT,C'Y'        IF NOT- WE GO TO JUL09 BOOK                  
         BNE   AFFWTP1G                                                         
         TM    BKTYPIND,SPBKOVN    BOOKTYPE AVAILABLE FOR OVERNIGHT?            
         BNZ   AFFWTP1H                                                         
AFFWTP1G MVI   DBSELMED,C'T'       IF NOT AVAILABLE-GO READ MONTHLY             
* RESET WEEKLY AND OVERNIGHT VALUES FROM PREVIOUS READ                          
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
*                                                                               
         B     AFFWTPX                                                          
*                                                                               
AFFWTP1H OC    DBSELSYC,DBSELSYC  CABLE IS ONLY MONTHLY FOR NOW                 
         BNZ   AFFWTPX                                                          
         OC    DBSELMK,DBSELMK                                                  
         BNZ   AFFWTPX                                                          
         MVI   DBMODE,DBMFRST      RESET THE DBLOCK                             
         XC    DBACTUAL,DBACTUAL   AND CLEAR THE ACTUALS                        
         MVC   SVAFFMED,DBSELMED                                                
         MVI   DBSELMED,C'W'                                                    
         MVC   SVSELBK,DBSELBK     BOOK FORMAT IS YY00                          
         BRAS  RE,GETVDAT          GET WTP YEAR                                 
*******************************************************************             
*  CONVERT THE BOOK TO GET THE CORRECT YEAR,,COULD BE END OF YR BUT             
*  ITS STILL CAN BE 1ST WEEK OF NEXT YR ON THE FILE                             
*******************************************************************             
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DUB),(0,DUB3)                                       
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 VNSIWEEK,DMCB,DUB3                                               
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
*                                                                               
* CHECK FOR LPM MARKET                                                          
         TM    CALLERF,SPXTSPWR    NON POSTING CALLERS LIKE $DEM                
         BZ    AFFWTP2             ALWAYS GETS VPH IF WVPHOPT IS ON             
         OC    LPMDTP,LPMDTP       ANY LPM DATE                                 
         BZ    AFFWTP2              NO - USE WEEKLY HOMES TO ADJUST             
         CLC   DUB(2),LPMDTB        MONTH GREATER THAT LPM                      
         BL    AFFWTP2                                                          
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0           IF LPM ALREADY NO BOOKTYPE                   
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
         MVI   MKTTYPE,MKTLPM                                                   
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         B     WVPHLPM3             YES - GET LPM DEMOS                         
*                                                                               
*----------------   NON LPM SECTION -------------------------------             
* AT THIS POINT- NOT LPM - CHECK TO SEE IF WE ARE READING SET METERED           
* MARKET.  IF SET METERED THEN PROCEED ELSE WE ARE DIARY MARKET                 
* DIARY MARKETS IGNORE  WTP AND OTP OPTIONS AND PROCEED TO READ                 
* BOOK OPTION OR ACT BOOK.                                                      
AFFWTP2  OC    LPMDTP,LPMDTP        IF WE HAVE LPM DATE WE MUST HAVE            
         BZ    AFFWTP2A             BEEN SETMETERED BEFORE                      
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION ONLY GO TO                
         BNE   *+12                OVERNIGHTS FOR LPM MARKETS                   
         MVI   DIGITALF,C'N'       ELSE WE WANT TO POST TO MONTHLY              
         B     AFFWTPX                                                          
* IF WTP/OVN OPTION TURNED ON READ SET METERED-ELSE READ MONTHLY                
         CLC   =C'NN',WVPHOPT                                                   
         BE    AFFWTP2O             READ MONTHLY- NO WTP OR OVN OPTION          
         MVI   MKTTYPE,MKTSETM      ELSE READ SET METERED WTP LOGIC             
         B     AFFWTP2B                                                         
AFFWTP2A BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
AFFWTP2B CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BE    AFFWTP2BB                                                        
         MVI   WVPHOPT,C'N'         DIARY MKT READ MONTHLY                      
         MVI   OVPHOPT,C'N'                                                     
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         B     AFFWTP2O                                                         
*                                                                               
AFFWTP2BB DS   0H                                                               
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0           IF LPM ALREADY NO BOOKTYPE                   
         MVI   MKTTYPE,MKTSETM                                                  
         MVI   DBSELBK+1,0         READ WEEKLY ZERO RECORD                      
*                                                                               
         CLI   DBSELBK,98                                                       
         BNE   *+14                                                             
         CLC   =C'WKMG',DBSELSTA   DOUBLE CALL LETTER SWITCH                    
         BE    AFFWTPX             NOT HANDLED                                  
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
***      BAS   RE,SETDQLNK                                                      
         BRAS  RE,SETDQLNK                                                      
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
*                                                                               
AFFWTP2C L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETVHOME                                        
         TM    CALLERF,SPXTSPWR    NON POSTING CALLER SKIP NEW                  
         BZ    AFFWTP2O            POSTING CODE                                 
*                                                                               
         MVI   MISCOUNT,0          CLEAR MISSING DATA COUNT AND DAY             
         MVI   MISSDAY,0                                                        
* IF LPMWK WAS THE ONLY OPTION ENTERED THAN WE HAVE TO SET                      
* DMINDEX TO 'Y' SO THE DEMO GETS INDEXED LATER ON                              
*                                                                               
         MVI   DMINDEX,C'N'                                                     
         CLC   LPMLKOPT,=C'YNN'                                                 
         BNE  *+8                                                               
         MVI   DMINDEX,C'Y'        SET FORCE DEMO INDEX FLAG                    
                                                                                
* ********OVERNIGHT LOGIC*************************************                  
                                                                                
         CLC   =C'NY',WVPHOPT      OVN=Y ONLY ALWAYS GRAB                       
         BNE   *+16                                                             
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
*                                                                               
         OC    VPHHOMES,VPHHOMES   IF ANY HOMES                                 
         BNZ   AFFWTP2O            IT'S OK                                      
*                                                                               
* NO VPHHOMES, CHECK TO SEE IF WTP OPTION IS ON                                 
*                                                                               
         CLI   WVPHOPT,C'Y'                                                     
         BNE   AFFWTP2CC                                                        
         CLC   DBSELSTA,DSTACALL   DO WE HAVE A STATION LINK                    
         BE    AFFWTP2CC           TO LOOK AT?                                  
         MVC   DBSELSTA,DSTACALL   LOOK FOR LINKED STATION                      
         MVC   DSTACALL,SVSELSTA     SWAP LINK AND  CALL LETTERS                
         MVI   DBERROR,0                                                        
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         B     AFFWTP2C                                                         
*                                                                               
AFFWTP2CC DS   0H                                                               
         CLI   OVPHOPT,C'Y'        IF WEEKLY NOT FOUND LOOK                     
         BNE   AFFWTP2G            IF NO OVN ASK FOR GO TO B.E                  
*                                                                               
*                                                                               
         CLI   SMMMONTH,C'Y'       SET METER READ MONTHLY OPTION?               
         JNE   AFFWTP2D                                                         
AFFWTP2CD DS   0H                                                               
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         MVI   DAYREAD,0                                                        
         XC    VPHFACT,VPHFACT                                                  
         J     AFFWTP2O                                                         
*                                                                               
AFFWTP2D DS    0X                                                               
         MVI   DBSELMED,C'O'       SET THE OVERNIGHT FLAG                       
         BRAS  RE,GETVHSWK                                                      
         MVC   DBSELBK,VPHSWK      WEEKLY BOOK                                  
         MVI   ANYOVFLG,C'N'                                                    
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
*                                                                               
*SHOULD WE BREAK AVERAGE QTR HOURS ?                                            
         NI    DBNPMFLG,X'FF'-DBNOBAVG                                          
         CLI   SV1WPROF+12,0                                                    
         JE    AFFWTP2E                                                         
         ZIC   RE,SV1WPROF+12                                                   
         AHI   RE,100                                                           
         ZIC   R1,DBSELBK                                                       
         CR    R1,RE                                                            
***      CLC   DBSELBK(1),SV1WPROF+12                                           
         BL    AFFWTP2E                    LOWER YEAR DONT TURN ON FLAG         
         BH    *+14                        HIGHER YEAR, TURN ON FLAG            
         CLC   DBSELBK+1(1),SV1WPROF+14    SAME YEAR/ CHECK MONTH               
         BL    *+8                                                              
         OI    DBNPMFLG,DBNOBAVG                                                
*                                                                               
*                                                                               
* CHECK TO SEE IF OVERNIGHT BOOKTYPE IS ONLY AVAILABLE                          
* AFTER A CERTAIN EFFECTIVE BOOK - AS DEFINED BY DEDEMTABS SPBOOKTB             
* IF BOOK IS BEFORE EFFECTIVE DATE THEN KEEP POSTING AS MONTHLY                 
* SO OLD POSTS WILL REMAIN THE SAME                                             
AFFWTP2E OC    OVEFFBK,OVEFFBK                                                  
         BZ    AFFWTP2F                                                         
         CLC   DBSELBK,OVEFFBK     READ MONTHLY PRIOR TO EFFECTIVE BK           
         BL    AFFWTP2CD                                                        
* CALL DEMAND AND RETURN OVERNIGHT VALUE IN VPHHOMES                            
AFFWTP2F L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETOVHOM                                        
AFFWTP2G CLI   DEMANDSW,0          IF NO OVERNIGHTS FOUND                       
         BE    POVPHX4             GO TO B.E                                    
         OC    VPHHOMES,VPHHOMES                                                
         BNZ   AFFWTP2O                                                         
         MVI   DEMANDSW,0                                                       
         B     POVPHX4                                                          
*                                                                               
AFFWTP2O MVC   DBSELBK,SVSELBK                                                  
         MVI   DBSELMED,C'T'                                                    
* IF WE ARE PROCESSING MEDIA T MAKE SURE TO RECHECK IMPRESSION BASE             
* USTV ALWAYS IMPRESSION BASED CALCULATION                                      
*****    CLI   SV1WPROF+5,C'I'     IMP BASED CALCULATED DEMOS                   
*****    BNE   *+8                                                              
         MVI   TAPEOPT,C'Y'        CHECK FOR NSI/USTV BELOW                     
*                                                                               
AFFWTPX  DS    0C                                                               
*                                                                               
***************************************************************                 
* WHEN PROCESSING LPM MAKE SURE WE SET THE WEEKLY BOOK                          
         CLI   MKTTYPE,MKTLPM                                                   
         BE    *+8                                                              
         CLI   WVPHOPT,C'P'        PEOPLE METER LOOKUP?                         
         JNE   *+8                                                              
WVPHLPM3 BRAS  RE,PWVPH                                                         
AFFORT   CLI   DBSELMED,C'N'       NETWORK HAS WEEKLY BOOKS                     
         BNE   *+10                                                             
         XC    DBSEL1WK,DBSEL1WK   SO DON'T LOOK HERE                           
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   AFFORT1                                                          
         XC    DBSELSYC,DBSELSYC                                                
         ICM   RF,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    AFFORT1                                                          
         USING SPLKXTD,RF                                                       
* HAVE TO ALWAYS PASS SYSCODE INTO DEGET TO GET AIUE TO WORK                    
         MVC   DBSELSYC,SPXTSYSC   DEGET MIGHT SWITCH TO BKTYP W                
         DROP  RF                                                               
*                                                                               
AFFORT1  L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
*                                                                               
         L     R0,SPLKHOOK         GET USER HOOK ADDRESS                        
         XC    SPLKHOOK,SPLKHOOK   AND SUPPRESS FOR DEMAND CALL                 
***      BAS   RE,SETDQLNK                                                      
         BRAS  RE,SETDQLNK                                                      
*                                                                               
*IF PROFILE SET TO USE NEW POSTING METHODOLOGY THEN                             
*SET THE BOOK TO AFDATE - DEGETTP WILL FIGURE OUT THE WEEKLY BOOK               
****     MVI   DBNPMFLG,0                                                       
         NI    DBNPMFLG,X'FF'-DBNOBAVG                                          
* IF NEW METHODLOGY PROFILE ON AND POSTING                                      
         IF (TM,SPLKOPT2,SPLKOPT2_NPM,O),AND,                                   
            (CLI,DBVOPT,E,X'40'),AND,(CLI,DBSELMED,E,C'C')                      
         MVI   DBNPMFLG,DBNPMAFF   SET FLAG TO NEW POSTING METHODOLOGY          
         MVC   DBSELBK,AFDATE                                                   
         ENDIF                                                                  
*                                                                               
*SHOULD WE BREAK AVERAGE QTR HOURS ?                                            
         CLI   SV1WPROF+12,0                                                    
         JE    AFFNOBAV                                                         
         ZIC   RE,SV1WPROF+12                                                   
         AHI   RE,100                                                           
         ZIC   R1,DBSELBK                                                       
         CR    R1,RE                                                            
***      CLC   DBSELBK(1),SV1WPROF+12                                           
         BH    AFFNOBA8            HIGHER YEAR - TURN ON FLAG                   
         BL    AFFNOBAV            LOWER YEAR- DONT TURN FLAG ON                
         CLI   DBSELMED,C'O'       SAME YEAR- CHECK MONTH                       
         BNE   AFFNOBA1                                                         
         CLC   DBSELBK+1(1),SV1WPROF+14                                         
         BL    AFFNOBAV                                                         
         B     AFFNOBA8                                                         
*                                                                               
AFFNOBA1 CLI   DBSELMED,C'T'                                                    
         BNE   AFFNOBAV                                                         
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   AFFNOBAV                                                         
         CLC   DBSELBK+1(1),SV1WPROF+13                                         
         BL    AFFNOBAV                                                         
AFFNOBA8 OI    DBNPMFLG,DBNOBAVG                                                
AFFNOBAV DS    0C                                                               
*SHOULD WE LOOK UP TRUE WEEKLY NIELSEN MONTHLY DATA                             
         CLI   SV1WPROF+15,C'Y'                                                 
         JNE   AFFNOTWK                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   AFFNOTWK                                                         
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   AFFNOTWK                                                         
AFFTRWK8 OI    DBNPMFLG,DBTRUEWK                                                
AFFNOTWK DS    0C                                                               
*                                                                               
*                                                                               
* CHECK TO SEE IF OVERNIGHT                                                     
* IF SO CHECK BOOK AGAINST OVEFFBK AND SEE IF BOOK FOR THE BOOKTYPE             
* WE ARE POSTING AGAINST IS BEFORE THE AVAILABLE START BOOK                     
         CLI   DIGITALF,C'Y'                                                    
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BNE   AFFORT1A                                                         
         CLI   DBSELMED,C'O'                                                    
         BNE   AFFORT1A                                                         
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    AFFORT1A                                                         
         CLC   DBSELBK,OVEFFBK       POST MONTHLY PRIOR TO EFFECTIVE BK         
         BNL   AFFORT1A                                                         
* READ MONTHLY BOOK                                                             
         DS    0C                                                               
         MVI   DBSELMED,C'T'                                                    
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         MVC   DBSELBK,SVSELBK                                                  
*                                                                               
         B     AFFORT1A                                                         
*                                                                               
AFFORT1A GOTO1 (RF),DMCB,DBLOCK,GETDEMHK                                        
         ST    R0,SPLKHOOK         THEN RESTORE HOOK ADDRESS                    
*******  MVI   DBBEST,0            AND RESET OLYMPIC EXCLUSION                  
         MVC   DBBEST,SVDBBEST     REST DBBEST TO ORIGINAL                      
******************************************************************              
* MEDIA ALWAYS = W  WHETHER THEY ASKED FOR OVN OR LPMWK=Y                       
* CHECK TO SEE IF WE NEED TO READ FOR OVERNIGHTS                                
         CLI   DBSELMED,C'W'       WEEKLY JUST LOOKED UP                        
         BNE   POVPHX4              NO - TREAT AS NORMAL                        
         TM    CALLERF,SPXTSPWR    NON POSTING CALLER SKIP NEW                  
         BZ    POVPHX4             POSTING CODE                                 
*                                                                               
AFFORT2  CLC   =C'NNY',LPMLKOPT    OVN=Y ONLY ALWAYS GRAB                       
         BE    AFFORT3             LPM OVERNIGHTS                               
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION - WE WANT                 
         BE    AFFORT3             OVERNIGHTS                                   
         CLI   OVPHOPT,C'Y'        OVN=Y OPTION                                 
         BNE   POVPHX4                                                          
         CLI   DEMANDSW,0          DID WE GET WEEKLY DATA?                      
         BNE   POVPHX4                                                          
AFFORT3  MVI   DBSELMED,C'O'       NOTHING FOUND - TRY OVERNITE                 
         BRAS  RE,GETVHSWK         CALL NSIWEEK TO CONVERT BK AGAIN             
         MVI   TAPEOPT,C'Y'        OVERNIGHTS IS IMPRESSION BASED               
         MVC   DBSELBK,VPHSWK      ONVERTED BOOK                                
         MVI   DEMANDSW,0          RESET SWITCH                                 
* TO REREAD FOR OVERNIGHTS - CLEAR IUN VALUES FROM  WHAT                        
* WAS STORED FROM WEEKLY READ IF ANY                                            
AFFORT4  LA    R0,4                RTGS/IMPS/PUTS/TOTS                          
         L     R1,ADTUNVS                                                       
         XC    0(IUNDEMS*4,R1),0(R1)                                            
         LA    R1,IUNDEMS*4(R1)                                                 
         BCT   R0,*-10                                                          
         B     AFFORT                                                           
******************************************************************              
*                                                                               
POVPHX4  CLI   DIGITALF,C'Y'                                                    
         BNE   POVPHX5                                                          
         CLI   DBSELMED,C'O'                                                    
         BNE   POVPHX5                                                          
         MVC   DBSELMED,SVAFFMED                                                
         CLI   DBERROR,X'80'       CHECK FOR EOF RETURN                         
         BNE   *+12                                                             
         CLI   DEMANDSW,0                                                       
         BNE   GETAFD2             NO                                           
         MVC   DBSELBK,=AL2(JUL_09)                                             
         MVI   DBSELMED,C'T'                                                    
         B     AFFORT4                                                          
*                                                                               
POVPHX5  MVC   DBSELMED,SVAFFMED                                                
         CLI   DBERROR,X'80'       CHECK FOR EOF RETURN                         
         BNE   *+12                                                             
         CLI   DEMANDSW,0                                                       
         BNE   GETAFD2             NO                                           
         OC    SPLKASPT,SPLKASPT   YES-TEST FOR TABLE LOOKUP                    
         BZ    GETAFD20            NO                                           
         OI    SPLKAIND,SPLKAIER   YES-INDICATE AN ERROR                        
         L     R1,SAVER1                                                        
         L     RF,0(R1)                                                         
         MVC   0(L'SPDEMLK,RF),SPDEMLK  COPY BLOCK TO USER'S                    
***** IF SPOT LOOKUP FAILED WE SHOULD STILL GO LOOKUP COMSCORE                  
****                                                                            
         IF (CLI,COMPASS,E,X'01'),OR,(CLI,COMPASS,E,X'02'),ANDIF,               
            (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,DBVOPT,E,X'40'),ANDIF,                                         
            (TM,SPLKOPT2,SPLKOPT2_COMS,O)   IF WE HAVE COMSCORE DEMOS           
         B     COMAFD10                                                         
         ENDIF                                                                  
*                                                                               
         B     GETAFD12                 AND CALL USER'S HOOK                    
         SPACE 1                                                                
* SET RATING MKT TO PREVENT DEMAND READING IT EACH TIME *                       
         SPACE 1                                                                
GETAFD2  MVC   DBSELRMK,DBACTRMK                                                
         SPACE 1                                                                
         MVC   DBTAPEP,TAPEOPT                                                  
         BAS   RE,GETIUN           EXTRACT REQUESTED DEMOS                      
                                                                                
         CLI   DBSELMED,C'R'                                                    
         BNE   *+16                                                             
         MVC   DTDEMSA,RADEMSA                                                  
         MVC   DTDEMSU,RADEMSU                                                  
                                                                                
         B     GETAFD4                                                          
*                                                                               
DAYTBL   DC    X'40201008040201'   DAY TABLE FOR DEMAND CALLS                   
         SPACE 1                                                                
* SET DATA FOR USER *                                                           
         SPACE 1                                                                
GETAFD4  MVC   SPLKADAT,AFDATE     SET AFFID DATE                               
         MVC   SPLKABK,SVACTBK     SET ACTUAL BOOK                              
*                                                                               
         TM    DBVOPT,X'40'        SPOT POSTING?                                
         BZ    GETAFD4X                                                         
         OC    STNAMEOV,STNAMEOV                                                
         BZ    GETAFD4X                                                         
         MVC   STNAME,STNAMEOV                                                  
         MVC   ENDNAME,ENDNAMEO                                                 
GETAFD4X DS    0H                                                               
*                                                                               
         LA    RE,SPLKPRG                                                       
         MVC   0(15,RE),STNAME                                                  
         CLC   STNAME,ENDNAME                                                   
         BE    *+14                                                             
         MVI   7(RE),C'/'                                                       
         MVC   8(7,RE),ENDNAME                                                  
         MVC   SPLKDAY,DBSELDAY    SET DAY                                      
         MVC   SPLKTIM,DBSELTIM     AND TIME                                    
         MVI   SPLKWKLY,C'N'                                                    
         TM    WKLYOPT,X'80'       TEST WEEKLY LOOKUP                           
         BZ    GETAFD6             NO                                           
         TM    WKLYIND,X'40'       TEST NON-WEEKLY DATA FOUND                   
         BO    GETAFD6                                                          
         MVI   SPLKWKLY,C'Y'       INDICATE WEEKLY DATA FOUND                   
*                                                                               
GETAFD6  L     R1,SAVER1                                                        
         L     RF,0(R1)                       GET A(SPLK BLOCK)                 
         MVC   0(L'SPDEMLK,RF),SPDEMLK        COPY BLOCK TO USER'S              
         SPACE 1                                                                
* CALL COMINTER IF PASS1 OR PASS2 , CALLER IS WRITER AND POSTING                
* AGAINST NIELSEN                                                               
COMAFD10 DS    0C                                                               
         IF (CLI,COMPASS,E,X'01'),OR,(CLI,COMPASS,E,X'02'),ANDIF,               
            (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,DBVOPT,E,X'40'),ANDIF,                                         
            (TM,SPLKOPT2,SPLKOPT2_COMS,O)   IF WE HAVE COMSCORE DEMOS           
         GOTOR COMSCORE,DMCB,=C'AFFID',DBLOCK                                   
*                                                                               
* IF WE DO NOT HAVE NIELSEN DEMOS, WE MUST USE THE COMSCORE PROGRAM             
* NAME                                                                          
         L     R1,SAVER1                      A(USER'S PARMS)                   
         L     RF,0(R1)                       GET A(SPDEMLK)                    
         IF    (TM,SPLKOPT2,SPLKOPT2_NLSN,Z)                                    
         MVC   SPLKPRG-SPDEMLK(L'STNAME,RF),STNAME                              
         ENDIF                                                                  
*                                                                               
         CLI   COMERRF,0                                                        
         BE    COMAFD20                                                         
         OI    SPLKCERR,SPLKCBAD                                                
         MVI   SPLKCERR-SPDEMLK(RF),SPLKCBAD  SET COMSCORE ERROR                
*                                                                               
COMAFD20 BAS   RE,GETADJ             GET SVI VALUES                             
         LA    R1,DTDEMSA                                                       
         BAS   RE,GDXSVI                                                        
         ENDIF                                                                  
*                                                                               
* DIVIDE ADJ DEMS BY UNADJ DEMS TO GET ACTUAL SVIS *                            
         SPACE 1                                                                
         LA    R1,DTDEMSA                                                       
         BRAS  RE,GDSVI                                                         
         SPACE 1                                                                
* NOW RETURN ==ADJ== DEMO VALUES AND SVIS IN USER AREA *                        
         SPACE 1                                                                
         LA    R1,DTDEMSA                                                       
         BRAS  RE,SETVALS                                                       
         SPACE 1                                                                
* CALCULATE WEIGHTED DEMO IF REQUIRED *                                         
         SPACE 1                                                                
         OC    SAVEWGTS,SAVEWGTS   TEST ANY WEIGHTS PRESENT                     
         BZ    GETAFD12            NO - SKIP WEIGHTED DEMO CALC                 
*                                                                               
***      BAS   RE,GETWD                                                         
         BRAS  RE,GETWD                                                         
*                                                                               
GETAFD12 OC    SPLKASPT,SPLKASPT   TEST FOR SPOT TABLE LOOKUP                   
         BZ    GETAFD13            NO                                           
         L     R1,SAVER1           R1=A(PARM LIST)                              
         L     RE,0(R1)            RE=A(CALLER'S BLOCK)                         
         ST    R6,SPLKAAFD-SPDEMLK(RE) RETURN A(THIS TABLE ENTRY)               
         BAS   RE,CALLHOOK                                                      
         B     GETAFDX                                                          
*                                                                               
GETAFD13 OC    SPLKAAFD,SPLKAAFD   TEST USER AFFID CALL                         
         BNZ   EXIT                YES - EXIT NOW                               
         BAS   RE,CALLHOOK         ELSE CALL HOOK                               
         SPACE 1                                                                
* ADD TO AFD TOTALS *                                                           
         SPACE 1                                                                
         LA    R0,MAXDEMS*2                                                     
         SR    R1,R1                                                            
GETAFD14 L     RE,DTDEMSA(R1)                                                   
         A     RE,AFDEMSA(R1)                                                   
         ST    RE,AFDEMSA(R1)                                                   
         LA    R1,4(R1)                                                         
         BCT   R0,GETAFD14                                                      
*                                                                               
GETAFDX  XC    DTDEMSA,DTDEMSA     CLEAR FOR NEXT LOOK-UP                       
         XC    DTDEMSU,DTDEMSU                                                  
         XC    STNAME,STNAME                                                    
         XC    ENDNAME,ENDNAME                                                  
         XC    DTVUTS,DTVUTS                                                    
         XC    STNAMEOV,STNAMEOV                                                
         XC    ENDNAMEO,ENDNAMEO                                                
         MVI   DEMANDSW,0                                                       
*                                                                               
         MVC   LPMLKOPT,SVLPMLKOPT RESTORE ORIGINAL OPTIONS                     
         MVC   WVPHOPT,SVWVPHOPT                                                
*                                                                               
         CLI   DBSELMED,C'R'                                                    
         BNE   *+22                                                             
         XC    RADEMSA,RADEMSA                                                  
         XC    RADEMSU,RADEMSU                                                  
         XC    RADEMSU2,RADEMSU2                                                
*                                                                               
         LA    R0,4                RTGS/IMPS/PUTS/TOTS                          
         L     R1,ADTUNVS                                                       
         XC    0(IUNDEMS*4,R1),0(R1)                                            
         LA    R1,IUNDEMS*4(R1)                                                 
         BCT   R0,*-10                                                          
         MVI   AFDSW,C'N'          SET AFFID LOOKUP FINIS                       
         B     EXIT                                                             
         SPACE 1                                                                
* IF ERROR OCCURS ON AFFID LOOK-UP TREAT AS NOAFD SPOT                          
         SPACE 1                                                                
GETAFD20 DS    0H                                                               
         LH    RE,NOAFDCNT                                                      
         LA    RE,1(RE)                                                         
         STH   RE,NOAFDCNT                                                      
         LH    RE,AFDCNT                                                        
         BCTR  RE,0                                                             
         STH   RE,AFDCNT                                                        
         B     GETAFDX                                                          
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
* DEMAND RECORD PROCESSING HOOK                                  *              
* TRY TO REMEMBER NOT TO USE DMCB AS IT WAS PARAM LIST TO DEMAND *              
******************************************************************              
         SPACE 2                                                                
GETDEMHK NTR1                                                                   
*                                                                               
* 08/06/2018 THIS WILL GO IN 18.3                                               
* COMMENT OUT FOR AUGUST INTERMIM RELEASE OF LIVE+1 CODE                        
*                                                                               
* FOR RADIO MULTIBOOK AVERAGE REQUESTS WE WANT TO SEE                           
* IF WE ARE PROCESSING DIFFERENT BOOK THAN THE FIRST BOOK OF THE AVG            
*  REST OF THE BOOKS IN AVERAGE SHOULD USE THE SAME UNIVERSE VALUES             
* AS THE 1ST BOOK                                                               
         CLI   RMBKFLG,C'Y'                                                     
         BNE   GTDMHK10                                                         
         BRAS  RE,RMBKUNIV                                                      
*                                                                               
*                                                                               
*                                                                               
*  MAKE SURE WE ARE INDEXING OFF THE SAME ROTATION WE READ FOR WTP              
GTDMHK10 L     RE,DBAQUART         GET ADDRESS OF QHELEM                        
         OC    DAYREAD,DAYREAD                                                  
         JZ    GDHK1                                                            
GDHKSUN  CLI   2(RE),X'70'         SUNDAY                                       
         BNE   *+16                                                             
         TM    DAYREAD,X'01'       IF SUNDAY WASNT READ FOR SMM WTP             
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKSAT  CLI   2(RE),X'60'         SAT                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'02'       IF SAT WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKFRI  CLI   2(RE),X'50'         FRI                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'04'       IF FRI WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKTHU  CLI   2(RE),X'40'         THU                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'08'       IF THU WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKWED  CLI   2(RE),X'30'         WED                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'10'       IF WED WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKTUE  CLI   2(RE),X'20'         TUE                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'20'       IF TUE WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKMON  CLI   2(RE),X'10'         MON                                          
         BNE   GDHK1                                                            
         TM    DAYREAD,X'40'       IF MON WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
                                                                                
GDHKEXC  LH    R0,DBDIVSOR         IF WE ARE IGNORING THIS QHR THEN             
         SHI   R0,1                ADJUST DIVSOR FOR CORRECT WEIGHT             
         STH   R0,DBDIVSOR                                                      
         B     EXIT                                                             
                                                                                
*                                                                               
GDHK1    CLI   DEMANDSW,0          TEST HERE PREVIOUSLY                         
         BNE   GDHK2               YES                                          
         MVI   DEMANDSW,C'Y'       SET SWITCH THAT WE WERE HERE                 
*                                                                               
         LA    R0,4                CLEAR DEMAND RTG/PUT/IMP/TOT AREAS           
         L     R1,ADTDEMS                                                       
         XC    0(4*IUNDEMS,R1),0(R1)                                            
         LA    R1,4*IUNDEMS(R1)                                                 
         BCT   R0,*-10                                                          
         XC    RADEMSA,RADEMSA                                                  
         XC    RADEMSU,RADEMSU                                                  
* MUST SAVE ACTUAL BOOK NOW (EXPLETIVE DELETED) *                               
         MVC   SVACTBK,DBACTBK     SET IT BECAUSE DEMOUT CHANGES IT             
         SPACE 1                                                                
* CALL DEFINE FOR MARKET NUMBER *                                               
         SPACE 1                                                                
         GOTO1 VDEFINE,P1,=C'MARKET',DBLOCK,SAVEMKT                             
         EJECT                                                                  
* EXTRACT UNIVS/RTGS/IMPS/PUTS/TOTS *                                           
         SPACE 1                                                                
GDHK2    TM    FLAGS,CANHPT        TEST CANADIAN HPT'S                          
         BNZ   GDHKCN                                                           
         CLI   SPLKMED,C'D'                                                     
         BE    GDHKCN                                                           
         CLI   SPLKMED,C'U'        RADIO COUNTY COVERAGE                        
         BE    GDHKCN                                                           
*                                                                               
GDHK2A   CLI   SPLKMED,C'R'        TEST RADIO                                   
         BE    GDHKRD                                                           
*                                                                               
*                                                                               
         CLI   DBSELMED,C'N'       USE W/NETWORK TOO                            
         BE    *+8                                                              
         CLI   DBSELMED,C'T'       TAPEOPT - USTV                               
         BE    *+8                                                              
         CLI   DBSELMED,C'O'       TAPEOPT - TP OVERNIGHTS                      
         BE    *+8                                                              
         MVI   TAPEOPT,C'N'        OTHERWISE RESET                              
*                                                                               
         L     RE,DBAREC           MAKE SURE BOSTON GETS                        
         CLC   0(3,RE),=C'RWN'     EXTENDED PRECISION FOR FILES                 
         BNE   BOSWTPX             WITH PERSONS DEMOS                           
* FREE UP- WE DONT HAVE WEEKLY ANYMORE AND WE ALSO ALWAYS FORCE TO IMP          
* BASED                                                                         
*&&DO                                                                           
         CLC   DBACTRMK,=H'106'                                                 
****     BNE   BOSWTPX                                                          
         BNE   GDHK03              CHECK 5E ELEMENT                             
         CLC   DBACTBK,=X'6627'                                                 
         BL    BOSWTPX                                                          
         MVI   TAPEOPT,C'Y'        HAVE DEMOS - FORCE TO IMP BASED              
         B     BOSWTPX                                                          
*&&                                                                             
** OTHER WEEKLY MARKETS - CHECK IF LPM BY CHECKING '5E' ELEMENT                 
** IF TTN IN '5E' THEN WE ARE LPM                                               
GDHK03   LH    RE,DBDTADSP                                                      
         L     R0,DBAREC                                                        
         AR    RE,R0                                                            
         SR    R0,R0                                                            
GDHK04   CLI   0(RE),0            TEST E-O-R                                    
         BE    BOSWTPX                                                          
         CLI   0(RE),X'5E'                                                      
         BE    GDHK05                                                           
         LLC   R0,1(RE)           LENGTH OF ELEMENT                             
         AR    RE,R0                                                            
         B     GDHK04                                                           
GDHK05   CLC   =C'TTN',2(RE)      LPM WEEKLY?                                   
         BNE   GDHK05X                                                          
         MVI   TAPEOPT,C'Y'       IMPRESSION BASED                              
         B     BOSWTPX                                                          
GDHK05X  DS    0C                                                               
         MVI   TAPEOPT,C'N'       RATINGS BASED                                 
*                                                                               
BOSWTPX  DS    0C                                                               
*                                                                               
         CLI   WVPHOPT,C'Y'        MUST FORCE FOR WTP OPTION                    
         BNE   *+8                                                              
         MVI   TAPEOPT,C'Y'                                                     
*                                                                               
         MVC   DBTAPEP,TAPEOPT                                                  
         GOTO1 VGETIUN,P1,(IUNTYPE,DBLOCK),ADTUNVS,DEMOLIST                     
         SPACE 1                                                                
* LOOK UP VUTS (HUTS USED TO DERIVE SHARES) *                                   
         SPACE 1                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         MVC   VUTLST,VUTLIST                                                   
         MVC   PUTLST,PUTLIST                                                   
         MVC   UNILST,UNILIST                                                   
         GOTO1 (RF),P1,(C'L',VUTLST),DBLOCK,QHVUTS                              
         OC    QHVUTS(4),QHVUTS    ANY VUTS                                     
         BNZ   GDHK06                                                           
         GOTO1 (RF),P1,(C'L',PUTLST)   NO - USE PUTS                            
GDHK06   CLI   TAPEOPT,C'Y'                                                     
         BNE   GDHK10                                                           
         GOTO1 (RF),P1,(C'L',UNILST),DBLOCK,QHUNIV                              
         L     RE,QHUNIV           CONVERT TO IMP BASED                         
         L     RF,QHVUTS                                                        
         MR    RE,RE                                                            
         ST    RF,QHVUTS                                                        
         L     RE,QHUNIV+4                                                      
         L     RF,QHVUTS+4                                                      
         MR    RE,RE                                                            
         ST    RF,QHVUTS+4                                                      
         L     RE,QHUNIV+8                                                      
         L     RF,QHVUTS+8                                                      
         MR    RE,RE                                                            
         ST    RF,QHVUTS+8                                                      
         SPACE 1                                                                
* SET 1 WEEK DATA INDICATOR *                                                   
* IF EXACTLY ONE OR TWO BITS ON, CONSIDER THE DATA TO BE WEEKLY *               
         SPACE 1                                                                
GDHK10   MVC   DUB(1),DBACT1WK     GET REQUESTED WEEK BITS                      
         L     RE,DBAQUART         GET ADDRESS OF QHELEM                        
         XC    DUB(1),5(RE)        CLEAR REQUESTED WEEK BITS                    
         MVI   DUB+1,X'80'         SET IND FOR WEEKLY DATA                      
*                                                                               
         CLI   DUB,0               TEST ANY UNREQUESTED WEEKS                   
         BE    GDHK20              NO                                           
*                                                                               
         LLC   RE,DUB                                                           
         SLL   RE,28                                                            
         B     *+8                                                              
*                                                                               
         SLL   RE,1                                                             
         LTR   RE,RE               SHIFT UNTIL NEGATIVE                         
         BP    *-6                                                              
         SLL   RE,1                SHIFT ONE MORE TIME                          
         LTR   RE,RE               AND SEE IF ANY MORE BITS                     
         BZ    GDHK20              IF NO MORE, EXACTLY TWO WERE ON              
         MVI   DUB+1,X'40'         ELSE SET IND FOR NON-WEEKLY DATA             
*                                                                               
GDHK20   OC    WKLYIND,DUB+1       SAVE FOR LATER                               
         EJECT                                                                  
* ACCUMULATE VUTS FOR HOME SHARE CALCULATIONS *                                 
         SPACE 1                                                                
         LA    R0,3                                                             
         LA    R4,QHVUTS                                                        
         LA    R5,DTVUTS                                                        
         SR    R7,R7                                                            
*                                                                               
GDHK24   SR    RE,RE                                                            
         L     RF,0(R4,R7)         GET VALUE                                    
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
         M     RE,=F'10'                                                        
         ST    RF,0(R4,R7)         SAVE ROUNDED VALUE                           
         MH    RF,DBFACTOR         X NUM QH                                     
         A     RF,0(R5,R7)         ADD TO TOTAL                                 
         ST    RF,0(R5,R7)                                                      
         LA    R7,4(R7)                                                         
         BCT   R0,GDHK24                                                        
*                                                                               
         CLI   TAPEHHSR,C'N'                                                    
         BE    GDHKSHX                                                          
*********************************************************************           
* IUN SHARE                                                                     
* THIS CODE GETS ORIGINAL HOUSEHOLD SHARES FROM TAPE                            
         DS    0H                                                               
         CLI   TAPEOPT,C'Y'        WE NOW WANT ORIG HOME SHR W/ IMP-BAS         
         BNE   GDHKSHX                                                          
         XC    OHOMSHR,OHOMSHR                                                  
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),P1,(C'L',DEMOSHR),DBLOCK,OHOMSHR                            
                                                                                
         LHI   R0,3                     SET HOME SHRS FOR SUMMARY LEVEL         
         ZICM  R1,DBFACTOR,(3)           R1 = WEIGHTING FACTOR                  
         SR    R7,R7                                                            
GDHKSHG  DS    0H                                                               
         SR    RE,RE                                                            
         L     RF,OHOMSHR(R7)                                                   
         MR    RE,R1                                                            
         A     RF,TOTHMSHR(R7)                                                  
         ST    RF,TOTHMSHR(R7)           TOTHMSHR = CUMULATIVE AREA             
         AHI   R7,4                                                             
         BCT   R0,GDHKSHG                                                       
*                                                                               
*********************************************************************           
GDHKSHX  EQU   *                                                                
         SPACE 1                                                                
* REQUEST PROGRAM NAME + WEEKS *                                                
         SPACE 1                                                                
GDHK28   LA    R4,ENDNAME                                                       
         CLI   DBSELXQH,C'Y'       TEST EXTENDED QH LOOKUP                      
         BNE   GDHK30                                                           
         LA    R4,NEWNAME                                                       
         CLI   XQHNUM,0            TEST FIRST TIME                              
         BE    GDHK30              YES                                          
         MVC   ENDNAME,NEWNAME                                                  
*                                                                               
GDHK30   GOTO1 VDEFINE,P1,=C'PROG+',DBLOCK,(R4)                                 
         OC    STNAME,STNAME                                                    
         BNZ   *+10                                                             
         MVC   STNAME,ENDNAME                                                   
*                                                                               
*                                                                               
         CLI   DBSELXQH,C'Y'       TEST EXTENDED QH LOOKUP                      
         BNE   GDHK30X                                                          
         CLI   XQHNUM,1            TEST TO SET START NAME                       
         BH    GDHK30X             NO                                           
         MVC   STNAME,NEWNAME      SET NAME                                     
         LLC   RE,XQHNUM           AND ADJUST COUNTER                           
         LA    RE,1(RE)                                                         
         CLC   DBACTSQC,DBACTEQC   TEST REC COVERS MULT QH'S                    
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,XQHNUM                                                        
         EJECT                                                                  
GDHK30X  DS    0H                                                               
         OC    SPLKHOOK,SPLKHOOK   TEST USER HOOK ACTIVE                        
         BZ    EXIT                NO                                           
         SPACE 1                                                                
* NEED TO CALL SPGETIUN AND ASK FOR QH VALUES ONLY *                            
         SPACE 1                                                                
         L     RE,AIUNVS           BETTER TO START WITH NOTHING                 
         LA    R0,5                                                             
         XC    0(4*IUNDEMS,RE),0(RE)                                            
         LA    RE,4*IUNDEMS(RE)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         LH    R0,DBFACTOR         SAVE DBFACTOR                                
         MVC   DBFACTOR,=H'1'      FORCE FACTOR                                 
         MVC   DBTAPEP,TAPEOPT                                                  
         GOTO1 VGETIUN,DMCB,(IUNTYPE,DBLOCK),AIUNVS,DEMOLIST                    
         STH   R0,DBFACTOR         NOW RESTORE IT                               
         SPACE 1                                                                
* COPY OLD TO NEW TO COMPLETE IUN RECORD *                                      
         SPACE 1                                                                
         LA    R0,4                                                             
         L     R4,AIUNOLD                                                       
         L     R5,AIUNNEW                                                       
         MVC   0(IUNDEMS*4,R5),0(R4)                                            
         LA    R4,IUNDEMS*4(R4)                                                 
         LA    R5,IUNDEMS*4(R5)                                                 
         BCT   R0,*-14                                                          
         SPACE 1                                                                
* NOW COMPUTE THE HOME SHARES = RATING/VUT                                      
         SPACE 1                                                                
         LA    R0,3                R0=N'HOMES SHARE VALUES                      
         SR    R7,R7               R7=INDEX REGISTER                            
*                                                                               
GDHK32   SR    RF,RF                                                            
         CLI   TAPEOPT,C'Y'        DONT SAVE SHARES FOR IMP BASED               
         BE    GDHK36        NOTE- (PUTS NEED TO BE MULTIPLIED BY               
*                                   UNIV IF SAVED SHARES NEEDED)                
         L     RF,AIRTGOLD                                                      
         L     RF,IUNHMDSP(RF,R7)                                               
         M     RE,=F'1000'                                                      
*                                                                               
         L     R1,QHVUTS(R7)       PICK UP VUT                                  
         TM    SHRSW,X'80'         TEST TO COMPUTE SHARES                       
         BZ    *+12                                                             
         L     R1,AIPUTOLD                                                      
         L     R1,IUNHMDSP(R1,R7)  PICK UP ACTUAL PUT                           
*                                                                               
         LTR   R1,R1               TEST FOR ZERO PUT                            
         BNZ   GDHK34                                                           
         SR    RF,RF               YES-SET SHARE TO ZERO                        
         B     GDHK36                                                           
*                                                                               
GDHK34   DR    RE,R1                                                            
*                                                                               
GDHK36   L     RE,AIUNXTRA                                                      
         ST    RF,0(RE,R7)         SET SHARE VALUE IN IUN RECORD                
*                                                                               
         LA    R7,4(R7)                                                         
         BCT   R0,GDHK32                                                        
                                                                                
*                                                                               
         DS    0H                                                               
*                                                                               
         CLI   TAPEHHSR,C'N'                                                    
         BE    GDHK36X                                                          
                                                                                
         CLI   TAPEOPT,C'Y'        WE NOW WANT ORIG HOME SHR W/ IMP-BAS         
         BNE   *+14                                                             
         L     RE,AIUNXTRA                                                      
         MVC   0(L'OHOMSHR,RE),OHOMSHR  SET HOME SHRS FOR DETAIL LEVEL          
*                                                                               
GDHK36X  L     RE,ADBLOCKS         USE THIS FOR DBLOCK SAVE AREA                
         MVC   0(256,RE),DBLOCK                                                 
*                                                                               
         MVC   DBFILE,=C'PAV'                                                   
         L     RE,ASVIREC                                                       
         ST    RE,DBAREC           SET IUN REC ADDRESS                          
         XC    0(200,RE),0(RE)     INIT RECORD                                  
         LA    RE,23(RE)           POINT TO FIRST DEMO ELEMENT                  
         MVI   0(RE),0             SET END OF RECORD                            
         ST    RE,DBAQUART         SET A(QUARTER HOUR)                          
         LA    R0,IUNDEMS*9+6                                                   
         STH   R0,DBNUMVLS                                                      
*                                                                               
         OC    SPLKSPL,SPLKSPL     TEST SPILL LOOK-UP                           
         BZ    GDHK38              NO                                           
         CLI   DBSELSRC,C'F'       ALLOW FUSION AND NSI WIRED                   
         BE    GDHK38              IMPRESSIONS                                  
         CLI   DBSELMED,C'O'                                                    
         BE    GDHK38                                                           
         CLI   DBSELMED,C'T'                                                    
         BNE   GDHK37                                                           
* 03/2020 AS PART OF THE LIVE+1 REQUEST TO SLOT TSA IMPS AND TOTS FROM          
* DMA WE ARE GOING TO DISABLE CLEARING TSA TOTS AND IMPS FOR SPILL              
* LOOKUPS AS PER MARIA DASILVA. DO THIS FOR NSI TV                              
         CLI   DBSELSRC,C'N'                                                    
         BE    GDHK38                                                           
*&&DO                                                                           
* CHECKING FOR NIELSEN /TV TO SKIP CLEARING TSA IMPS AND TOTALS                 
* MAKE THESE BOOKTYPE CHECKS UNNECESSARY                                        
* COMMENT OUT TO FREE UP ADDRESSIBLITY                                          
         CLI   DBBTYPE,BOOKTYPE_YL   IMPACT DATA                                
         BL    GDHK36Z                                                          
         CLI   DBBTYPE,BOOKTYPE_Y7   SHOW IMPRESSIONS EXCEPT                    
         BH    GDHK36Z               FOR YU,YD, AND YC                          
         CLI   DBBTYPE,BOOKTYPE_YU                                              
         BE    GDHK36Z                                                          
         CLI   DBBTYPE,BOOKTYPE_YD                                              
         BE    GDHK36Z                                                          
         CLI   DBBTYPE,BOOKTYPE_YC                                              
         BE    GDHK36Z                                                          
         B     GDHK38                                                           
*                                                                               
GDHK36Z  CLI   DBBTYPE,BOOKTYPE_W1                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_WS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_W3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BE    *+8                                                              
         CLI   DBBTYPE,C'Z'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'4'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BE    GDHK38                                                           
*&&                                                                             
GDHK37   L     RE,AIIMPOLD         CLEAR ALL IMPS/TOTS                          
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AITOTOLD                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AIIMPNEW                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AITOTNEW                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         SPACE 1                                                                
* CALL DEMAINT TO BUILD IUN RECORD *                                            
         SPACE 1                                                                
GDHK38   L     RF,DBCOMFCS                                                      
         L     RF,CDEMAINT-COMFACSD(RF)                                         
         MVC   WORK(10),OFORMAT                                                 
         CLC   SVACTBK,=X'5801'    SWITCH FORMULAS AT JAN/88                    
         BL    *+10                                                             
         MVC   WORK+7(2),=X'530B'                                               
         CLI   TAPEOPT,C'Y'        SWITCH FORMULAS FOR TAPE BASED               
         BNE   *+10                                                             
         MVC   WORK+7(2),=X'5A0B'                                               
         CLI   DBACTSRC,C'C'       PROCESS SAME AS NIELSEN                      
         BNE   *+8                                                              
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 (RF),DMCB,REP,DBLOCK,AIUNWK,WORK                                 
         SPACE 1                                                                
* CALL DEMOUT TO EXTRACT REQUESTED DEMOS *                                      
         SPACE 1                                                                
*                                                                               
*CHECK THE EXTENSION FOR SYSCODE FOR FUSION FILE                                
* ALLOW SYSCODE FOR NSI ALSO                                                    
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   GDHK39                                                           
         XC    DBSELSYC,DBSELSYC                                                
         ICM   RF,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GDHK39                                                           
         USING SPLKXTD,RF                                                       
         MVC   DBSELSYC,SPXTSYSC                                                
         DROP  RF                                                               
*                                                                               
GDHK39   L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,QHDEMSU                         
*                                                                               
         L     RE,ADBLOCKS         RESTORE ORIGINAL DBLOCK                      
         MVC   DBLOCK(256),0(RE)                                                
         EJECT                                                                  
* CALL USER HOOK FOR EACH QUARTER HOUR IN CURRENT LOOKUP                        
* WITH SINGLE QH SVI FACTORS                                                    
         SPACE 1                                                                
         L     RE,ASVIREC                                                       
         XC    0(9,RE),0(RE)       CLEAR SVI GARBAGE                            
*                                                                               
GDHK40   MVC   SVACTSQC(2),DBACTSQC   SAVE CURRENT ACTUAL START/END QH          
*                                                                               
GDHK42   MVC   DBACTEQC,DBACTSQC      FORCE ONE QH                              
         TM    FLAGS,CANHPT        TEST CANADIAN HPT'S                          
         BNZ   GDHK43                                                           
         CLI   SPLKMED,C'R'        FOR NOW, ARB RADIO FOLLOWS                   
         BE    GDHK43               BBM CANADIEN                                
*                                                                               
****     BAS   RE,GETADQTB                                                      
         BRAS  RE,GETADQTB                                                      
         LLC   R1,DBDQPTR                                                       
         BCTR  R1,0                                                             
         MHI   R1,DBDQLEN                                                       
         A     R1,ADBDQD             POINT TO CURRENT ENTRY                     
*                                                                               
         XC    ADJQTAB,ADJQTAB       AND BUILD 1 ENTRY TABLE                    
         MVC   ADJQDAY(1),0(R1)      MOVE DAY                                   
         MVC   ADJQSQH(2),DBACTSQC   SET START/END QH                           
         MVI   ADJQWGT,1             FORCE WEIGHT                               
         MVI   ADJQTAB+DBDQLEN,X'FF' SET EOL FLAG                               
*                                                                               
GDHK43   BAS   RE,GETADJ             GET SVI VALUES                             
         SPACE 1                                                                
         L     R4,SPLKALST                                                      
         LA    R5,QHDEMSU                                                       
         LA    R6,SVIS                                                          
         L     R7,SPLKAVAL                                                      
*                                                                               
GDHK44   L     RF,0(R5)            DEMO VALUE                                   
         M     RE,0(R6)            X SVI                                        
         AHI   RF,50                                                            
         D     RE,=F'100'                                                       
         ST    RF,0(R7)            PASS ADJUSTED DEMO VALUE                     
         MVC   4(4,R7),0(R6)       AND SVI                                      
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         LA    R7,8(R7)                                                         
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   GDHK44                                                           
         L     R1,SPLKAVAL                                                      
*                                                                               
         L     R1,SAVER1                                                        
         L     RF,0(R1)                                                         
         MVC   SPLKPRG-SPDEMLK(16,RF),ENDNAME                                   
         MVC   DBFACTOR,=H'1'      FORCE FACTOR = 1                             
         BAS   RE,CALLHOOK                                                      
*                                                                               
         LLC   RE,DBACTSQC                                                      
         LA    RE,1(RE)            INCREMENT QH                                 
         STC   RE,DBACTSQC                                                      
         CLC   DBACTSQC,SVACTEQC                                                
         BNH   GDHK42                                                           
         MVC   DBACTSQC(2),SVACTEQC   RESTORE ACTUAL START/END QH               
         B     EXIT                                                             
         EJECT                                                                  
GDHKRD   DS    0H                  FOR RADIO, DO MY OWN "MAD"                   
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLST2),DBLOCK,DTDEMSU                         
* HAVE TO ADD CODE TO CAL TSL HERE                                              
         BRAS  RE,ADJTSL                                                        
         BRAS  RE,RMAD                                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,DTDEMSU                         
         BRAS  RE,ADJTSL                                                        
         B     GDHKCN10                                                         
         SPACE 2                                                                
GDHKCN   DS    0H                                                               
         XC    WORK,WORK           MAD RECORD INTO ASVIREC                      
         LA    R0,DBLOCK                                                        
         ST    R0,WORK                                                          
         MVC   WORK+6(2),DBFACTOR                                               
         CLI   DBSELMED,C'U'       RADIO COUNTY COVERAGE SHOW                   
         BNE   *+10                TOTAL NUMBER -DONT AVG                       
         MVC   WORK+6(2),=X'0001'                                               
         MVC   WORK+8(3),DBFILE                                                 
         MVC   WORK+11(3),DBFILE                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOMTH-COMFACSD(RF)                                         
         L     RE,DBAREC                                                        
         OC    0(3,RE),0(RE)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* COUNTY COVERAGE - ARE WE READING FOR COUNTY LEVEL RECS FOR ONLY UNV?          
         CLI   READCUNV,C'N'                                                    
         BE    GDHKCN06                                                         
         CLI   READCUNV,C'Y'                                                    
         BE    GDHKCN07                                                         
         B     GDHKCN08                                                         
* FOR COUNTY COVERAGE - STATION LEVEL READ CALL DEMOMATH WITH                   
* OPTION TO NOT PASS BACK UNIVERSE                                              
GDHKCN06 GOTO1 (RF),DMCB,(C'X',=C'MAD'),DBAREC,ASVIREC,WORK                     
         OC    SVDIVSOR,SVDIVSOR     SAME DIVSOR FOR 1 DAYPART                  
         BNZ   *+10                                                             
         MVC   SVDIVSOR,DBDIVSOR                                                
         B     GDHKCN10                                                         
* FOR COUNTY COVERAGE - COUNTY LEVEL READ CALL DEMOMATH WITH                    
* OPTION TO ONLY MAD UNIVERSES AND KEEP OTHER DEMOS FROM PREVIOUS               
* READS.                                                                        
* THIS SHOULD GIVE US THE DEMOS FROM THE STATION LEVEL READ WITH THE            
* UNIVERSES FROM THE COUNTY LEVEL READS                                         
GDHKCN07 GOTO1 (RF),DMCB,(C'L',=C'MAD'),DBAREC,ASVIREC,WORK                     
         B     GDHKCN10                                                         
*                                                                               
*                                                                               
*                                                                               
GDHKCN08 DS    0H                                                               
         TM    DBNPMFLG,DBNPMACH                                                
         BNO   GDHKCN8B                                                         
         TM    DBVOPT,X'40'         POSTING USE DOUBLE WORD ARITHMETIC          
         BO    GDHKCN09             BPOO                                        
GDHKCN8B GOTO1 (RF),DMCB,=C'MAD',DBAREC,ASVIREC,WORK                            
         B     GDHKCN10                                                         
GDHKCN09 GOTO1 (RF),DMCB,=C'DMA',DBAREC,ASVIREC,WORK   DOUBLE WORD-BPOO         
*                                                                               
GDHKCN10 LA    R4,ENDNAME                                                       
         GOTO1 VDEFINE,P1,=C'PROG+',DBLOCK,(R4)                                 
         OC    STNAME,STNAME                                                    
         BNZ   *+10                                                             
         MVC   STNAME,ENDNAME                                                   
*                                                                               
         OC    SPLKHOOK,SPLKHOOK   TEST USER HOOK ACTIVE                        
         BZ    EXIT                                                             
         SPACE 1                                                                
* NOW EXTRACT VALUES FOR USER HOOK *                                            
         SPACE 1                                                                
         CLI   DBSELMED,C'R'       VALUES ALREADY EXTRACTED FOR RADIO           
         BE    GDHKCN20                                                         
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,DTDEMSU                         
GDHKCN20 B     GDHK40                                                           
         SPACE 2                                                                
*                                                                               
* HOOK FOR SMM OVERNIGHTS                                                       
*                                                                               
GETOVHOM NTR1                                                                   
*                                                                               
         MVI   VPWKSTPT,0      WE DIDNT READ WEEKLY -SET TO 0                   
         MVI   DEMANDSW,C'Y'                                                    
         LA    R4,ENDNAMEO                                                      
         GOTO1 VDEFINE,P1,=C'PROG+',DBLOCK,(R4)                                 
         OC    STNAMEOV,STNAMEOV                                                
         BNZ   *+10                                                             
         MVC   STNAMEOV,ENDNAMEO                                                
                                                                                
*                                                                               
* LOOK UP HOMES TO DERIVE INDEX *                                               
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),P1,(C'L',HOMDEM),DBLOCK,QHVUTS                              
*                                                                               
* GET UNIVERSE INTO 100'S TO MATCH IUN PRECSION                                 
*                                                                               
         SR    RE,RE                                                            
         L     RF,QHVUTS+4                                                      
         AHI   RF,50                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         ST    RF,OVHOMUNV                                                      
*                                                                               
         SR    RE,RE                                                            
         L     RF,QHVUTS                                                        
                                                                                
         MH    RF,DBFACTOR                                                      
         A     RF,VPHHOMES                                                      
         ST    RF,VPHHOMES                                                      
         MVC   OVPHHMES,VPHHOMES                                                
         LH    RF,DBFACTOR                                                      
         A     RF,VPHFACT                                                       
         ST    RF,VPHFACT                                                       
*                                                                               
* SAVE OFF DAYS READ SO WE CAN USE THE SAME DAYS TO READ THE MONTHLY            
* SO WE DONT HAVE A MISMATCH ON THE WEIGHTING                                   
*                                                                               
         L     RF,DBAQUART                                                      
         CLI   2(RF),X'95'          M-F RECORD                                  
         BNE   *+8                                                              
         OI    DAYREAD,B'01111100'                                              
*                                                                               
         CLI   2(RF),X'70'      SAVE OFF BITS OF DAYS READ                      
         BH    GETOVHKX                                                         
         CLI   2(RF),X'10'                                                      
         BL    GETOVHKX                                                         
         LLC   RE,2(RF)                                                         
         SRL   RE,4                                                             
         LA    R0,X'80'                                                         
         SRL   R0,1                                                             
         BCT   RE,*-4                                                           
         LLC   RE,DAYREAD                                                       
         OR    R0,RE                                                            
         STC   R0,DAYREAD                                                       
*                                                                               
GETOVHKX B     EXIT                                                             
HOMDEM   DC    X'00',C'J',X'01',X'00',C'L',X'01',X'FF'                          
         EJECT                                                                  
**************************                                                      
* CALL USER HOOK ROUTINE *                                                      
**************************                                                      
         SPACE 1                                                                
CALLHOOK NTR1                                                                   
*                                                                               
         ICM   RF,15,SPLKHOOK                                                   
         BZ    CALLHKX                                                          
         L     RE,CALLRD                                                        
         LM    R1,RC,24(RE)        RESTORE MOST USER REGS                       
         BASR  RE,RF               CALL USER HOOK                               
CALLHKX  XIT1                                                                   
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R6),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
*                                                                               
GDERRNSI NTR1                                                                   
*                                                                               
GDERR1   DS    0H                                                               
GDERR2   DS    0H                                                               
*&&DO                                                                           
GDERRX   ICM   R3,15,ADDEMEL                                                    
         BZ    *+10                                                             
         MVC   22(1,R3),DBERRMOD   FAILING MODULE ID                            
*&&                                                                             
GDERRX   ICM   R8,15,ADDEMEL                                                    
         BZ    *+10                                                             
         MVC   22(1,R8),DBERRMOD   FAILING MODULE ID                            
         L     R1,SAVER1                                                        
         MVC   0(1,R1),DBERROR     RETURN ERROR CODE                            
*                                                                               
         BAS   RE,RSTDQLNK         RESTORE DAYS/QHS TABLE BEFORE EXIT           
*                                                                               
         CLI   DBERROR,NOTFOUND    TEST NOT FOUND ERROR                         
         BNE   *+12                                                             
         MVI   0(R1),X'45'         TRANSLATE TO OLD ERROR CODE                  
         B     EXIT                                                             
*                                                                               
         CLI   DBERROR,INVFM       TEST INVALID FILE/MEDIA                      
         BL    GDERRX2                                                          
         CLI   DBERROR,INVFMS                                                   
         BH    GDERRX2                                                          
         MVI   0(R1),X'45'         TRANSLATE TO OLD ERROR CODE                  
         B     EXIT                                                             
*                                                                               
GDERRX2  B     EXIT                                                             
*&&DO                                                                           
* COMMENT OUT- LABELS NEVER REFERENCED                                          
GDERR3   B     GDERRX                                                           
GDERR4   B     GDERRX                                                           
GDERR5   B     GDERRX                                                           
GDHKERR1 B     GDERRX                                                           
GDHKERR2 B     GDERRX                                                           
GDHKERR3 B     GDERRX                                                           
GDHKERR4 B     GDERRX                                                           
*&&                                                                             
         EJECT                                                                  
**************************************************************                  
*                                                            *                  
* SUBROUTINE TO BUILD IUN RECORD AND EXTRACT REQUESTED DEMOS *                  
*                                                            *                  
**************************************************************                  
         SPACE 1                                                                
GETIUN   NTR1                                                                   
         TM    FLAGS,CANHPT        TEST CANADIAN HPT'S                          
         BNZ   GETCN                                                            
         CLI   SPLKMED,C'D'        TEST FOR USTV DAYPART                        
         BE    GETCN                                                            
*DONT HAVE THIS ANYMORE                                                         
         CLI   SPLKMED,C'U'        TEST FOR RADIO COUNTY COVERAGE               
         BE    GETCN                                                            
         CLI   SPLKMED,C'R'        TEST FOR RADIO                               
         BE    GETRAD                                                           
* COMMENT THIS OUT.  NCM WILL NOT ROLL UP THE WEEKS DATA IN EACH                
* QTR HOUR                                                                      
*&&DO                                                                           
* NCM DONT UNWEIGHT FOR NOW                                                     
         CLI   DBSELSRC,C'C'                                                    
         BNE   *+10                                                             
         MVC   DBDIVSOR,=H'1'                                                   
*&&                                                                             
         LH    R0,DBDIVSOR         UNWEIGHT DEMOS                               
         ST    R0,DUB              GET DIVISOR ON WORD BOUNDARY                 
*                                                                               
         CLC   DBDIVSOR,=H'0'      NO BASE - BYPASS WVPH                        
         JE    GETIUNV1                                                         
         CLC   =F'0',VPHHOMES      NO WEEKLY - BYPASS WVPH                      
         JZ    GETIUNV1                                                         
         CLI   DMINDEX,C'Y'        FORCE TO DO INDEX                            
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BE    *+8                                                              
         CLI   WVPHOPT,C'Y'                                                     
         JNE   GETIUNV1                                                         
         L     R1,ADTUNVS          POINT TO EXPLODED DATA                       
         L     R7,IUNHMDSP(R1)     SAVE HOMES UNIVERESE                         
         ST    R7,HOMUNIV                                                       
         LA    R1,IUNDEMS*4(R1)    POINT TO DEMOS                               
         L     R7,IUNHMDSP(R1)       GET HOMES VALUE                            
         LA    R0,IUNDEMS*4        TOTAL NUMBER OF VALUES                       
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R7               UNWEIGHT THE HOMES                           
         SLDA  RE,1                                                             
         D     RE,DUB              UNWEIGHT IT                                  
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         LR    R7,RF               AND SAVE IN R7                               
         LTR   R7,R7               TEST FOR ZERO HOMES FROM BASE BOOK           
         JZ    GETIUNV1                BYPASS WVPH IF NO HOMES                  
* CHECK TO SEE IF WE READ THE OVERNIGHTS HOMES IMPRESSION                       
* IF SO UNWEIGHT IT AND USE IT                                                  
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GETIUNWV                                                         
         OC    OVPHHMES,OVPHHMES                                                
         BNZ   GETIUNOV                                                         
*                                                                               
GETIUNWV SR    RE,RE                                                            
         L     RF,VPHHOMES         UNWEIGHT THE WEEKLY RATINGS                  
         SLDA  RE,1                                                             
         D     RE,VPHFACT          UNWEIGHT IT                                  
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         SR    RE,RE                                                            
         M     RE,HOMUNIV          CONVERT TO DMA IMPS  UNIV IN (00)            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,VPHHOMES                                                      
         B     GETIUNOVX                                                        
*                                                                               
GETIUNOV SR    RE,RE                                                            
         L     RF,OVPHHMES         UNWEIGHT THE OVERNIGHTS IMP                  
         SLDA  RE,1                                                             
         D     RE,VPHFACT          UNWEIGHT IT                                  
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,VPHHOMES                                                      
*                                                                               
* INDEX THE TOTAL HOME VIEWERS BY OV UNIV AND MONTHLY UNIV                      
*                                                                               
         XC    DUB3,DUB3             PACK INPUT  VALUE                          
         CVD   RF,DUB3              RF= IMPRESSION                              
         OI    DUB3+7,X'0C'                                                     
         L     RF,HOMUNIV           MONTHLY UNIV                                
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         MP    DUB3(8),DUB2+4(4)     DUB HAS PACKED NUMBER1                     
*                                                                               
         L     RF,OVHOMUNV          OVERNIGHT HOME UNIV                         
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB3                                                   
*                                                                               
         DP    WORK(12),DUB2+4(4)                                               
         MVC   DUB3(8),WORK                                                     
         CVB   RF,DUB3                                                          
         ST    RF,VPHHOMES         UNIV INDEXED IMPRESSION                      
*                                                                               
GETIUNOVX DS    0H                                                              
*                                                                               
*                                                                               
         CLI   VPWKSTPT,0                                                       
         BE    GETIUNV                                                          
         L     RF,VPHHOMES         WEIGHT FOR WEEKS FOUND                       
         SR    RE,RE                                                            
         M     RE,VPNWKS                                                        
         ST    RF,VPHHOMES                                                      
         LLC   RE,VPWKSTPT         UNCOVERED WEEKS                              
         LR    RF,R7               RATING RECORD HOMES                          
         A     RF,VPHHOMES         ADJUST VPHHOMES                              
         LLC   RE,VPWKSTPT                                                      
         A     RE,VPNWKS                                                        
         ST    RE,VPNWKS                                                        
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,VPNWKS                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,VPHHOMES                                                      
*                                                                               
*        CALCULATE THE VPHS                                                     
*                                                                               
GETIUNV  L     RE,0(R1)            GET THE DEMO VALUE                           
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,DUB                                                           
         SR    RE,RE                                                            
         M     RE,=F'10000'        X 1000                                       
         DR    RE,R7               / HOMES                                      
         AHI   RF,1                ROUND IT                                     
         SRL   RF,1                                                             
         ST    RF,0(R1)            GIVES VPH                                    
         LA    R1,4(R1)            NEXT DEMO                                    
         BCT   R0,GETIUNV          DO FOR ALL                                   
         MVC   DUB,=F'1'           STOP THE UNWEIGHTING FOR DEMS                
*                                                                               
GETIUNV1 L     RE,AIUNVS                                                        
         L     RF,ADTUNVS                                                       
         MVC   0(4*IUNDEMS,RE),0(RF)                                            
*                                                                               
         LA    R0,IUNDEMS*4                                                     
         L     R4,ADTDEMS                                                       
         L     R5,AIUNOLD                                                       
         SR    R7,R7                                                            
*                                                                               
GETIUN2  SR    RE,RE                                                            
         L     RF,0(R4,R7)                                                      
         SLDL  RE,1                                                             
         D     RE,DUB                                                           
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
         CLC   =F'0',VPHHOMES                                                   
         BE    GETIUN2V                                                         
         CLI   DMINDEX,C'Y'        FORCE TO DO INDEX                            
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'        DOING OVERNIGHTS                             
         BE    *+8                                                              
         CLI   WVPHOPT,C'Y'        DOING WEEKLY VPH                             
         BNE   GETIUN2V                                                         
         SR    RE,RE                                                            
         M     RE,VPHHOMES                                                      
         SLDA  RE,1                                                             
         D     RE,=F'10000'                                                     
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*        SLA   RF,8                  PRECISION CORRECT                          
*                                                                               
GETIUN2V DS    0H                                                               
         ST    RF,0(R5,R7)           SET AS OLD VALUE                           
         ST    RF,IUNDEMS*16(R5,R7)  AND AS NEW                                 
         LA    R7,4(R7)                                                         
         BCT   R0,GETIUN2                                                       
         SPACE 1                                                                
* UNWEIGHT THE VUTS *                                                           
         SPACE 1                                                                
         LA    R0,3                                                             
         LA    R4,DTVUTS                                                        
*                                                                               
GETIUN4  SR    RE,RE                                                            
         L     RF,0(R4)                                                         
*        SLDA  RE,1                                                             
*        D     RE,DUB                                                           
*        AH    RF,=H'1'                                                         
*        SRL   RF,1                                                             
* NEW YORK EXCEEDS BINARY LIMIT - MUST DO IN PACKED                             
         CVD   RF,DUBP             RATING TO PACKED                             
         ZAP   DPWORK(16),DUBP                                                  
         L     RE,DUB              GET THE WEIGHT                               
         CVD   RE,DUBP             INDEX TO PACKED                              
         MP    DPWORK,=P'10'       ADJ FOR ROUND                                
         DP    DPWORK,DUBP                                                      
         ZAP   DUBP,DPWORK(8)                                                   
         AP    DUBP,=P'5'          ROUND IT                                     
         ZAP   DPWORK(16),DUBP                                                  
         DP    DPWORK,=PL8'10'                                                  
         ZAP   DUBP,DPWORK(8)                                                   
         CVB   RF,DUBP             GET IT IN BINARY                             
         ST    RF,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,GETIUN4                                                       
         SPACE 1                                                                
* NOW COMPUTE THE HOME SHARES = RATING/VUT                                      
         SPACE 1                                                                
         LA    R0,3                R0=N'HOMES SHARE VALUES                      
         SR    R7,R7               R7=INDEX REGISTER                            
*                                                                               
GETIUN6  SR    RF,RF                                                            
         CLI   TAPEOPT,C'Y'        DONT SAVE SHARES FOR IMP BASED               
         BE    GETIUN6B      NOTE- (PUTS NEED TO BE MULTIPLIED BY               
*                                   UNIV IF SAVED SHARES NEEDED)                
         L     RF,AIRTGOLD                                                      
         L     RF,IUNHMDSP(RF,R7)                                               
         M     RE,=F'1000'                                                      
*                                                                               
         L     R1,DTVUTS(R7)       PICK UP VUT                                  
         TM    SHRSW,X'80'         TEST TO COMPUTE SHARES                       
         BZ    *+12                                                             
         L     R1,AIPUTOLD                                                      
         L     R1,IUNHMDSP(R1,R7)  PICK UP ACTUAL PUT                           
*                                                                               
         LTR   R1,R1               TEST FOR ZERO PUT                            
         BNZ   GETIUN6A                                                         
         SR    RF,RF               YES-SET SHARE TO ZERO                        
         B     GETIUN6B                                                         
*                                                                               
GETIUN6A DR    RE,R1                                                            
*                                                                               
GETIUN6B L     RE,AIUNXTRA                                                      
         ST    RF,0(RE,R7)         SET SHARE VALUE IN IUN RECORD                
*                                                                               
         LA    R7,4(R7)                                                         
         BCT   R0,GETIUN6                                                       
                                                                                
                                                                                
* UNWEIGH THE HOME SHARES *                                                     
                                                                                
         DS    0H                                                               
         CLI   TAPEOPT,C'Y'        WE NOW WANT THE ORIGINAL HOME SHARES         
         BNE   GETIUNSHX            W/ IMP-BASED CALCULATIONS                   
*                                                                               
         LHI   R0,3                                                             
         SR    R1,R1                                                            
         L     R4,AIUNXTRA         OUTPUT FOR UNWEIGHTED SHARES                 
                                                                                
GETIUNSH DS    0H                                                               
         SR    RE,RE                                                            
         L     RF,TOTHMSHR(R1)      RF = WEIGHTED SHARE                         
         SLDL  RE,1                                                             
         D     RE,DUB                UNWEIGH IT,                                
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,0(R1,R4)           AND STORE IT IN OUTPUT AREA                
         AHI   R1,4                                                             
         BCT   R0,GETIUNSH                                                      
GETIUNSHX EQU  *                                                                
                                                                                
*                                                                               
GETIUN8  L     RE,ADBLOCKS         USE THIS FOR DBLOCK SAVE AREA                
         MVC   0(256,RE),DBLOCK                                                 
*                                                                               
         MVC   DBFILE,=C'PAV'                                                   
         L     RE,ASVIREC                                                       
         ST    RE,DBAREC                                                        
         XC    0(200,RE),0(RE)                                                  
         LA    RE,23(RE)           POINT TO FIRST DEMO ELEMENT                  
         MVI   0(RE),0             SET END OF RECORD                            
         ST    RE,DBAQUART         SET A(QUARTER HOUR)                          
         LA    R0,IUNDEMS*9+6                                                   
         STH   R0,DBNUMVLS                                                      
*                                                                               
         OC    SPLKSPL,SPLKSPL     TEST SPILL LOOK-UP                           
         BZ    GETIUN10            NO                                           
         CLI   DBSELSRC,C'F'                                                    
         BE    GETIUN10                                                         
         CLI   DBSELMED,C'O'                                                    
         BE    GETIUN10                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   GETIUN9                                                          
* 03/2020 AS PART OF THE LIVE+1 REQUEST TO SLOT TSA IMPS AND TOTS FROM          
* DMA WE ARE GOING TO DISABLE CLEARING TSA TOTS AND IMPS FOR SPILL              
* LOOKUPS AS PER MARIA DASILVA. DO THIS FOR NSI TV                              
         CLI   DBSELSRC,C'N'                                                    
         BE    GETIUN10                                                         
*                                                                               
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_YL   IMPACT DATA                                
         BL    GETIUN8X                                                         
         CLI   DBBTYPE,BOOKTYPE_Y7   SHOW IMPRESSIONS EXCEPT                    
         BH    GETIUN8X              FOR YU,YD, AND YC                          
         CLI   DBBTYPE,BOOKTYPE_YU                                              
         BE    GETIUN8X                                                         
         CLI   DBBTYPE,BOOKTYPE_YD                                              
         BE    GETIUN8X                                                         
         CLI   DBBTYPE,BOOKTYPE_YC                                              
         BE    GETIUN8X                                                         
         B     GETIUN10                                                         
*                                                                               
*                                                                               
GETIUN8X CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_WS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_W1                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_W3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,C'Z'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'4'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BE    GETIUN10                                                         
GETIUN9  L     RE,AIIMPOLD         CLEAR ALL IMPS/TOTS                          
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AITOTOLD                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AIIMPNEW                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AITOTNEW                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         SPACE 1                                                                
* CALL DEMAINT TO BUILD IUN RECORD *                                            
         SPACE 1                                                                
GETIUN10 L     RF,DBCOMFCS                                                      
         L     RF,CDEMAINT-COMFACSD(RF)                                         
         MVC   WORK(10),OFORMAT                                                 
         CLC   SVACTBK,=X'5801'    SWITCH FORMULAS AT JAN/88                    
         BL    *+10                                                             
         MVC   WORK+7(2),=X'530B'                                               
         CLI   TAPEOPT,C'Y'        SWITCH FORMS FOR TAPE BASED OPTION           
         BNE   *+10                                                             
         MVC   WORK+7(2),=X'5A0B'                                               
         CLI   DBACTSRC,C'C'       PROCESS SAME AS NIELSEN                      
         BNE   *+8                                                              
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 (RF),DMCB,REP,DBLOCK,AIUNWK,WORK                                 
         B     GETIUN20                                                         
*                                                                               
REP      DC    C'REP'                                                           
OFORMAT  DC    C'PAVUIUN',X'520B00'                                             
         EJECT                                                                  
* CALL DEMOUT TO EXTRACT DEMOS IN CALLERS LIST INTO DTDEMSU *                   
         SPACE 1                                                                
GETIUN20 L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
*                                                                               
*    CHECK THE EXTENSION FOR SYSCODE FOR  FUSION FILE                           
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   GETIUN21                                                         
         XC    DBSELSYC,DBSELSYC                                                
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GETIUN21                                                         
         USING SPLKXTD,RE                                                       
         MVC   DBSELSYC,SPXTSYSC                                                
         DROP  RE                                                               
GETIUN21 DS    0H                                                               
*                                                                               
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,DTDEMSU                         
         SPACE 1                                                                
* RETURN VALUES TO USER *                                                       
         SPACE 1                                                                
         LA    R4,DEMOLIST                                                      
         LA    R5,DTDEMSU                                                       
                                                                                
GETIUN22 SR    RE,RE                                                            
         L     RF,0(R5)                                                         
         ST    RF,0(R5)                                                         
         LA    R4,3(R4)            NEXT DEMO                                    
         LA    R5,4(R5)            NEXT ACCUM                                   
         CLI   0(R4),X'FF'                                                      
         BNE   GETIUN22                                                         
*                                                                               
         L     RE,ADBLOCKS         RESTORE ORIGINAL DBLOCK                      
         MVC   DBLOCK(256),0(RE)                                                
*                                                                               
****     BAS   RE,GETADQTB                                                      
         BRAS  RE,GETADQTB                                                      
         L     RE,ADBDQD                                                        
         MVC   ADJQTAB,0(RE)       MOVE DQ TABLE FOR SVI LOOKUP                 
         BAS   RE,GETADJ           GET ADJUSTMENT FACTORS                       
*                                                                               
         LA    R1,DTDEMSA                                                       
         BAS   RE,GDXSVI           MULTIPLY ADJ FACTORS X DEMOS                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* EXTRACT RADIO DEMOS *                                                         
                                                                                
GETRAD   XC    DUB(4),DUB                                                       
         MVC   DUB+2(2),DBDIVSOR                                                
         LA    RE,DEMOLST2                                                      
         LA    RF,RADEMSU2         RF-->"MAD"ED DEMO VALUES FOR RADIO           
*                                                                               
GETRAD10 CLI   0(RE),X'FF'          NEED TO "DIV" THEM OUT                      
         BE    GETRAD20                                                         
         SR    R0,R0                                                            
         ICM   R1,15,0(RF)                                                      
                                                                                
         SLDA  R0,1                ******* START OF DIVIDE LOGIC ******         
         D     R0,DUB                                                           
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                ******** END OF DIVIDE LOGIC *******         
                                                                                
         STCM  R1,15,0(RF)                                                      
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         B     GETRAD10                                                         
*                                                                               
** NOW CALCULATE DEMOS GIVEN IN ORIGINAL DEMO LIST **                           
*                                                                               
GETRAD20 LA    RE,RDEMLIST                                                      
         USING RDEMLSTD,RE                                                      
**       LA    R3,RADEMSU                                                       
         LA    R8,RADEMSU                                                       
         LA    RF,RADEMSU2                                                      
         SR    R0,R0                                                            
*                                                                               
GETRAD22 CLI   0(RE),X'FF'                                                      
         BE    GETRADX                                                          
         ICM   R1,15,0(RF)         R1=DEMO REQUESTED IN DEMOLIST                
         CLI   RDLOPER,0           DOES IT NEED TO BE CALCULATED?               
         BE    GETRAD30             NO                                          
                                                                                
         CLI   RDLOPER,C'D'        IS THE CALC A DIVIDE OPERATION?              
         BNE   GETRAD24             NO                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         OC    8(4,RF),8(RF)       IS THERE A UNIVERSE VALUE?                   
         BZ    GTRAD22A             NO, RETURN VALUE OF ZERO                    
         ICM   R1,15,4(RF)         R1=DIVIDEND VALUE                            
         CLI   RDLDMOD1,C'I'        IF IT IS AN IMPRESSION,                     
         BNE   *+8                                                              
         M     R0,=F'1000'           MULTIPLY IT BY 1000                        
         MVC   DUB(4),8(RF)        DUB=DIVISOR VALUE                            
                                                                                
         SLDA  R0,1                ******* START OF DIVIDE LOGIC ******         
         D     R0,DUB                                                           
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                ******** END OF DIVIDE LOGIC *******         
                                                                                
GTRAD22A LA    RF,8(RF)            BUMP 2X FOR DIVIDEND & DIVISOR               
         B     GETRAD30                                                         
                                                                                
GETRAD24 DC    H'0'                                                             
*&&DO                                                                           
GETRAD30 STCM  R1,15,0(R3)                                                      
         LA    R3,4(R3)                                                         
*&&                                                                             
GETRAD30 STCM  R1,15,0(R8)                                                      
         LA    R8,4(R8)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,RDEMLSTQ(RE)                                                  
         B     GETRAD22                                                         
         DROP  RE                                                               
*                                                                               
GETRADX  B     GETCN10                                                          
         SPACE 2                                                                
* EXTRACT CANADIAN DEMOS *                                                      
         SPACE 1                                                                
GETCN    L     R1,ASVIREC          POINT TO I/O AREA                            
***                                                                             
* CANADIAN SPOT POSTING USES DOUBLE WORD ARITHMETIC WHERE                       
* ASVIREC POINT TO DOUBLE WORD ACCUMLATOR BUFFERS, DBAREC STILL HOLD            
* THE REAL DEMO RECORD LAST READ.                                               
* SO SET DBAQUART OFF THE REAL DEMO RECORD NOT ASVIREC WHICH                    
* HOLDS THE REAL ACCUMULATED DEMO RECORD FOR FULL WORD ARTHMETIC                
* MATH BUT HOLDS DOUBLE WORD BUFFERS FOR DOUBLE WORD ARTHMETIC.                 
*                                                                               
         TM    DBNPMFLG,DBNPMACH                                                
         BNO   *+8                                                              
         TM    DBVOPT,X'40'                                                     
         BNO   *+8                                                              
         L     R1,DBAREC                                                        
***                                                                             
         STCM  R1,15,DBAREC                                                     
         LA    R1,DRFRSTEL-DRKEY(R1)                                            
         STCM  R1,15,DBAQUART                                                   
         XC    WORK,WORK                                                        
         LA    R0,DBLOCK                                                        
         STCM  R0,15,WORK                                                       
         MVC   WORK+06(2),DBDIVSOR                                              
* IF NEW POSTING METHODOLOGY FOR CANADA USE THE FULL WORD                       
* DBDIVSR2 FOR ACHIEVED POSTING                                                 
* IN THEORY WE CAN ALWAYS YSE DBDIVSR2 INSTEAD OF DBDIVSOR                      
* BUT WE WANT TO LIMIT THE TESTING TO CANADIAN POSTING                          
         TM    DBNPMFLG,DBNPMACH                                                
         BNO   GETCN06                                                          
* ONLY IF GETTTP SETS DBDIVSR2 SHOULD WE USE IT                                 
* THIS IS TO PREVENT ISSUES WHERE DEGETTP GETS BACKED OUT WITHOUT               
* SETTING DBDIVSR2.                                                             
         OC    DBDIVSR2,DBDIVSR2                                                
         BZ    GETCN06                                                          
         MVC   WORK+04(4),DBDIVSR2                                              
*                                                                               
GETCN06  MVC   WORK+08(3),DBFILE                                                
         MVC   WORK+11(3),DBFILE                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOMTH-COMFACSD(RF)                                         
*                                                                               
         TM    DBVOPT,X'40'   DOUBLE WORD ARITHMETIC - BPOO                     
         BZ    *+12           FOR CANADIAN POSTING                              
         TM    DBNPMFLG,DBNPMACH                                                
         BO    GETCN06B                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'DIV',ASVIREC,ASVIREC,WORK                           
         B     GETCN06C                                                         
*                                                                               
* DOUBLE WORD DIVIDE FOR CANADIAN POSTING                                       
GETCN06B GOTO1 (RF),DMCB,=C'DDI',ASVIREC,DBAREC,WORK                            
GETCN06C DS    0C                                                               
*                                                                               
*                                                                               
                                                                                
*==========================================================                     
* NOW EXTRACT VALUES FROM RECORD JUST CREATED                                   
* AND CLEAR IMPRESSIONS IF FLAG IS SET                                          
*==========================================================                     
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,DTDEMSU                         
*                                                                               
         ICM   R8,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GETCN10                                                          
         USING SPLKXTD,R8                                                       
         TM    SPXTSVFL,X'80'      TEST TO SUPPRESS IMPS                        
         BZ    GETCN10                                                          
         DROP  R8                                                               
*                                                                               
         LA    R4,DEMOLIST                                                      
         LA    R5,DTDEMSU                                                       
*                                                                               
GETCN2   CLI   1(R4),C'R'          TEST RATING                                  
         BE    GETCN4                                                           
         CLI   1(R4),C'E'          OR EXTENDED RATING                           
         BE    GETCN4                                                           
         XC    0(4,R5),0(R5)       CLEAR THE IMPS                               
*                                                                               
GETCN4   LA    R4,3(R4)            NEXT DEMO                                    
         LA    R5,4(R5)            NEXT ACCUM                                   
         CLI   0(R4),X'FF'                                                      
         BNE   GETCN2                                                           
*                                                                               
GETCN10  L     RE,ASVIREC                                                       
         XC    0(9,RE),0(RE)       CLEAR SVI KEY                                
***      BAS   RE,GETADQTB                                                      
         BRAS  RE,GETADQTB                                                      
         L     RE,ADBDQD                                                        
         MVC   ADJQTAB,0(RE)       MOVE TABLE FOR LOOKUP                        
         BAS   RE,GETADJ           GET ADJUSTMENT FACTORS                       
*                                                                               
         LA    R1,DTDEMSA          GET DEMOS X ADJ FACTORS                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    R1,RADEMSA                                                       
         BAS   RE,GDXSVI                                                        
*                                                                               
BENTEST  MVC   DBAREC,SPLKAREC     RESTORE ORIGINAL ADDRESS                     
         B     EXIT                                                             
         EJECT                                                                  
****************************************                                        
* SUBROUTINE TO GET ADJUSTMENT FACTORS *                                        
****************************************                                        
         SPACE 1                                                                
GETADJ   NTR1                                                                   
         L     R8,ASVIREC                                                       
         USING SVIREC,R8                                                        
         XC    SVIKEY,SVIKEY       SET I/O AREA CONTENTS INVALID                
*                                                                               
         LA    R0,100              SET ALL SVI VALUES = 100                     
         ST    R0,SVIS                                                          
         MVC   SVIS+4(L'SVIS-4),SVIS                                            
         B     EXIT                AND EXIT                                     
****************SVI FACTORS ARE NOW FORCED TO 100                               
         CLI   DBSELMED,C'C'                                                    
         BE    EXIT                                                             
         CLI   SVISW,0             TEST FIRST TIME                              
         BNE   GETADJ50            NO                                           
         SPACE 1                                                                
* TEST SVI FACTORS APPLICABLE *                                                 
         SPACE 1                                                                
         CLI   SAVEHUT,X'FF'       TEST SUPPRESS SVIS                           
         BE    EXIT                                                             
         LLC   R0,SAVEHUT                                                       
         SRL   R0,4                                                             
         CLM   R0,1,SVACTBK+1      TEST HUT MONTH = BOOK MONTH                  
         BE    EXIT                YES - IGNORE                                 
         SPACE 1                                                                
* GET ADJ TABLE ADDRESS VIA DEMADDR *                                           
         SPACE 1                                                                
         MVI   SVISW,C'Y'          RESET FIRST TIME SWITCH                      
         XC    DUB,DUB                                                          
         MVC   DUB(5),=X'D8000000FF'                                            
         L     R4,DBCOMFCS                                                      
         L     RF,CDEMADDR-COMFACSD(R4)                                         
         GOTO1 (RF),P1,(X'FF',DUB),(R4)                                         
*                                                                               
         L     R4,DUB                                                           
**       LAM   R4,R4,ALET                                                       
         LAM   AR4,AR4,ALET                                                     
         SAC   512                                                              
         USING ADJTABD,R4                                                       
*                                                                               
         MVC   DUB(1),DBACTSRC                                                  
         MVC   DUB+1(1),DBSELMED                                                
         XR    R0,R0                                                            
*                                                                               
GETADJ2  CLC   DUB(2),ADJSRC       MATCH SOURCE/MEDIA                           
         BNE   GETADJ4                                                          
         CLC   ADJAGY,=X'FFFF'     TEST DEFAULT AGY                             
         BE    *+14                                                             
         CLC   DBSELAGY,ADJAGY     MATCH AGY                                    
         BNE   GETADJ4                                                          
         CLI   ADJCODE,X'FF'                                                    
         BNE   GETADJ4                                                          
* NON-DEFAULT CODES NOT IMPLEMENTED *                                           
         CLC   ADJCLI,=X'FFFFFF'   TEST DEFAULT CLIENT                          
         BE    *+14                                                             
         CLC   DBSELCLI,ADJCLI                                                  
         BNE   GETADJ4                                                          
         MVC   DUB+2(2),ADJSBOOK                                                
         XC    DUB+2(2),=X'FFFF'   COMPLEMENT BOOK                              
         CLC   SVACTBK,DUB+2       TEST ACTUAL BOOK TO START BOOK               
         BNL   GETADJ6                                                          
         SPACE 1                                                                
GETADJ4  ICM   R4,7,11(R4)         POINT TO END                                 
         LA    R4,1(R4)            THEN TO END                                  
         CLI   0(R4),0             TEST EOT                                     
         BNE   GETADJ2             NO - CONTINUE                                
         SAC   0                                                                
***      LAM   R4,R4,=F'0'                                                      
         LAM   AR4,AR4,=F'0'                                                    
         B     EXIT                ELSE SVI'S NOT ACTIVE                        
*                                                                               
GETADJ6  MVC   SVADJHUT,ADJHUT     SAVE SOURCE CODE                             
*                                                                               
         LA    R4,14(R4)           SET A(MODIFIER LIST)                         
         ST    R4,ASVIMOD                                                       
*                                                                               
         LA    R4,2(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   *-8                                                              
         LA    R4,1(R4)            POINT TO DEMO TYPE LIST                      
         ST    R4,ASVIDEM          SAVE A(DEMO TYPE LIST)                       
         SAC   0                                                                
***      LAM   R4,R4,=F'0'                                                      
         LAM   AR4,AR4,=F'0'                                                    
         DROP R4                                                                
         SPACE 1                                                                
* DETERMINE SVI TYPE CODES FOR EACH DEMO IN DEMOLIST *                          
         SPACE 1                                                                
         XC    SVITYPES,SVITYPES                                                
         LA    R4,SVITYPES                                                      
         LA    R1,DEMOLIST                                                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    R1,DEMOLST2                                                      
*                                                                               
GETADJ10 L     R5,ASVIMOD                                                       
**       LAM   R5,R5,ALET                                                       
         LAM   AR5,AR5,ALET                                                     
         SAC   512                                                              
*                                                                               
GETADJ12 CLC   1(1,R1),0(R5)       MATCH MODIFIER LIST                          
         BE    GETADJ14                                                         
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   GETADJ12                                                         
         B     GETADJ16            UNKNOWN MOD TYPE                             
*                                                                               
GETADJ14 CLI   1(R5),0             TEST ADJUST THIS MOD TYPE                    
         BE    GETADJ16            NO                                           
         EJECT                                                                  
* THIS TYPE TO BE ADJUSTED *                                                    
         SPACE 1                                                                
         XR    R5,R5                                                            
         IC    R5,2(R1)            GET DEMO NUMBER                              
         BCTR  R5,0                                                             
         A     R5,ASVIDEM                                                       
         MVC   0(1,R4),0(R5)       SAVE SVI TYPE                                
*                                                                               
GETADJ16 LA    R4,1(R4)                                                         
         LA    R1,3(R1)            NEXT DEMO                                    
         SAC   0                                                                
***      LAM   R5,R5,=F'0'                                                      
         LAM   AR5,AR5,=F'0'                                                    
         CLI   0(R1),X'FF'                                                      
         BNE   GETADJ10                                                         
*                             BUILD TABLE OF MONTHLY WEIGHTS *                  
GETADJ20 CLI   SAVEHUT,0           TEST AUTO ADJUST                             
         BE    GETADJ26                                                         
*                                                                               
         LLC   RE,SAVEHUT                                                       
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         LTR   RE,RE               TEST START MONTH PRESENT                     
         BNZ   *+6                                                              
         LR    RE,RF               NO - SET START = END                         
         LTR   RF,RF               TEST END MONTH PRESENT                       
         BNZ   *+6                                                              
         LR    RF,RE               IF NO, SET END = START                       
         CR    RE,RF               START TO END                                 
         BH    GETADJ24                                                         
*                                                                               
GETADJ22 LR    R4,RE                                                            
         AR    R4,R4               X 2                                          
         LA    R4,SVIWGTS-2(R4)                                                 
         MVI   1(R4),1             SET WEIGHT OF 1 IN ACTIVE MONTH              
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BNH   GETADJ22                                                         
         B     GETADJ40                                                         
         SPACE 1                                                                
* START MONTH LESS THAN END MONTH    *                                          
* INVERT TABLE AND COMPLEMENT AT END *                                          
         SPACE 1                                                                
GETADJ24 LR    R4,RF                                                            
         AR    R4,R4               X 2                                          
         LA    R4,SVIWGTS-2(R4)                                                 
         MVI   1(R4),1                                                          
         LA    RF,1(RF)                                                         
         CR    RF,RE                                                            
         BNH   GETADJ24                                                         
         XC    SVIWGTS,=12H'1'                                                  
         B     GETADJ40                                                         
         EJECT                                                                  
* AUTO ADJUST - WEIGHT MONTHS BY NUMBER OF OCCURENCES   *                       
*  IN BUY DESCRIPTION PERIOD                            *                       
         SPACE 1                                                                
GETADJ26 CLI   AFDSW,C'Y'          TEST AFFID LOOKUP                            
         BNE   GETADJ27            NO                                           
* FORCE SVI MONTH TO MONTH OF CURRENT AFFID                                     
         L     R7,DBCOMFCS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CDATCON,P1,(2,AFDATE),(3,WORK)                                   
         LLC   R4,WORK+1                                                        
         AR    R4,R4               X 2                                          
         LA    R4,SVIWGTS-2(R4)                                                 
         MVI   1(R4),1             SET WEIGHT OF 1 IN ACTIVE MONTH              
         B     GETADJ40                                                         
*                                                                               
GETADJ27 LA    R0,7                SET INTERVAL BETWEEN WEEKLY SPOTS            
* FORCE SVI MONTH TO MONTH OF CURRENT AFFID                                     
         CLI   SPLKAUTF,C'O'                                                    
         BE    GETADJ28                                                         
         LA    R0,14                                                            
         CLI   SPLKAUTF,C'A'                                                    
         BE    GETADJ28                                                         
         LA    R0,21                                                            
         CLI   SPLKAUTF,C'T'                                                    
         BE    GETADJ28                                                         
         LA    R0,28                                                            
         CLI   SPLKAUTF,C'F'                                                    
         BE    GETADJ28                                                         
         LA    R0,7                DEFAULT TO 7 DAY INTERVAL                    
*                                                                               
GETADJ28 L     R7,DBCOMFCS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CDATCON,P1,(2,SPLKAUST),WORK                                     
         GOTO1 CDATCON,P1,(2,SPLKAUND),WORK+6                                   
         SPACE 1                                                                
* DETERMINE START/END DATE DAYS THEN ADVANCE TO FOLLOWING SUNDAYS *             
         SPACE 1                                                                
         GOTO1 CGETDAY,P1,WORK,DUB                                              
         CLI   0(R1),7             TEST SUNDAY                                  
         BE    GETADJ30                                                         
         LLC   R4,0(R1)                                                         
         LCR   R4,R4                                                            
         A     R4,=F'7'                                                         
         GOTO1 CADDAY,P1,WORK,WORK,(R4)                                         
*                                                                               
GETADJ30 DS    0H                                                               
         GOTO1 CGETDAY,P1,WORK+6,DUB                                            
         CLI   0(R1),7             TEST SUNDAY                                  
         BE    GETADJ32                                                         
         LLC   R4,0(R1)                                                         
         LCR   R4,R4                                                            
         A     R4,=F'7'                                                         
         GOTO1 CADDAY,P1,WORK+6,WORK+6,(R4)                                     
*                                                                               
GETADJ32 PACK  DUB,WORK+2(2)       PACK MONTH                                   
         CVB   RE,DUB                                                           
         AR    RE,RE               X 2                                          
         LH    RF,SVIWGTS-2(RE)                                                 
         LA    RF,1(RF)                                                         
         STH   RF,SVIWGTS-2(RE)                                                 
         GOTO1 CADDAY,P1,WORK,WORK,(R0) INCREMENT WEEK OF DATE                  
         CLC   WORK(6),WORK+6           TEST IN BUY DESC PERIOD                 
         BNH   GETADJ32                 YES - BUMP WEIGHT                       
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
* CALCULATE SUM OF WEIGHTS *                                                    
         SPACE 1                                                                
GETADJ40 LA    R4,SVIWGTS                                                       
         LA    R0,12                                                            
         SR    RE,RE                                                            
*                                                                               
         AH    RE,0(R4)                                                         
         LA    R4,2(R4)                                                         
         BCT   R0,*-8                                                           
         ST    RE,SVIWGTSM                                                      
         SPACE 1                                                                
GETADJ50 OC    SVIWGTS,SVIWGTS     TEST SVIS ACTIVE                             
         BZ    EXIT                NO                                           
         OC    SVITYPES,SVITYPES   TEST ANY DEMOS TO ADJUST                     
         BZ    EXIT                NO                                           
*                                                                               
         MVI   ADJQPTR,0           RESET DAY/QH LIST POINTER                    
         XC    SVIS,SVIS           CLEAR SVI VALUES                             
         XC    SVIWGT,SVIWGT       CLEAR SVI QH COUNT                           
*                                                                               
GETADJ51 LLC   RE,ADJQPTR          GET LIST ENTRY NUMBER                        
         LA    RF,DBDQLEN          GET LIST ENTRY LENGTH                        
         MR    RE,RE                                                            
         LA    R4,ADJQTAB(RF)      POINT TO ENTRY                               
QD       USING DBDQD,R4                                                         
         CLI   0(R4),X'FF'         TEST E-O-L                                   
         BE    GETADJ80                                                         
         SPACE 1                                                                
* CONVERT DEMFILE DAY CODE (10,20,30,40,50,60,70,95) *                          
*      TO SVIFILE DAY CODE (15,66,77)                *                          
         SPACE 1                                                                
         LA    R1,DAYCODES                                                      
         LA    R0,8                                                             
*                                                                               
GETADJ52 CLC   QD.DBDQKDAY,0(R1)   DAY TO TABLE                                 
         BE    GETADJ54                                                         
         LA    R1,2(R1)                                                         
         BCT   R0,GETADJ52                                                      
         DC    H'0'                                                             
*                                                                               
DAYCODES DC    X'10152015301540155015606670779515'                              
*                                                                               
GETADJ54 MVC   SVIDAY,1(R1)          SAVE SVI DAY CODE                          
         MVC   SVISQH(2),QD.DBDQSQH  SAVE CURRENT ENTRY ST/END QHS              
*                                                                               
         CLC   SVIDAY,SVIKDAY        TEST CURRENT SVIREC RIGHT DAY              
         BNE   GETADJ58                                                         
*                                                                               
GETADJ56 CLC   SVISQH,SVIKEQH      TEST START QH AFTER SVIREC END               
         BH    GETADJ58                                                         
         CLC   SVISQH,SVIKSQH      OR BEFORE SVIREC START                       
         BL    GETADJ58                                                         
         B     GETADJ60                                                         
*                                                                               
GETADJ58 CLI   OVPHOPT,C'Y'        OTP REQUEST                                  
         BE    *+8                                                              
         CLI   WVPHOPT,C'Y'        WTP REQUEST                                  
         BE    *+12                  KILL SVI FACTORS                           
         BRAS  RE,RDSVI            READ SVI RECORD                              
         BE    GETADJ60            FOUND - GO GET SVI'S                         
*                                                                               
         LA    R0,100              ON ERROR SET QH VALUES TO 100                
         ST    R0,WORK                                                          
         MVC   WORK+4(76),WORK     SET SVI'S TO 100                             
         B     GETADJ61            AND CONTINUE                                 
         SPACE 1                                                                
* EXTRACT SVI VALUES FOR THIS QH AND ADD TO TOTALS *                            
         SPACE 1                                                                
GETADJ60 BAS   RE,GETSVI                                                        
*                                                                               
GETADJ61 L     R0,NUMDEMS                                                       
         SR    R1,R1                                                            
*                                                                               
GETADJ62 L     RF,WORK(R1)                                                      
         LLC   RE,QD.DBDQDAYW      GET WEIGHTING FACTOR                         
         MR    RE,RE                                                            
         A     RF,SVIS(R1)                                                      
         ST    RF,SVIS(R1)                                                      
         LA    R1,4(R1)                                                         
         BCT   R0,GETADJ62                                                      
*                                                                               
         LLC   RE,QD.DBDQDAYW      GET CURRENT WEIGHTING FACTOR                 
         L     R0,SVIWGT           GET TOTAL SO FAR                             
         AR    R0,RE                                                            
         ST    R0,SVIWGT                                                        
         SPACE 1                                                                
* TEST PROCESSED ALL QUARTER HOURS *                                            
         SPACE 1                                                                
         CLC   SVISQH,SVIEQH                                                    
         BNL   GETADJ64            YES                                          
         IC    R1,SVISQH                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SVISQH                                                        
         B     GETADJ56                                                         
         SPACE 1                                                                
* ALL QUARTER HOURS PROCESSED - TRY NEXT DAY/QH TABLE ENTRY *                   
         SPACE 1                                                                
GETADJ64 DS    0H                                                               
         LLC   RE,ADJQPTR                                                       
         LA    RE,1(RE)                                                         
         STC   RE,ADJQPTR                                                       
         B     GETADJ51                                                         
         DROP  QD                                                               
         EJECT                                                                  
* ALL DAY/QH ENTRIES PROCESSED - UNWEIGHT VALUES *                              
         SPACE 1                                                                
GETADJ80 DS    0H                                                               
         CLC   SVIWGT,=F'1'                                                     
         BE    EXIT                                                             
*                                                                               
         L     R0,NUMDEMS                                                       
         LA    R1,SVIS                                                          
*                                                                               
GETADJ82 SR    RE,RE                                                            
         L     RF,0(R1)                                                         
         SLDL  RE,1                                                             
         D     RE,SVIWGT                                                        
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,GETADJ82                                                      
         B     EXIT                                                             
         SPACE 1                                                                
* MISSING SVI DATA FOR AT LEAST ONE QH - SET ALL SVIS TO 100 *                  
         SPACE 1                                                                
GETADJ90 LA    R0,100                                                           
         ST    R0,SVIS                                                          
         MVC   SVIS+4(76),SVIS                                                  
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE TO EXTRACT VALUES FROM SVI RECORD                  *               
* MONTH WEIGHTED FACTORS FOR EACH DEMO ARE RETURNED IN WORK     *               
*****************************************************************               
         SPACE 2                                                                
GETSVI   NTR1                                                                   
*                                                                               
         LA    R0,100                                                           
         ST    R0,WORK                                                          
         MVC   WORK+4(76),WORK     FORCE ALL VALUES = 100                       
*                                                                               
         LA    R1,DEMOLIST         R1 = CURRENT DEMOLIST ENTRY                  
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    R1,DEMOLST2                                                      
         LA    R4,SVITYPES         R4 = CORRESPONDING SVI TYPE ENTRY            
         LA    R2,WORK                                                          
*                                                                               
GETSVI2  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    GETSVI10                                                         
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,SVISDEL                                                       
         BAS   RE,NEXTEL2                                                       
         B     *+8                                                              
*                                                                               
GETSVI3  BAS   RE,NEXTEL                                                        
         BNE   GETSVI3X                                                         
         CLC   0(1,R4),2(R6)       MATCH SVI TYPE                               
         BE    GETSVI4                                                          
         B     GETSVI3                                                          
         SPACE 1                                                                
* TYPE NOT FOUND - POINT TO DUMMY SVI ELEM *                                    
         SPACE 1                                                                
GETSVI3X LA    R6,=12AL1(100)                                                   
         SHI   R6,3                                                             
         B     GETSVI5                                                          
         SPACE 1                                                                
* SET BAD SVI FACTORS TO 100 *                                                  
         SPACE 1                                                                
GETSVI4  LA    R0,12                                                            
         LA    RE,3(R6)                                                         
GETSVI4A CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),100                                                        
         LA    RE,1(RE)                                                         
         BCT   R0,GETSVI4A                                                      
         EJECT                                                                  
GETSVI5  LLC   RE,SVACTBK+1        GET 'FROM' MONTH                             
         BCTR  RE,0                                                             
         IC    RE,3(RE,R6)         GET FACTOR                                   
         LTR   RE,RE               DON'T DIE ON DIVIDE                          
         BNZ   *+8                                                              
         LA    RE,100                                                           
         ST    RE,DUB              SAVE IN DUB                                  
*                                                                               
         LA    R0,12                                                            
         LA    R6,3(R6)            POINT TO MONTHLY SVIS                        
         LA    R5,SVIWGTS          POINT TO MONTHLY WEIGHTS                     
         SR    R7,R7               CLEAR TOTAL                                  
*                                                                               
GETSVI6  LLC   RF,0(R6)            GET 'TO' MONTH FACTOR                        
         LA    RE,200              X 100 X 2                                    
         MR    RE,RE                                                            
         D     RE,DUB              DIVIDE BY 'FROM' MONTH FACTOR                
         LA    RF,1(RF)                                                         
         SRL   RF,1                ROUND                                        
         SPACE 1                                                                
* TEST FOR BAD SVI VALUES *                                                     
         SPACE 1                                                                
         CHI   RF,25               LESS THAN 26 IS BAD                          
         BH    *+8                                                              
         LA    RF,100                                                           
         CHI   RF,200              MORE THAN 199 IS BAD                         
         BL    *+8                                                              
         LA    RF,100                                                           
*                                                                               
         MH    RF,0(R5)            X MONTHLY WEIGHT                             
         AR    R7,RF               ADD TO TOTAL                                 
         LA    R6,1(R6)                                                         
         LA    R5,2(R5)                                                         
         BCT   R0,GETSVI6                                                       
         SPACE 1                                                                
         SR    R6,R6                                                            
         AR    R7,R7               X 2                                          
         D     R6,SVIWGTSM                                                      
         LA    R7,1(R7)                                                         
         SRL   R7,1                                                             
         ST    R7,0(R2)                                                         
*                                                                               
GETSVI10 LA    R2,4(R2)                                                         
         LA    R4,1(R4)                                                         
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GETSVI2                                                          
         B     EXIT                                                             
*                                                                               
         DROP  R8                  UP TO HERE USED FOR SVIREC                   
         EJECT                                                                  
*&&DO                                                                           
*****************************************************************               
* SUBROUTINE TO CALCULATE ACTUAL SVI VALUES FOR ACCUMS AT 0(R1) *               
*             VALUES ARE RETURNED IN SVIS                       *               
*****************************************************************               
         SPACE 2                                                                
GDSVI    NTR1                                                                   
         L     R4,NUMDEMS                                                       
         LA    R5,SVIS                                                          
         XC    SVIS,SVIS                                                        
*                                                                               
GDSVI2   ICM   RE,15,MAXDEMS*4(R1)                                              
         BNZ   *+10                                                             
         SR    RF,RF                                                            
         B     GDSVI4                                                           
         SR    RE,RE                                                            
         L     RF,0(R1)                                                         
* COMSCORE NULL VALUES ARE PASSED FOR DEMOS WHICH HAS NO VALUE                  
* FROM COMSCORE API CAL                                                         
         CLC   0(4,R1),=X'FFFFFFFF'  COMSCORE X'FFFFFFFF' SAME                  
         BNE   GDSVI3                AS NO VALUE                                
         SR    RF,RF                                                            
         B     GDSVI4                                                           
***      CLC   0(4,R1),=X'FFFFFFFF'                                             
***      BE    GDSVI4                                                           
*                                                                               
GDSVI3   SLDL  RE,1                X 2                                          
         D     RE,MAXDEMS*4(R1)                                                 
         AHI   RF,1                                                             
         SRL   RF,1                                                             
GDSVI4   ST    RF,0(R5)                                                         
         LA    R1,4(R1)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,GDSVI2                                                        
         B     EXIT                                                             
         SPACE 2                                                                
*&&                                                                             
*************************************************                               
* NOTE- ROUTINE SETVALS USED TO BE HERE         *                               
* MOVED TO ITS OWN BASE REGISTER TO CREATE ROOM *                               
* BPOO  10/26/2004                              *                               
*************************************************                               
*****************************************************************               
*  COMMENT OUT- GDWUNWGT NEVER REFERENCED                                       
*           SUBROUTINE TO UNWEIGHT ACCUMS AT 0(R1)              *               
*                       BY VALUE IN R0                          *               
*****************************************************************               
         SPACE 2                                                                
*&&DO                                                                           
GDUNWGT  NTR1                                                                   
         LA    R4,MAXDEMS*2        ADJ/UNADJ ACCUMS                             
*                                                                               
GDUNWGT2 L     RF,0(R1)                                                         
         M     RE,=F'2'            X 2                                          
         LTR   R0,R0                                                            
         BNZ   *+10                                                             
         SR    RF,RF                                                            
         B     *+6                                                              
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R4,GDUNWGT2                                                      
         B     EXIT                                                             
         SPACE 2                                                                
*&&                                                                             
*****************************************************************               
* SUBROUTINE TO CALCULATE ADJUSTED DEMO VALUES AS EQUAL TO      *               
*  UNADJUSTED VALUES X SVI'S                                    *               
* R1 POINTS TO 80 BYTE ADJ DEMOS FOLLOWED BY 80 BYTES UNADJ     *               
*****************************************************************               
         SPACE 2                                                                
GDXSVI   NTR1                                                                   
         L     R4,NUMDEMS                                                       
         LA    R5,SVIS                                                          
*&&DO                                                                           
GDXSVI2  CLC   MAXDEMS*4(4,R1),=X'FFFFFFFF'                                     
         BE    GDXSVI3                                                          
         L     RF,MAXDEMS*4(R1)    GET UNADJ VALUE                              
         MH    RF,2(R5)            X SVI                                        
*&&                                                                             
*                                                                               
GDXSVI2  L     RF,MAXDEMS*4(R1)    GET UNADJ VALUE                              
*                                                                               
         CLC   MAXDEMS*4(4,R1),=X'FFFFFFFF'                                     
         BE    GDXSVI2N            KEEP X'FFFFFFFF' COMSCORE ERROR VAL          
*                                                                               
         MH    RF,2(R5)            X SVI                                        
                                                                                
                                                                                
GDXSVI2N ST    RF,0(R1)            STORE ADJ VALUE                              
*                                                                               
GDXSVI3  LA    R1,4(R1)            NEXT DEMO                                    
         LA    R5,4(R5)            NEXT SVI                                     
         BCT   R4,GDXSVI2                                                       
         B     EXIT                                                             
         EJECT                                                                  
*&&DO                                                                           
*****************************************************************               
*                                                               *               
*           SUBROUTINE TO COMPUTE WEIGHTED DEMO                 *               
*                                                               *               
*****************************************************************               
         SPACE 2                                                                
GETWD    NTR1                                                                   
*                                                                               
         LA    R5,DEMOLIST                                                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    R5,DEMOLST2                                                      
                                                                                
         L     R6,SPLKAVAL                                                      
         LA    R8,SAVEWGTS                                                      
         XC    WORK,WORK                                                        
         XC    WTDEMA(8),WTDEMA                                                 
*                                                                               
GETWD2   CLI   1(R6),63            TEST WEIGHTED DEMO                           
         BNE   GETWD4                                                           
         ST    R6,WORK             ELSE SAVE DATA ADDRESS                       
         MVC   WORK+7(1),0(R8)     AND WEIGHT (IF ANY)                          
         B     GETWD6                                                           
*                                                                               
GETWD4   SR    R0,R0                                                            
         ICM   R0,1,0(R8)          GET WEIGHT VALUE                             
         BZ    GETWD6                                                           
*                                                                               
         L     RF,0(R6)            UNADJ DEMO VALUE                             
         MR    RE,R0                                                            
         A     RF,WTDEMU                                                        
         ST    RF,WTDEMU                                                        
*                                                                               
         L     RF,0(R6)            UNADJ DEMO VALUE                             
         L     RE,4(R6)            SVI FACTOR                                   
         MR    RE,RE               GIVES ADJ DEM                                
         MR    RE,R0               X WEIGHT                                     
         A     RF,WTDEMA                                                        
         ST    RF,WTDEMA                                                        
*                                                                               
GETWD6   LA    R5,3(R5)            NEXT DEMO IN LIST                            
         LA    R6,8(R6)            NEXT DEMO VALUE                              
         LA    R8,1(R8)            NEXT WEIGHT                                  
         CLI   1(R5),0             TEST E-O-L                                   
         BNE   GETWD2                                                           
         EJECT                                                                  
* DIVIDE ADJ BY UNADJ VALUE TO GET SVI *                                        
         SPACE 1                                                                
GETWD7   ICM   R6,15,WORK          POINT TO WEIGHTED DEMO SLOT                  
         BZ    EXIT                                                             
         SR    RE,RE                                                            
         L     RF,WTDEMA           GET ADJ VALUE                                
         SLDL  RE,1                X 2                                          
         ICM   R0,15,WTDEMU                                                     
         BZ    *+6                                                              
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,4(R6)            SET SVI VALUE                                
*                                                                               
         MVC   4(4,R6),WTDEMU      WEIGHTED DEMO VALUE                          
         ICM   RF,15,4(R6)                                                      
         OC    WORK+4(4),WORK+4    TEST WEIGHT FOR WTD DEMO                     
         BZ    GETWD8                                                           
         SR    RE,RE                                                            
         L     RF,WTDEMU           <- PER MEL                                   
         SLDL  RE,1                X 2                                          
         D     RE,WORK+4           DIVIDE BY WEIGHT                             
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,0(R6)                                                         
         SPACE 1                                                                
GETWD8   CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         BNE   EXIT                                                             
         SPACE 1                                                                
* CALC DEMO VALUE X SPOTS *                                                     
         SPACE 1                                                                
         LH    RE,NOAFDCNT                                                      
         AH    RE,AFDCNT                                                        
         MR    RE,RE                                                            
         EJECT                                                                  
GETWD14  L     R1,SPLKAVAL                                                      
         LA    R6,DEMOLIST                                                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    R6,DEMOLST2                                                      
*                                                                               
GETWD16  CLI   1(R6),63            TEST WEIGHTED DEMO                           
         BE    GETWD18                                                          
         LA    R1,8(R1)            NEXT SLOT IN OUTPUT AREA                     
         LA    R6,3(R6)            NEXT DEMO IN LIST                            
         CLI   0(R1),0                                                          
         BNE   GETWD16                                                          
         DC    H'0'                                                             
*                                                                               
GETWD18  ST    RF,0(R1)            SET DEMO VALUE                               
         LA    R0,100                                                           
         ST    R0,4(R1)            AND SVI                                      
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*&&                                                                             
         EJECT                                                                  
GETVHOME NTR1                                                                   
*                                                                               
         BRAS  RE,GETVHNTR                                                      
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
*===========================================================*                   
* SUBROUTINE SETS VALUES FOR SPECIAL 7 MINUTE AFFID OPTION  *                   
* IF PROGRAM SPANS 2 QH'S AND RUNS LESS THAN 8 MINUTES IN   *                   
* FINAL QH, SET END TIME TO 3 MINUTES PRIOR TO LAST QH      *                   
* E.G. IF PROG RUNS 11-1137, AFFIDS FROM 1128 TO 1139 WILL  *                   
* BE CHANGED TO 1127.                                       *                   
*===========================================================*                   
         SPACE 1                                                                
SET7     NTR1  ,                                                                
         TM    AFDOPT,AFDO7SET     TEST ALREADY BEEN THROUGH THIS CODE          
         BO    SET7X               YES-EXIT                                     
         OI    AFDOPT,AFDO7SET                                                  
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
         XC    AF7QHSET,AF7QHSET                                                
         SR    R0,R0               TEST PROGRAM RUNS LESS THAN                  
         SR    R1,R1               8 MINUTES INTO LAST QH                       
         ICM   R1,3,BDTIMEND                                                    
         D     R0,=F'100'          HOURS IN R1/ MINS IN R0                      
         SRDL  R0,32                                                            
         D     R0,=F'15'           MINUTES INTO LAST QH IN R0                   
         LTR   R0,R0               TEST ENDS ON QH BOUNDARY                     
         BZ    SET7X                                                            
         CHI   R0,7                ELSE TEST 1-7 MINUTES INTO QH                
         BH    SET7X                                                            
*                                                                               
         SR    R1,R1               TEST PROGRAM RUNS AT LEAST 2 QH'S            
         ICM   R1,3,BDTIMST                                                     
         BAS   RE,SET7QH                                                        
         LR    RF,R1               SAVE START QH NUMBER                         
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMEND                                                    
         BAS   RE,SET7QH                                                        
         CR    R1,RF               TEST SAME START/END QH                       
         BE    SET7X               YES                                          
*                                                                               
         D     R0,=F'4'            HOURS/100 IN R1, MINS/15 IN R0               
         MHI   R0,15               MINS IN R0                                   
         SHI   R0,2                BACK UP 2 MINUTES                            
         BP    *+10                                                             
         BCTR  R1,0                BACK UP 1 HOUR                               
         LA    R0,58               AND SET MINUTES                              
         MHI   R1,100              HOUR X 100                                   
         AR    R1,R0                                                            
         STH   R1,AF7QHST          SET START OF LAST QH IN HHMM                 
         BCTR  R1,0                BACK UP 1 MORE MINUTE                        
         STH   R1,AF7QHSET         AND SET 'SET TIME' VALUE                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMEND       GET END TIME                                 
         LA    R1,0(R1)            ADD 0 MINUTES - NO BREAK                     
         STH   R1,AF7QHEND         SET END TIME OF LAST QH IN  HHMM             
*                                                                               
SET7X    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*==========================================================*                    
* CONVERT MILITARY TIME IN R1 TO QH NUMBER                 *                    
*==========================================================*                    
         SPACE 1                                                                
SET7QH   SR    R0,R0                                                            
         D     R0,=F'100'          HOUR IN R1/MIN IN R0                         
         SLL   R1,2                HOUR X 4                                     
         ST    R1,DUB                                                           
         SRDL  R0,32                                                            
         D     R0,=F'15'                                                        
         A     R1,DUB                                                           
         SR    R0,R0               EXIT WITH R0=0 ALWAYS                        
         BR    RE                                                               
         EJECT                                                                  
*&&DO                                                                           
*------------------------ GET A(DAY/QH TABLE) ------------------------*         
                                                                                
* AT ENTRY,                                                                     
*   DBLOCK   IS SET                                                             
* AT EXIT,                                                                      
*   ADBDQD   = A(INTERNAL DAY/QHR TABLE)                                        
                                                                                
GETADQTB NTR1                                                                   
*        LA    R3,DBDQD             PICK UP ADDR DIRECTLY FROM HERE             
         LA    R8,DBDQD             PICK UP ADDR DIRECTLY FROM HERE             
         CLC   =AL2(DBDQUXTD),DBDQTAB+0  DO WE NEED TO LOOK IN XTND?            
         BNE   GADQT12X                   NOPE                                  
*                                                                               
         DS    0H                                                               
         LA    RF,DBEXTEND-4                                                    
GADQT12G DS    0H                   LOOK THROUGH DBEXTEND                       
         ICM   RF,15,4(RF)                                                      
         BZ    GADQT12X                                                         
         CLC   0(4,RF),DBDQTAB+2     FOR NAME GIVEN HERE                        
         BNE   GADQT12G                                                         
*        LA    R3,DBDQXFXL(RF)                                                  
         LA    R8,DBDQXFXL(RF)                                                  
GADQT12X EQU   *                                                                
*                                                                               
         DS    0H                                                               
*        ST    R3,ADBDQD                                                        
         ST    R8,ADBDQD                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
*=========================== SET DBDQD LINK ==========================*         
*&&DO                                                                           
* JUL28/00 LVL=032 - CHANGED ALL BRANCH INSTRUCTIONS TO JUMP.  THIS             
*   ROUTINE IS CALLED FROM OTHER PLACES WHERE THE 1ST BASE REGISTER             
*   HAS BEEN CHANGED                                                            
                                                                                
SETDQLNK NTR1                                                                   
         CLI   DBSELMED,C'T'       FOR MEDIA=USTV                               
         JNE   SDQLX                                                            
         CLI   DBSELSRC,C'N'       FOR SOURCE=NSI                               
         JNE   SDQLX                                                            
         CLC   DBFILE,=C'TP '      FOR TP FILE                                  
         JNE   SDQLX                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         LA    RF,DBEXTEND-4                                                    
*                                                                               
SDQL010  DS    0H                                                               
         OC    4(4,RF),4(RF)                                                    
         JZ    SDQL020                                                          
*                                                                               
SDQL015  DS    0H                                                               
         ICM   RF,15,4(RF)                                                      
         CLC   0(3,RF),=C'DBD2'    (LENGTH OF 3 IS INTENTIONAL!!)               
         JE    SDQL040             FOUND CALLER'S D/QH LINK AREA                
         J     SDQL010                                                          
                                                                                
*                                                                               
SDQL020  DS    0H                                                               
         LA    R1,DBXTDQT                                                       
         STCM  R1,15,4(RF)                                                      
         XC    0(DBXTDQTL,R1),0(R1)                                             
         MVC   0(4,R1),=C'DBD2'                                                 
         LR    RF,R1                                                            
         J     SDQL040                                                          
                                                                                
*                                                                               
SDQL040  DS    0H                                                               
         MVC   DBDQTAB+0(2),=AL2(DBDQUXTD)                                      
         MVC   DBDQTAB+2(4),0(RF)                                               
                                                                                
*                                                                               
SDQLX    DS    0H                                                               
         J     EXIT                                                             
*&&                                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= RESTORE DBDQD LINK ========================*         
                                                                                
* ROUTINE RESTORES AS MANY DAYS/QHS IN THE EXTENDED AREA TO THE                 
*  ORIGINAL LOCATION IN THE DBLOCK (DBDQD) AS POSSIBLE.                         
* THE RESTORING IS ONLY APPLICABLE IF AND ONLY IF  GETTP  CREATED THE           
*  DAYS/QHS LINK.                                                               
                                                                                
RSTDQLNK NTR1                                                                   
*&&DO                                                                           
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F')                      
         IF (CLI,COMPASS,E,X'01')                                               
         IF (TM,SPLKOPT2,SPLKOPT2_COMS,O) HAVE COMSCORE DEMOS                   
         J     EXIT       IF PASS ONE AND NO COMSCORE DEMOS JUST EXIT           
         ENDIF                                                                  
         ENDIF                                                                  
         ENDIF                                                                  
*&&                                                                             
         IF (CLI,DBSELSRC,E,C'N'),OR,(CLI,DBSELSRC,E,C'F'),ANDIF,               
            (CLI,COMPASS,E,X'01'),ANDIF,                                        
            (TM,SPLKOPT2,SPLKOPT2_COMS,O) HAVE COMSCORE DEMOS                   
         J     EXIT       IF PASS ONE AND NO COMSCORE DEMOS JUST EXIT           
         ENDIF                                                                  
         LA    RF,DBEXTEND-4                                                    
         SR    RE,RE                                                            
*                                                                               
RDQL010  DS    0H                  FIND DAYS/QHS ENTRIES LINK                   
         LR    RE,RF                KEEP PREVIOUS LINK ADDRESSABLE              
         ICM   RF,15,4(RF)          BUMP TO NEXT LINK                           
         BZ    RDQLX                NO MORE LINKS -- NOTHING TO RESTORE         
*                                                                               
         DS    0H                                                               
         CLC   =C'DBD2',0(RF)       LOOK FOR A SPGETDEMF-CREATED LINK           
         BNE   RDQL010              NOT THE DAYS/QHS ENTRIES WE WANT            
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   DBDQTAB,DBDQXFXL(RF)  RESTORE DBDQTAB                            
*                                                                               
         DS    0H                                                               
         MVC   4(4,RE),4(RF)         REMOVE LINK FROM DBEXTEND                  
*                                                                               
         DS    0H                                                               
         LA    RE,DBDQTAB                                                       
         LA    RF,(L'DBDQTAB-1)(RE)                                             
                                                                                
RDQL046  DS    0H                                                               
         CLI   0(RF),0                                                          
         BNE   RDQL046G                                                         
         SHI   RF,DBDQLEN                                                       
         CR    RE,RF                                                            
         BL    RDQL046                                                          
RDQL046G EQU   *                                                                
         MVI   0(RF),X'FF'                    RESTORE DELIMITER TOO!            
                                                                                
*                                                                               
RDQLX    DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
VUTLIST  DC    X'81',C'V',AL1(1)   RATING TIMES SHARE                           
         DC    X'81',C'V',AL1(2)                                                
         DC    X'81',C'V',AL1(3)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
PUTLIST  DC    X'81',C'P',AL1(1)   STRAIGHT PUT                                 
         DC    X'81',C'P',AL1(2)                                                
         DC    X'81',C'P',AL1(3)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
UNILIST  DC    X'43',C'U',AL1(1)   STRAIGHT UNIVERSE                            
         DC    X'43',C'U',AL1(2)                                                
         DC    X'43',C'U',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
DEMOSHR  DS    0XL3                HOMES SHARES                                 
         DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*  OLYMPIC EXCLUSION DATE TABLE                                                 
*  BYTES 1-2 BOOK YYMM                                                          
*  BYTES 3-5 SWEEP START DATE COMPRESSED                                        
*  BYTES 6-8 SWEEP END DATE COMPRESSED                                          
*                                                                               
OLMEXCDT DC    XL6'6A02D43DD45B'                                                
***      DC    XL6'6A02D43DD45B'                                                
***      DC    XL6'6E02DC44DC63'                                                
         DC    XL6'7202E43EE45A'                                                
         DC    XL6'7602EC48EC59'                                                
***      DC    XL6'6602CC3CCC5B'                                                
         DC    X'FF'                                                            
         LTORG                                                                  
         DROP  RB,RA,R9,R3                                                      
         EJECT                                                                  
*===================================================================*           
* LOOK UP FIRST DEMO FOR EACH BREAK TIME IN AFFID LIST TO DETERMINE *           
* WHICH HAS LOWEST RATING. THEN CHANGE TIME TO APPROPRIATE QH.      *           
* FEATURE CONTROLLED BY CHARACTER 5 OF SPD0 PROFILE                 *           
*===================================================================*           
         SPACE 1                                                                
LQHSET   NMOD1 0,**LQH***                                                       
         L     RC,0(R1)            RESTORE WORK AREA POINTER                    
*                                                                               
         XC    LQHLIST,LQHLIST     BUILD DEMO LIST (FIRST RATING)               
         MVC   LQHLIST(4),LRAD3564                                              
*                                                                               
         L     RF,DBCOMFCS            GET YYMMDD VIA DATCON                     
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,AFDATE),WORK                                        
*                                                                               
         L     RF,DBCOMFCS            GET DAY VIA GETDAY                        
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,WORK,WORK+6                                            
*                                                                               
         LLC   R1,0(R1)            GET DAY                                      
         IC    R1,LDAYTAB-1(R1)                                                 
         STC   R1,DBSELDAY                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,AFDTIME        GET TIME                                     
         BAS   RE,TIMTOQH          CONVERT TIME TO START QH                     
         STC   R1,SQHNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,EQHNUM                                                        
*                                                                               
         L     R0,DUB+4            GET MINUTES                                  
         CHI   R0,3                TEST BETWEEN 00 AND 03                       
         BNH   LQH14               YES- BREAK SPOT                              
*                                                                               
         CHI   R0,27               TEST BETWEEN 04 AND 27                       
         BL    LQHX                YES - NOT A BREAK SPOT                       
         CHI   R0,33               TEST BETWEEN 27 AND 33                       
         BH    LQH16               NO                                           
         CHI   R0,30               TEST BETWEEN 27 AND 30                       
         BL    LQH20               YES                                          
*                                                                               
LQH14    BCTR  R1,0                BACKUP 1 QH FOR 00-03 AND 30-33              
         STC   R1,EQHNUM                                                        
         BCTR  R1,0                                                             
         STC   R1,SQHNUM                                                        
         B     LQH20                                                            
*                                                                               
LQH16    CHI   R0,57               TEST BETWEEN 34 AND 57                       
         BL    LQHX                YES - NOT A BREAK SPOT                       
*                                                                               
* BREAK SPOT                                                                    
*                                                                               
LQH20    L     R0,=F'-1'           SET FLAG FOR NOT FOUND                       
         ST    R0,SQHVAL                                                        
         ST    R0,EQHVAL                                                        
*                                                                               
         XC    DBSEL1WK,DBSEL1WK   CLEAR WEEKLY REQUEST FIELDS                  
         MVI   DBSELWKN,0                                                       
         TM    WKLYOPT,X'80'       TEST WEEKLY LOOK-UP                          
         BZ    *+10                NO                                           
         MVC   DBSEL1WK(2),AFDATE  SET WEEK DATE                                
*                                                                               
         LLC   R1,SQHNUM                                                        
         BAS   RE,QHTOTIM          CONVERT START QH TO TIME                     
         STCM  R1,3,DBSELTIM       SET START TIME FOR DEMO LOOKUP               
*                                                                               
         BRAS  RE,SETDQLNK                                                      
*                                                                               
         MVI   DEMANDSW,0                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,LQHHOOK                                         
         MVC   SQHVAL,EQHVAL       SAVE VALUE FROM EQHVAL                       
*                                                                               
         LLC   R1,EQHNUM                                                        
         BAS   RE,QHTOTIM                                                       
         STCM  R1,3,DBSELTIM                                                    
*                                                                               
         BRAS  RE,SETDQLNK                                                      
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,EQHNUM                                                        
         CLC   EQHVAL,SQHVAL       COMPARE RATINGS, EQH TO SQH                  
         BH    LQH30               IF EQH IS HIGH, USE SQH                      
         BAS   RE,QHTOTIM          ELSE USE EQH                                 
         B     LQH35                                                            
*                                                                               
LQH30    IC    R1,SQHNUM           GET SQHNUM                                   
         BAS   RE,QHTOTIM          CONVERT QH TO TIME                           
*                                                                               
LQH35    LA    R1,5(R1)            ADD 5 MINUTES SO NEVER IN BREAK              
         STCM  R1,3,AFDTIME        AND OVERWRITE TIME IN AFDLIST                
*                                                                               
LQHX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
LRAD3564 DC    X'81',C'R',AL1(153)                                              
         DC    X'FF'                                                            
*                                                                               
LDAYTAB  DC    X'40201008040201'   DAY TABLE FOR DEMAND CALLS                   
         EJECT                                                                  
*========================================================*                      
* CONVERT TIME TO START QUARTER HOUR NUMBER              *                      
* INPUT IS TIME IN R1, OUTPUT IS QH IN R1                *                      
* RETURN HOUR IN DUB, MINUTES IN DUB+4                   *                      
*========================================================*                      
         SPACE 1                                                                
TIMTOQH  SR    R0,R0                                                            
         D     R0,=F'100'          HOUR IN R1/MIN IN R0                         
         SLL   R1,2                                                             
         ST    R1,DUB              STORE HOUR IN DUB                            
         ST    R0,DUB+4            STORE MINUTES IN DUB+4                       
         SRDL  R0,32                                                            
         D     R0,=F'15'                                                        
         A     R1,DUB                                                           
         BR    RE                                                               
         SPACE 1                                                                
QHTOTIM  SR    R0,R0                                                            
         D     R0,=F'4'            HOURS IN R1/QH IN R0                         
         MHI   R1,100              GIVES HOURS X 100                            
         MHI   R0,15               GIVES QH X 15                                
         AR    R1,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*================================================================*              
* DEMAND HOOK FOR LQH TO EXTRACT RHOMES                          *              
*================================================================*              
         SPACE 1                                                                
LQHHOOK  NTR1                                                                   
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),P1,(C'L',LQHLIST),DBLOCK,EQHVAL                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE TO CALCULATE ACTUAL SVI VALUES FOR ACCUMS AT 0(R1) *               
*             VALUES ARE RETURNED IN SVIS                       *               
*****************************************************************               
         SPACE 2                                                                
GDSVI    NTR1  BASE=*,LABEL=*                                                   
         L     R4,NUMDEMS                                                       
         LA    R5,SVIS                                                          
         XC    SVIS,SVIS                                                        
*                                                                               
GDSVI2   ICM   RE,15,MAXDEMS*4(R1)                                              
         BNZ   *+10                                                             
         SR    RF,RF                                                            
         B     GDSVI4                                                           
         SR    RE,RE                                                            
         L     RF,0(R1)                                                         
* COMSCORE NULL VALUES ARE PASSED FOR DEMOS WHICH HAS NO VALUE                  
* FROM COMSCORE API CAL                                                         
         CLC   0(4,R1),=X'FFFFFFFF'  COMSCORE X'FFFFFFFF' SAME                  
         BNE   GDSVI3                AS NO VALUE                                
         SR    RF,RF                                                            
         B     GDSVI4                                                           
***      CLC   0(4,R1),=X'FFFFFFFF'                                             
***      BE    GDSVI4                                                           
*                                                                               
GDSVI3   SLDL  RE,1                X 2                                          
         D     RE,MAXDEMS*4(R1)                                                 
         AHI   RF,1                                                             
         SRL   RF,1                                                             
GDSVI4   ST    RF,0(R5)                                                         
         LA    R1,4(R1)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,GDSVI2                                                        
         J     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
RMBKUNIV NTR1  BASE=*,LABEL=*                                                   
* ONLY SBTK SHOULD BE CALLING RADIO MBK FEATURE AND THEY SHOULD HAVE            
* HELLO SET IN COMFACS.                                                         
*                                                                               
* ONLY IF THE APPLICATION IS READY TO USE THE 1ST NOOK'S UNIVERSE               
* FEATURE.  SBTK VERSION  AFTER 4.6.0.208 SUPPORTS THIS FEATURE                 
         TM    SPLKOPT2,SPLKOPT2_RMBKU                                          
         JZ    EXIT                                                             
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CHELLO-COMFACSD(RF)                                           
         OR    RF,RF                                                            
         JZ    EXIT                                                             
*                                                                               
         L     RE,SVRMBKEX                                                      
         USING DBXMBD,RE                                                        
         CLC   DBACTBK,DBXMBKS     IS THIS THE 1ST BOOK OF AVG?                 
         JNE   RMBKUV10                                                         
* 1ST BOOK - SAVE OFF 43 ELEMENT FOR UNIVERSE                                   
         XC    SVRADUNV,SVRADUNV                                                
         L     R6,DBAQUART                                                      
*                                                                               
         BAS   RE,NEXTEL43                                                      
***      JE    *+6                 BETTER FIND X'43' UNIVERSE ELEMENT           
***      DC    H'0'                                                             
         JE    RMBKUV08            BETTER FIND X'43' UNIVERSE ELEMENT           
         J     EXIT                JUST EXIT IF 43 NOT FOUND-PREVENTIVE         
* FOUND 43 UNIVERSE ELEMENT                                                     
RMBKUV08 ZIC   RE,1(R6)                                                         
         SHI   RE,1                                                             
         AHI   RE,3                3 BYTE DEMO ELEMENT HEADER                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVRADUNV(0),0(R6)                                                
* SUBSEQUENT BOOKS                                                              
RMBKUV10 DS    0H                                                               
* PREVENTIVE CODE IS THE SAVED UNIVERSE FAILED FROM 1ST BOOK IN AVG             
* JUST EXIT.  DONT REPLACE.                                                     
*                                                                               
         OC    SVRADUNV,SVRADUNV                                                
         JZ    EXIT                                                             
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CHELLO-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,(C'D',=C'DEMFILE '),(X'43',DBAREC),0,0                 
         L     RF,DBCOMFCS                                                      
         L     RF,CHELLO-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,(C'P',=C'DEMFILE '),DBAREC,SVRADUNV,0                  
         J     EXIT                                                             
NEXTEL43 SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NXTEL43X                                                         
         CLI   0(R6),X'43'                                                      
         BL    NEXTEL43                                                         
         CLI   0(R6),X'43'                                                      
         BH    NXTEL43X                                                         
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
NXTEL43X LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
CLRHOMEV NTR1  BASE=*,LABEL=*                                                   
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         J     EXIT                                                             
*------------------------ GET A(DAY/QH TABLE) ------------------------*         
                                                                                
* AT ENTRY,                                                                     
*   DBLOCK   IS SET                                                             
* AT EXIT,                                                                      
*   ADBDQD   = A(INTERNAL DAY/QHR TABLE)                                        
                                                                                
GETADQTB NTR1  BASE=*,LABEL=*                                                   
*        LA    R3,DBDQD             PICK UP ADDR DIRECTLY FROM HERE             
         LA    R8,DBDQD             PICK UP ADDR DIRECTLY FROM HERE             
         CLC   =AL2(DBDQUXTD),DBDQTAB+0  DO WE NEED TO LOOK IN XTND?            
         BNE   GADQT12X                   NOPE                                  
*                                                                               
         DS    0H                                                               
         LA    RF,DBEXTEND-4                                                    
GADQT12G DS    0H                   LOOK THROUGH DBEXTEND                       
         ICM   RF,15,4(RF)                                                      
         BZ    GADQT12X                                                         
         CLC   0(4,RF),DBDQTAB+2     FOR NAME GIVEN HERE                        
         BNE   GADQT12G                                                         
*        LA    R3,DBDQXFXL(RF)                                                  
         LA    R8,DBDQXFXL(RF)                                                  
GADQT12X EQU   *                                                                
*                                                                               
         DS    0H                                                               
*        ST    R3,ADBDQD                                                        
         ST    R8,ADBDQD                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*           SUBROUTINE TO COMPUTE WEIGHTED DEMO                 *               
*                                                               *               
*****************************************************************               
         SPACE 2                                                                
GETWD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,DEMOLIST                                                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    R5,DEMOLST2                                                      
                                                                                
         L     R6,SPLKAVAL                                                      
         LA    R8,SAVEWGTS                                                      
         XC    WORK,WORK                                                        
         XC    WTDEMA(8),WTDEMA                                                 
*                                                                               
GETWD2   CLI   1(R6),63            TEST WEIGHTED DEMO                           
         BNE   GETWD4                                                           
         ST    R6,WORK             ELSE SAVE DATA ADDRESS                       
         MVC   WORK+7(1),0(R8)     AND WEIGHT (IF ANY)                          
         B     GETWD6                                                           
*                                                                               
GETWD4   SR    R0,R0                                                            
         ICM   R0,1,0(R8)          GET WEIGHT VALUE                             
         BZ    GETWD6                                                           
*                                                                               
         L     RF,0(R6)            UNADJ DEMO VALUE                             
         MR    RE,R0                                                            
         A     RF,WTDEMU                                                        
         ST    RF,WTDEMU                                                        
*                                                                               
         L     RF,0(R6)            UNADJ DEMO VALUE                             
         L     RE,4(R6)            SVI FACTOR                                   
         MR    RE,RE               GIVES ADJ DEM                                
         MR    RE,R0               X WEIGHT                                     
         A     RF,WTDEMA                                                        
         ST    RF,WTDEMA                                                        
*                                                                               
GETWD6   LA    R5,3(R5)            NEXT DEMO IN LIST                            
         LA    R6,8(R6)            NEXT DEMO VALUE                              
         LA    R8,1(R8)            NEXT WEIGHT                                  
         CLI   1(R5),0             TEST E-O-L                                   
         BNE   GETWD2                                                           
         EJECT                                                                  
* DIVIDE ADJ BY UNADJ VALUE TO GET SVI *                                        
         SPACE 1                                                                
GETWD7   ICM   R6,15,WORK          POINT TO WEIGHTED DEMO SLOT                  
         JZ    EXIT                                                             
         SR    RE,RE                                                            
         L     RF,WTDEMA           GET ADJ VALUE                                
         SLDL  RE,1                X 2                                          
         ICM   R0,15,WTDEMU                                                     
         BZ    *+6                                                              
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,4(R6)            SET SVI VALUE                                
*                                                                               
         MVC   4(4,R6),WTDEMU      WEIGHTED DEMO VALUE                          
         ICM   RF,15,4(R6)                                                      
         OC    WORK+4(4),WORK+4    TEST WEIGHT FOR WTD DEMO                     
         BZ    GETWD8                                                           
         SR    RE,RE                                                            
         L     RF,WTDEMU           <- PER MEL                                   
         SLDL  RE,1                X 2                                          
         D     RE,WORK+4           DIVIDE BY WEIGHT                             
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,0(R6)                                                         
         SPACE 1                                                                
GETWD8   CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         JNE   EXIT                                                             
         SPACE 1                                                                
* CALC DEMO VALUE X SPOTS *                                                     
         SPACE 1                                                                
         LH    RE,NOAFDCNT                                                      
         AH    RE,AFDCNT                                                        
         MR    RE,RE                                                            
         EJECT                                                                  
GETWD14  L     R1,SPLKAVAL                                                      
         LA    R6,DEMOLIST                                                      
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         LA    R6,DEMOLST2                                                      
*                                                                               
GETWD16  CLI   1(R6),63            TEST WEIGHTED DEMO                           
         BE    GETWD18                                                          
         LA    R1,8(R1)            NEXT SLOT IN OUTPUT AREA                     
         LA    R6,3(R6)            NEXT DEMO IN LIST                            
         CLI   0(R1),0                                                          
         BNE   GETWD16                                                          
         DC    H'0'                                                             
*                                                                               
GETWD18  ST    RF,0(R1)            SET DEMO VALUE                               
         LA    R0,100                                                           
         ST    R0,4(R1)            AND SVI                                      
         J     EXIT                                                             
         EJECT                                                                  
***************************************************************                 
* INSPECT DEMO LIST TO SEE IF WE HAVE COMSCORE DEMOS/NIELSEN                    
* DEMOS OR MIX OF BOTH                                                          
***************************************************************                 
INSPDEM  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,DEMOLIST                                                      
         MVI   NTDFLAG,0                                                        
INSPD10  CLI   0(RE),X'FF'                                                      
         BE    INSPDEMX                                                         
         CLI   2(RE),0                                                          
         BNE   *+8                                                              
         OI    NTDFLAG,NTDCOMD                                                  
         CLI   2(RE),0                                                          
         BNH   *+8                                                              
         OI    NTDFLAG,NTDNSID                                                  
         AHI   RE,3                                                             
         B     INSPD10                                                          
INSPDEMX J     EXIT                                                             
         EJECT                                                                  
         DROP  R2                                                               
***************************************************************                 
* CALL COMINTER                                                                 
***************************************************************                 
COMSCORE NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         L     R6,4(R1)                                                         
         USING DBLOCK,R6                                                        
COMSC01  DS    0C                                                               
***      BRAS  RE,FINDNTDM                                                      
         MVC   ANTDELEM,ASPXT50E      A(NTDELEM PASSED IN)                      
         BRAS  RE,TRAMKT                                                        
         L     RE,ADBUY                                                         
         USING BUYRECD,RE                                                       
         MVC   BSDATE,BDSTART      SAVE BUYLINE START DATE                      
         MVC   BENDDATE,BDEND       SAVE BUYLINE END DATE                       
         DROP  RE                                                               
* RESTORE DBSELSTA TO REQUEST STATION- DEMO SYSTEM CHANGES DBSELSTA             
* FOR FUSION TO INTERNAL NUMERIC STATION NUMBER                                 
         MVC   DBSELSTA,SPLKSTA                                                 
*                                                                               
         ICM   RE,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    COMSC02             NO                                           
         USING SPLKXTD,RE          DEMO EXTENSION DSECT                         
         TM    SPXTFLG2,SPXTPP     PARENT+ STATION?                             
         BZ    COMSC02             NO                                           
         CLI   DBSELMED,C'T'       MEDIA T?                                     
         BNE   COMSC02             NO                                           
         MVI   DBSELSTA+4,C'+'     OVERWRITE TRAIILING T                        
         CLI   DBSELSTA+3,C' '     3 CHAR STATION?                              
         BNE   COMSC02             NO                                           
         MVI   DBSELSTA+3,C'+'     OVERWRITE TRAIILING T                        
         MVI   DBSELSTA+4,C' '     RESET TO A SPACE                             
         DROP  RE                  DROP DEMO EXTENSION USING                    
*                                                                               
COMSC02  L     RE,ACOMEXT                                                       
         XC    0(L'COMEXT,RE),0(RE)                                             
         USING DBCMINTD,RE                                                      
         MVC   DBCMID,=C'CLOC'        COMINTER                                  
         MVC   DBCMNEXT,DBEXTEND                                                
         MVC   DBEXTEND,ACOMEXT                                                 
         MVC   DBCMREQS,REQSDATE      REPORT REQUEST START DATE                 
         MVC   DBCMREQE,REQEDATE      REPORT REQUEST END DATE                   
         TM    SPLKOPT2,SPLKOPT2_ACT  IF POSTING AGAINST BK=MMM/YY              
         BNZ   COMSC10                THEN SET FORCE BOOK                       
         CLI   OVPHOPT,C'Y'           DO NOT SET FORCE BOOK FOR                 
         BE    COMSC10                NIGHTS, ONLY FOR MONTHY POSTING           
         MVC   DBCMFRBK,DBSELBK                                                 
COMSC10  MVI   DBCMPREC,C'1'                                                    
         CLI   TWODEC,C'Y'                                                      
         BNE   *+8                                                              
         MVI   DBCMPREC,C'2'       2 DECIMAL RATING PRECISION                   
*                                                                               
         MVC   DBCMMKT,SVNUMMKT                                                 
         LA    R0,DEMOLIST                                                      
         ST    R0,DBDEMOL                                                       
         LA    R0,DTDEMSU                                                       
         ST    R0,DBDEMVAL                                                      
**       MVC   DBDEMOL,=A(DEMOLIST)                                             
**       MVC   DBDEMVAL,=A(DTDEMSU)                                             
         MVC   DBCMAEST,ANTDELEM  A( NO TRADIITONAL DEMO LIST)                  
         MVI   DBCMBTYP,C'L'      ALWAYS LIVE ONLY BOOKTYPE                     
*********************************************************                       
* HARDCODE FOR NOW UNTIL ALLEN GIVES US FIELD                                   
***      MVI   DBCMDATE,DBCMBRD                                                 
*********************************************************                       
         MVC   DBCMDATE,COMCSD                                                  
*                                                                               
*                                                                               
COMACHVD CLC   =C'ACHVD',0(R2)    ACHIEVED                                      
         BNE   COMAFFD                                                          
         CLI   OVPHOPT,C'Y'       IF OVERNIGHTS REQUESTED                       
         BE    *+8                                                              
         CLI   OVPHOPT,C'M'                                                     
         BNE   *+14                                                             
         MVC   DBCMSPDT,SPLKAFST  SET SPOTDATE FIELD WITH WEEK DATE             
         B     COMPASS1                                                         
* MONTHLY POSTING - USE BUYDATES                                                
         MVC   DBCMSTDT,BSDATE    BUYLINE START DATE                            
         MVC   DBCMENDD,BENDDATE  BUYLINE END DATE                              
*                                                                               
         B     COMPASS1                                                         
*                                                                               
COMAFFD  MVC   DBCMSPDT,AFDATE    AFFID SETS SPOT DATE                          
         DROP  RE                                                               
*                                                                               
COMPASS1 DS    0C                                                               
         CLI   COMPASS,1                                                        
         BNE   COMPASS2                                                         
         GOTO1 VCOMINTR,DMCB2,=C'PUT',DBLOCK                                    
         B     COMSCORX                                                         
COMPASS2 DS    0C                                                               
         CLI   COMPASS,2                                                        
         BNE   COMSCORX                                                         
         MVI   COMERRF,0                                                        
         XC    DMCB2,DMCB2                                                      
         GOTO1 VCOMINTR,DMCB2,=C'GET',DBLOCK                                    
         CLI   DMCB2,X'02'          FAIL STATUS?                                
         BNE   *+10                                                             
         MVC   COMERRF,DMCB2        SAVE ERROR CODE FROM COMINTER               
*                                                                               
         IF    (TM,SPLKOPT2,SPLKOPT2_NLSN,Z) DONT HAVE NIELSEN DEMOS            
         L     RE,ACOMEXT                                                       
         USING DBCMINTD,RE                                                      
         MVC   STNAME,DBCMPNAM                                                  
         MVC   ENDNAME,DBCMPNAM                                                 
         DROP  RE                                                               
         ENDIF                                                                  
*                                                                               
         B     COMSCORX                                                         
COMSCORX L     RE,ADBLOCK2                                                      
         MVC   DBLOCK(256),0(RE)     RESTORE DBLOCK                             
         MVC   DBSELSTA,SPLKSTA      IN CASE WE CHANGED FOR PARENT+             
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
* TRANSLATE SVMKALPH ALPHA MKT TO SVNUMMKT (NIELSEN NUMERIC MKT)                
*                                                                               
TRAMKT   NTR1  BASE=*,LABEL=*                                                   
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
         L     R6,ADBLOCK2                                                      
         MVC   0(256,R6),DBLOCK      SAVE DBLOCK                                
*                                                                               
         MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   DBSELALF,SVMKALPH                                                
         MVI   DBSELMED,C'T'         USE MEDIA/BOOK LAST READ                   
         MVI   DBSELSRC,C'N'         USE NSI MARKETS                            
*                                                                               
                                                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0,0                                             
         CLI   DBERROR,0                                                        
         BNE   TRAMKTX                                                          
         MVC   SVNUMMKT,DBSELRMK                                                
         CLC   DBSELRMK,=H'168'     ATLANTA IS 168 IN OUR SYSTEM                
         BNE   *+10                 BUT IT SHOULD BE 124 TO REST OF             
         MVC   SVNUMMKT,=H'124'     THE WORKD                                   
                                                                                
TRAMKTX  MVC   DBLOCK(256),0(R6)     RESTORE                                    
         J      EXIT                                                            
**************************************************************                  
* FIND NO TRADITIONAL DEMO LIST IN BUY RECORD                                   
**************************************************************                  
FINDNTDM NTR1  BASE=*,LABEL=*                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
         XC    ANTDELEM,ANTDELEM                                                
         LA    R6,BDELEM                                                        
         MVC   BSDATE,BDSTART      SAVE BUYLINE START DATE                      
         MVC   BENDDATE,BDEND       SAVE BUYLINE END DATE                       
         MVI   ELCODE,NTDELCDQ                                                  
FINDNT10 BAS   RE,NEXTEL4           BETTER FIND THE DEMO LIST                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ELCODE,NTDELCDQ                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         STCM  R6,15,ANTDELEM                                                   
*                                                                               
FINDNTTX XIT1   1                                                               
NEXTEL4  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTEL4X                                                         
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                  EXIT WITH CC EQ                              
         B     NEXTEL4                                                          
NEXTEL4X LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
**************************************************************                  
* IS COMSCORE DEMO RATING                                                       
**************************************************************                  
ISCOMRAT NTR1  BASE=*,LABEL=*                                                   
         OC    ANTDELEM,ANTDELEM                                                
         BZ    ISCOMRTN                                                         
         L     RE,0(R1)             A(COMSCORE DEMO)                            
***      L     RF,0(RE)                                                         
         ZICM  R0,0(RE),(3)                                                     
         SHI   R0,1                 R0 = INDEX TO DEMO NAMES LIST               
*                                                                               
         L     R2,ANTDELEM                                                      
         LA    RF,9                 LENGTH OF EACH DEMO NAME IN LIST            
         SR    RE,RE                                                            
         MR    RE,R0                                                            
         AR    RF,R2                A(DEMO NAME FOUND)                          
         AHI   RF,2                 NUMP PASS ELCODE, LENGTH OF ELEMENT         
*                                                                               
***      CLI   0(RF),NTDELCDQ                                                   
***      JNE   ISCOMRTN                                                         
         CLI   0(RF),C'R'                                                       
         BE    ISCOMRTY                                                         
         B     ISCOMRTN                                                         
*                                                                               
ISCOMRTY CR    RB,RB                                                            
         B     ISCOMRTX                                                         
ISCOMRTN CR    RB,RE                                                            
         B     ISCOMRTX                                                         
ISCOMRTX J     EXIT                                                             
***********************************************************************         
* FILL SPDTTAB WITH X'0B'/X'0C' SPOT DATE ELEMENTS IN BUYREC          *         
***********************************************************************         
SETSPDTS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,BDELEM           A(FIRST BUY ELEMENT)                         
         L     R2,ASPDTTAB         A(SPOT DATES TABLE)                          
         XC    PREVSPDT,PREVSPDT   INIT PREVSPDT                                
         XC    NUMSPOTS,NUMSPOTS   INIT NUMSPOTS                                
*                                                                               
         MVI   ELCDLO,X'0B'        LOOK FOR X'0B' ELEMENTS                      
         MVI   ELCDHI,X'0C'        LOOK FOR X'0C' ELEMENTS                      
*                                                                               
FILLSP10 BRAS  RE,NEXTEL3          HAVE X'0B'/X'0C' ELEMENT?                    
         BNE   FILLSP30            NO - DONE BUILDING DATE TABLE                
*                                                                               
         USING REGELEM,R6          SPOT ELEMENT DSECT                           
         TM    RSTATUS,RSMINUSQ    MINUS SPOT?                                  
         BNZ   FILLSP10            YES - SKIP THIS ELEMENT                      
         TM    RSTATUS,RSMINSDQ    SPOT HAS BEEN MINUSED?                       
         BNZ   FILLSP10            YES - SKIP THIS ELEMENT                      
         CLC   RDATE,PREVSPDT      SAME DATE AS PREVIOUS SPOT?                  
         BNE   FILLSP20            NO - ADD THIS DATE TO THE TABLE              
* SAME SPOT DATE AS PREVIOUS                                                    
         SHI   R2,1                BACK UP TABLE POINTER 1 BYTE                 
         ZIC   R0,0(R2)            CURRENT OF TIMES WE HAVE THIS DATE           
         AHI   R0,1                ADD ONE TO TIMES WE HAVE THIS DATE           
         STC   R0,0(R2)            NUM OF TIMES WE NOW HAVE THIS DATE           
         AHI   R2,1                RESTORE TABLE POINTER                        
         B     FILLSP10            GET NEXT SPOT ELEMENT                        
* NEW UNIQUE SPOT DATE                                                          
FILLSP20 MVC   PREVSPDT,RDATE      SAVE THE CURRENT SPOT DATE                   
         MVC   0(2,R2),RDATE       SPOT DATE IN TABLE                           
         MVI   2(R2),1             SO FAR, THIS DATE OCCURS ONCE                
         AHI   R2,3                BUMP TO NEXT TABLE ENTRY                     
         ZICM  R0,NUMSPOTS,(3)     R0 = NUMBER OF UNIQUE SPOT DATES             
         AHI   R0,1                ADD ONE                                      
         STCM  R0,3,NUMSPOTS       NUMBER OF UNIQUE SPOT DATES                  
         CLC   NUMSPOTS,=H'300'    HAVE UNDER 300 DATE ENTRIES?                 
         BL    FILLSP10            YES - CONTINUE BUILDING DATE TABLE           
         DROP  R6                  DROP SPOT ELEMENT USING                      
*                                                                               
FILLSP30 OC    NUMSPOTS,NUMSPOTS   DO WE HAVE ANY SPOT DATES?                   
         BZ    FILLSPTX            NO - DONE                                    
*                                                                               
         L     RF,AGDEXSPD         A(SPOT POSTING DATES TABLE)                  
         USING DBSPDTD,RF          SPOT POSTING DATES DSECT                     
         MVC   DBSPDNXT,DBEXTEND   LINK TO PREV DBEXTEND                        
         STCM  RF,15,DBEXTEND      SET A(EXTENDED DBLOCK)                       
         MVC   DBSPDTID,=C'SPDT'   SPOT DATE ID                                 
         MVC   DBSPASPT,ASPDTTAB   A(SPOT DATES TABLE)                          
         MVC   DBSPNSPT,NUMSPOTS   NUMBER OF SPOTS                              
         MVI   DBSPLEN,3           3 BYTE ENTRIES ( DATE(2),DUP(1) )            
         DROP  RF                  DROP DBSPDTD USING                           
*                                                                               
FILLSPTX XIT1                      DONE                                         
*                                                                               
NEXTEL3  XR    R0,R0               CLEAR R0                                     
         ICM   R0,1,1(R6)          HAVE ELEMENT LENGTH                          
         BZ    NEXTEL3X            NO - AVOID INFINITE LOOP                     
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         CLI   0(R6),0             END OF RECORD?                               
         BE    NEXTEL3X            YES - RETURN CC NEQ                          
         CLC   0(1,R6),ELCDLO      ELEMENT < ELCDLO?                            
         JL    NEXTEL3             YES, GET NEXT ELEMENT                        
         CLC   0(1,R6),ELCDHI      ELEMENT > ELCDHI?                            
         JH    NEXTEL3             YES, GET NEXT ELEMENT                        
         CR    RE,RE               SET CC EQU                                   
         BR    RE                  RETURN                                       
NEXTEL3X LTR   RE,RE               SET CC NEQ                                   
         BR    RE                  RETURN                                       
*                                                                               
* JUL28/00 LVL=032 - CHANGED ALL BRANCH INSTRUCTIONS TO JUMP.  THIS             
*   ROUTINE IS CALLED FROM OTHER PLACES WHERE THE 1ST BASE REGISTER             
*   HAS BEEN CHANGED                                                            
                                                                                
SETDQLNK NTR1  BASE=*,LABEL=*                                                   
         CLI   DBSELMED,C'T'       FOR MEDIA=USTV                               
         JNE   SDQLX                                                            
         CLI   DBSELSRC,C'N'       FOR SOURCE=NSI                               
         JNE   SDQLX                                                            
         CLC   DBFILE,=C'TP '      FOR TP FILE                                  
         JNE   SDQLX                                                            
*                                                                               
         LA    RF,DBEXTEND-4                                                    
*                                                                               
SDQL010  DS    0H                                                               
         OC    4(4,RF),4(RF)                                                    
         JZ    SDQL020                                                          
*                                                                               
SDQL015  DS    0H                                                               
         ICM   RF,15,4(RF)                                                      
         CLC   0(3,RF),=C'DBD2'    (LENGTH OF 3 IS INTENTIONAL!!)               
         JE    SDQL040             FOUND CALLER'S D/QH LINK AREA                
         J     SDQL010                                                          
                                                                                
*                                                                               
SDQL020  DS    0H                                                               
         LA    R1,DBXTDQT                                                       
         STCM  R1,15,4(RF)                                                      
         XC    0(DBXTDQTL,R1),0(R1)                                             
         MVC   0(4,R1),=C'DBD2'                                                 
         LR    RF,R1                                                            
         J     SDQL040                                                          
                                                                                
*                                                                               
SDQL040  DS    0H                                                               
         MVC   DBDQTAB+0(2),=AL2(DBDQUXTD)                                      
         MVC   DBDQTAB+2(4),0(RF)                                               
                                                                                
*                                                                               
SDQLX    DS    0H                                                               
         J     EXIT                                                             
********************************************                                    
* SUBROUTINE TO READ SVI RECORD FROM HUTFL *                                    
********************************************                                    
         SPACE 1                                                                
RDSVI    NTR1  BASE=*,LABEL=*                                                   
         L     R8,ASVIREC                                                       
         USING SVIREC,R8                                                        
         MVI   SVIKCD,C'S'                                                      
         MVC   SVIKMED,DBSELMED                                                 
         MVC   SVIKSRC,DBACTSRC                                                 
         MVC   SVIKSCD,SVADJHUT                                                 
         MVC   SVIKMKT,SAVEMKT                                                  
         XC    WORK(9),WORK                                                     
         MVC   WORK(6),SVIKEY                                                   
         MVC   WORK+6(1),SVIDAY                                                 
         MVC   WORK+7(1),SVISQH                                                 
         XC    SVIKEY,SVIKEY                                                    
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),P1,=C'DMRDHI',=C'HUTFL',WORK,ASVIREC                        
         CLC   WORK(7),SVIKEY                                                   
         BNE   RDSVI2                                                           
         CLC   SVISQH,SVIKEQH                                                   
         BH    RDSVI2                                                           
         CLC   SVISQH,SVIKSQH                                                   
         BL    RDSVI2                                                           
*                                                                               
         LA    RE,SVIKEY                                                        
         SR    R0,R0                                                            
         ICM   R0,3,9(RE)          GET RECORD LENGTH                            
         AR    RE,R0                                                            
         XC    0(2,RE),0(RE)       SET E-O-R FLAG                               
         B     EQXIT                                                            
         SPACE 1                                                                
* IF SVI REC NOT FOUND MUST RESTORE KEY *                                       
         SPACE 1                                                                
RDSVI2   XC    SVIKEY,SVIKEY                                                    
         MVC   SVIKEY(6),WORK      RESTORE KEY THRU MKT                         
         B     NEQXIT                                                           
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         J     RDSVIX                                                           
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
RDSVIX   XIT1                                                                   
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* LOOK UP CANADIAN SOFT DEMO OVERRIDES                                          
*============================================================                   
                                                                                
GETSTOVR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RE,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    GETSTOV4            NO                                           
         USING SPLKXTD,RE                                                       
         TM    SPXTFLAG,SPXTRAPP   CALLER A RESEARCH APPLICATION ?              
         BO    GETOVRX                                                          
         TM    SPXTFLAG,SPXTCDEM   GO THROUGH OLD SOFT DEMO CODE?               
         BO    GETOVRX             NO, WE SET RTG/MKT/STA FROM BUY              
         DROP  RE                                                               
*                                                                               
GETSTOV4 XC    WORK,WORK                                                        
         ICM   R8,15,SPLKXTND                                                   
         BNZ   *+8                                                              
         LA    R8,WORK+40          IF NOT DEFINED, USE A DUMMY AREA             
         USING SPLKXTD,R8                                                       
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
         MVC   DMCB+12(4),DBAREC                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
*                                                                               
         LA    R9,WORK             BUILD KEY OF STATION RECORD                  
         USING STARECD,R9                                                       
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,DBSELMED    MEDIA                                        
         CLI   STAKMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL,DBSELSTA   STATION CALL LETTERS                         
         CLI   STAKCALL+4,C'/'                                                  
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,DBSELAGY    AGENCY CODE                                  
         MVC   STAKCLT,DBSELCLI    CLIENT CODE                                  
*                                                                               
***      L     RE,DBCOMFCS                                                      
***      ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
***      BNZ   GETOVR2             AND NO SAVE AREA AVAILABLE                   
*                                                                               
***      CLC   SPXTSVKY(12),STAKEY TEST SAME STA/CLT AS PREV                    
***      BE    GETOVR10            YES - USE PREVIOUS                           
*                                                                               
**GETOVR2  MVC   SPXTSVKY,STAKEY     ELSE SAVE STA/CLT NOW                      
         DROP  R9                                                               
         XC    SPXTSVMK,SPXTSVMK   AND CLEAR SAVED VALUES                       
         XC    SPXTSVST,SPXTSVST                                                
         MVI   SPXTSVFL,0                                                       
*                                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC                                                        
         USING STARECD,R9                                                       
*                                                                               
         CLC   STAKEY(12),WORK     DID WE FIND MASTER RECORD?                   
         BNE   GETOVR4             NO                                           
         BAS   RE,GETOVSTA                                                      
         B     GETOVR6                                                          
         DROP  R9                                                               
*                                                                               
GETOVR4  LA    R9,WORK             READ AGY DEFAULT STATION                     
         USING STARECD,R9                                                       
         MVC   STAKCLT,=3C'0'      CLEAR CLIENT CODE                            
         DROP  R9                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC                                                        
         USING STARECD,R9                                                       
*                                                                               
         CLC   STAKEY(12),WORK     DID WE FIND MASTER RECORD?                   
         BNE   GETOVR6             IGNORE MISSING STATION MASTER REC RD         
         BAS   RE,GETOVSTA                                                      
         DROP  R9                                                               
*                                                                               
GETOVR6  CLI   SPXTSVMK,C' '        HAVE MARKET OVERRIDE YET ?                  
         BH    GETOVR10             YES - WE DO NOT WANT ALPHA CODE             
*                                                                               
         LA    R9,WORK             NOT IN STATION, READ MKT REC                 
         USING MKTRECD,R9                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVI   MKTKTYPE,C'M'       RECORD TYPE                                  
         MVC   MKTKMED,DBSELMED    MEDIA                                        
         CLI   MKTKMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   MKTKMED,C'T'                                                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DBSELUMK       GET MKT NUM                                  
         BZ    GETOVRX                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,DBSELAGY    WE DO NOT ALWAYS HAVE THE BUY RECORD         
         OC    SPLKABUY,SPLKABUY   BUY RECORD SET?                              
         BZ    *+10                NO                                           
         MVC   MKTKAGY,BUYALPHA    YES - USE THAT AGENCY                        
         DROP  R9                                                               
*                                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC           PULL OUT ALPHAMKT CODE                       
         USING MKTRECD,R9                                                       
*                                                                               
         CLC   MKTREC(8),WORK      TEST WE FOUND IT                             
         BNE   GETOVR10            NO                                           
*                                                                               
         CLC   WORK(8),0(R9)                                                    
         BNE   GETOVR10                                                         
         CLI   MKTALST,C' '         ANY ALPHA-MKT CODE?                         
         BNH   GETOVR10             NO                                          
         MVC   SPXTSVMK,MKTALST     SAVE FIRST ALPHA MKT CODE                   
         DROP  R9                                                               
*                                                                               
GETOVR10 CLI   SPXTSVST,C' '       TEST ANY OVERRIDE STATION                    
         BNH   *+10                                                             
         MVC   DBSELSTA,SPXTSVST                                                
         CLI   DBSELALF,C' '       ALPHA MARKET ALREADY THERE?                  
         BH    GETOVRX             YES - WE CAME FOR THE STA OVERRIDE!          
*                                                                               
         CLI   SPXTSVMK,C' '       TEST ANY ALPHA MARKET                        
         BNH   GETOVRX                                                          
         MVC   DBSELALF,SPXTSVMK   PASS THROUGH ALPHAMKT                        
         XC    DBSELRMK,DBSELRMK                                                
         XC    DBSELMK,DBSELMK                                                  
*                                                                               
GETOVRX  XIT1                                                                   
         EJECT                                                                  
*===============================================================                
* EXTRACT MARKET/STATION OVERRIDES FROM STATION MASTER RECORD                   
*===============================================================                
                                                                                
         USING STARECD,R9                                                       
GETOVSTA NTR1                                                                   
         CLC   STAKLEN,=Y(STANCLNQ) CHECK RECLEN                                
         BL    GETOVRX             IF LOW, DATA NOT IN RECORD                   
*                                                                               
         MVC   SPXTSVMK,SMKTALPH   SAVE MARKET OVERRIDE                         
*                                                                               
         MVC   SPXTSVST,SRS1CALL   MOVE NSI LOOKUP CALL LETTERS                 
         LHI   RE,SQNORS1I         SET FOR STATION 1 IMPS FLAG                  
         CLI   SPLKSRC,C'N'        TEST NSI CLIENT                              
         BE    *+14                                                             
         MVC   SPXTSVST,SRS2CALL   ELSE USE BBM/ARB CALL LETTERS                
         LHI   RE,SQNORS2I                                                      
*                                                                               
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    SFLAG1,0 ** EXECUTED **                                          
         BZ    *+8                                                              
         MVI   SPXTSVFL,X'80'      SET TO SUPPRESS IMPS                         
         B     GETOVRX                                                          
         DROP  R8,R9                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GETVDAT  NTR1  BASE=*,LABEL=*      GET THE YEAR FOR WTP                         
         XC    VHSDAT(4),VHSDAT                                                 
         CLC   =X'0000',AFDATE     ANY AFFIDS                                   
         BE    GETVDAT1                                                         
         MVC   VHSDAT,AFDATE       YES - USE THEM                               
         B     *+10                                                             
GETVDAT1 MVC   VHSDAT(4),SPLKAFST  NO - USE REQUEST DATES                       
*                                                                               
         CLC   VHSDAT,=X'0000'     WE NEED DATES HERE                           
         BE    GETVDATX                                                         
         CLC   VHEDAT,=X'0000'                                                  
         BNE   *+10                                                             
         MVC   VHEDAT,VHSDAT                                                    
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VHSDAT),(3,DUB)                                     
         CLI   DUB,X'66'                                                        
         BNE   GETVDAT2                                                         
         CLC   DUB+1(2),=X'0C1B'                                                
         BL    GETVDATX                                                         
         MVI   DUB,X'67'                                                        
         B     GETVDATX                                                         
GETVDAT2 CLI   DUB,X'67'                                                        
         BNE   GETVDATX                                                         
         CLC   DUB+1(2),=X'0C1D'                                                
         BL    GETVDATX                                                         
         MVI   DUB,X'68'                                                        
GETVDATX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
********************************************************************            
* CHECK TO SEE IF THE BUY/AFFID DATE IS BEFORE THE MMW CUTOFF DATE.             
* WE NO LONGER HAVE MMW DATA STARTING WITH SEP1/12.                             
* RESET OPTIONS FOR MONTHLY LOOKUPS STARTING WITH CUTOFF DATE.                  
********************************************************************            
TOMONTHL NTR1  BASE=*,LABEL=*                                                   
         MVC   SVLPMLKOPT,LPMLKOPT  SAVE ORIGINAL OPTIONS                       
         MVC   SVWVPHOPT,WVPHOPT                                                
*                                                                               
         CLI   LPMLKOPT,C'Y'      CHECK WEEKLY OPTIONS SET                      
         BE    TOMON10                                                          
         CLI   WVPHOPT,C'Y'                                                     
         BE    TOMON10                                                          
         B     TOMONTHX                                                         
*                                                                               
TOMON10  BRAS  RE,GETVDAT         GET BUY/AFFID DATE IN DUB(BINARY YMD)         
         L     RF,DBCOMFCS                                                      
         MVC   DUB2,=C'120901'    CUTOFF DATE IS SEP1/12                        
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,DUB2),(3,DUB3)   CONVERT TO BINARY YMD              
         CLC   DUB(3),DUB3        COMPARE DATES                                 
         BL    TOMONTHX           PRIOR TO CUTOFF DATE, CONTINUE AS BEF         
*                               RESET TO MONTHLY LOOKUPS                        
         MVI   LPMLKOPT,C'N'      LPMWK=N                                       
         MVI   WVPHOPT,C'N'       WTP=N                                         
         MVI   DBSELMED,C'T'      MEDIA FOR MONTHLY                             
         MVI   TAPEOPT,C'Y'       MONTHLY DATA IS IMP BASED                     
         MVI   DBBEST,0                                                         
         MVI   MKTTYPE,0                                                        
*                                                                               
TOMONTHX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PWVPH    NTR1  BASE=*,LABEL=*                                                   
         MVI   DBSELMED,C'W'       SET FOR WEEKLY LOOKUP                        
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VHSDAT),(0,DUB) GET THE WEEK DATE                   
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO  CONVERT TO A BOOK                  
         MVC   DBSELBK(1),DMCB+4             AND SET IT IN DBLOCK               
         MVC   DBSELBK+1(1),DMCB                                                
*                                                                               
* IF NOT NOT POSTING APPLICATION THEN SKIP SUBSEQUENT OOWR CODE                 
         TM    CALLERF,SPXTSPWR                                                 
         BZ    PWVPHX                                                           
*                                                                               
********************************************************************            
* CALL GETDAY TO GET DAY OF THE WEEK OF SPOT DATE                               
* IF THE DAY OF THE SPOT LIES ON A DEMO SPLIT WEEK THEN READJUST THE            
* WEEK NUMBER BACK SO WE CAN GET THE CORRECT WEEK FOR THE ROTATION              
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DUB,SPOTDAY                                            
         MVC   BISPTDAY,0(R1)                                                   
         MVI   DBBEST,0                                                         
         CLI   AFDSW,C'Y'      ONLY DO THIS FOR ACHIEVED NOT AFFID              
         BE    PWVPMON         LOOKUPS                                          
*                                                                               
*                                                                               
*                                                                               
* ADJUST WEEK BASED ON OOWR AND DAY OF SPOTDATE                                 
* FIGURE OUT IF WE ARE OOWR AND SET DBBEST SO DEGETTP KNOWS WHAT                
* THE DAYS ARE FOR THE SPLIT WEEK.                                              
*                                                                               
*                                                                               
         CLI   AFDSW,C'Y'              TURN OFF SPLIT WEEK INDICATOR            
         BNE   PWVPSUN                 FOR AFFIDS                               
         MVI   DBBEST,0                                                         
         B     PWVPMON                                                          
PWVPSUN  TM    BDSTAT2,BDSDSKDQ        DAILY ON IN BUY?                         
         BZ    PWVPSUN1                DAILY CHECK ESTIMATE OOWR                
         CLI   ESTEOW,X'07'            SUNDAY TO SAT WEEK OUT OF WEEK           
         BNE   PWVPSAT                 ROTATION                                 
         MVI   DBBEST,X'60'            SAT  SPLIT TO NEXT WEEK                  
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT                           
         BE    PWVPSUB                 THEN BUMP BACK A WEEK                    
         B     PWVPHX                                                           
*NON DAILY SPOTDAY IS EXACT DAY OF ROTATION                                     
PWVPSUN1 CLC   SPOTDAY,=C'SUN'         SUNDAY TO SAT WEEK                       
         BNE   PWVPSAT                                                          
         MVI   DBBEST,X'60'            SAT  SPLIT TO NEXT WEEK                  
         B     PWVPHX                                                           
*                                                                               
PWVPSAT  TM    BDSTAT2,BDSDSKDQ        DAILY ON IN BUY?                         
         BZ    PWVPSAT1                DAILY CHECK ESTIMATE OOWR                
         CLI   ESTEOW,X'06'            SAT -FRI WEEK                            
         BNE   PWVPFRI                                                          
         MVI   DBBEST,X'65'            M-F SPLIT TO NEXT WEEK                   
         B     PWVPHX                                                           
PWVPSAT1 CLC   SPOTDAY,=C'SAT'         SAT- FRI WEEK                            
         BNE   PWVPFRI                                                          
         MVI   DBBEST,X'65'            M-F SPLIT TO NEXT WEEK                   
         B     PWVPHX                                                           
*                                                                               
PWVPFRI  TM    BDSTAT2,BDSDSKDQ        DAILY ON IN BUY?                         
         BZ    PWVPFRI1                                                         
         CLI   ESTEOW,X'05'            FRI -THU WEEK                            
         BNE   PWVPTHU                                                          
         MVI   DBBEST,X'64'            SAT-TH SPLIT TO NEXT WEEK                
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   PWVPSUB                 SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'04'          IF SPOTDAY=M-THU                         
         BNH   PWVPSUB                 SUBTRACT A WEEK BACK                     
         B     PWVPHX                                                           
PWVPFRI1 CLC   SPOTDAY,=C'FRI'         FRI WEEK                                 
         BNE   PWVPTHU                                                          
         MVI   DBBEST,X'64'            SAT-TH SPLIT TO NEXT WEEK                
         B     PWVPHX                                                           
*                                                                               
PWVPTHU  TM    BDSTAT2,BDSDSKDQ        DAILY ON IN BUY?                         
         BZ    PWVPTHU1                                                         
         CLI   ESTEOW,X'04'            THU -WED WEEK                            
         BNE   PWVPWED                                                          
         MVI   DBBEST,X'63'            SAT-WED SPLIT TO NEXT WEEK               
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   PWVPSUB                 SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'03'          IF SPOTDAY=M-WED                         
         BNH   PWVPSUB                 SUBTRACT A WEEK BACK                     
         B     PWVPHX                                                           
PWVPTHU1 CLC   SPOTDAY,=C'THU'         THU WEEK                                 
         BNE   PWVPWED                                                          
         MVI   DBBEST,X'63'            SAT-WED SPLIT TO NEXT WEEK               
         B     PWVPHX                                                           
*                                                                               
PWVPWED  TM    BDSTAT2,BDSDSKDQ        DAILY ON IN BUY?                         
         BZ    PWVPWED1                                                         
         CLI   ESTEOW,X'03'            WED -TUE WEEK                            
         BNE   PWVPTUE                                                          
         MVI   DBBEST,X'62'            SAT-TUE SPLIT TO NEXT WEEK               
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   PWVPSUB                 SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'02'          IF SPOTDAY=M-TUE                         
         BNH   PWVPSUB                 SUBTRACT A WEEK BACK                     
         B     PWVPHX                                                           
PWVPWED1 CLC   SPOTDAY,=C'WED'         WED WEEK                                 
         BNE   PWVPTUE                                                          
         MVI   DBBEST,X'62'            SAT-TUE SPLIT TO NEXT WEEK               
         B     PWVPHX                                                           
                                                                                
PWVPTUE  TM    BDSTAT2,BDSDSKDQ        DAILY ON IN BUY?                         
         BZ    PWVPTUE1                                                         
         CLI   ESTEOW,X'02'            TUE -MON WEEK                            
         BNE   PWVPMON                                                          
         MVI   DBBEST,X'61'            SAT-MON SPLIT TO NEXT WEEK               
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   PWVPSUB                 SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'01'          IF SPOTDAY=MON                           
         BE    PWVPSUB                 SUBTRACT A WEEK BACK                     
         B     PWVPHX                                                           
PWVPTUE1 CLC   SPOTDAY,=C'TUE'         TUE WEEK                                 
         BNE   PWVPHX                                                           
         MVI   DBBEST,X'61'            SAT-MON SPLIT TO NEXT WEEK               
         B     PWVPHX                                                           
********************************************************************            
* IF THE DAY OF THE SPOT IS SAT AND THE ROTATION IS SAT                         
* THEN ADJUST THE BOOK BACK ONE WEEK                                            
* SINCE GETTP DOES THE ADJUSTMENT OF SAT/SUN BY BUMPING A WEEK                  
* AHEAD FOR SAT/SUN READJUST SAT/SUN BACK A WEEK TO SAME                        
* AS M-F.  IF SPOTDAY IS SAT OR SUNDAY FOR M-F ROTATIONS                        
* ALSO GO BACK A WEEK.                                                          
                                                                                
PWVPMON  TM    BDSTAT2,BDSDSKDQ        DAILY ON IN BUY?                         
         BZ    PWVPMON1                                                         
         CLI   ESTEOW,X'01'            MON-SUN  WEEK                            
         BE    *+8                                                              
         CLI   ESTEOW,X'00'            MON-SUN  WEEK                            
         BNE   PWVPHX                                                           
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BL    PWVPHX                                                           
         TM    DBSELDAY,B'01111100'    AND ROTATION M-F THEN SUBTRACT           
         BO    PWVPSUB                 1 WEEK                                   
*                                                                               
PWVPMON1 CLC   SPOTDAY,=C'SUN'       SUBTRACT A WEEK FOR SUNDAY AND SAT         
         BE    *+10                  GIVEN A STANDARD M-SU BUY WEEK             
         CLC   SPOTDAY,=C'SAT'                                                  
         BNE   PWVPHX                                                           
*                                                                               
* ELSE IF SAT/SUN  SPOT DAY AND SAT/SUN ROTATION JUST SUBTRACT 1 WEEK           
* BECAUSE GETTP ALWAYS ADJUST SAT-SUN UP A WEEK                                 
         CLI   DBSELBK+1,1           IF THE BOOK IS ALREADY 1ST WEEK            
         BNE   PWVPSUB               THEN ADJUST BACK TO 54TH WEEK              
         LLC   RE,DBSELBK            SO THAT GETTP WILL KNOW TO ADJUST          
         SHI   RE,1                  THIS BACK TO 1ST WEEK OF NEXT YEAR         
         STC   RE,DBSELBK            WE HAVE CODE IN GETTP THAT ALWAYS          
         MVI   DBSELBK+1,54          BUMPS SAT-SUN A HEAD 1 WEEK FOR            
         B     PWVPHX                FOR MON-SUN STANDARD WEEKS                 
*                                                                               
PWVPSUB  DS    0C                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         XC    DUB3,DUB3                                                        
         GOTO1 (RF),DMCB,(2,VHSDAT),(0,DUB3)                                    
         L     RF,DBCOMFCS                    ADJUST A WEEK BACK                
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB3,DUB3,-7                                           
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
         B     PWVPHX                                                           
                                                                                
*                                                                               
*                                                                               
*                                                                               
PWVPHX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RMAD     NTR1  BASE=*,LABEL=*                                                   
         LA    RE,DTDEMSU                                                       
         LA    RF,RADEMSU2                                                      
***      LA    R3,DEMOLST2                                                      
         LA    R8,DEMOLST2                                                      
         ZICM  R4,DBFACTOR,(3)                                                  
*                                                                               
*RMAD10   CLI   0(R3),X'FF'         IF NOT EOL OF DEMOLIST,                     
RMAD10   CLI   0(R8),X'FF'         IF NOT EOL OF DEMOLIST,                      
         BE    RMADX                                                            
         SR    R0,R0                                                            
         ICM   R1,15,0(RE)          TAKE DEMO VALUE,                            
         MR    R0,R4                MULTIPLY BY DBFACTOR,                       
         ICM   R0,15,0(RF)                                                      
         AR    R0,R1                ADD IT TO RUNNING TOTAL                     
         STCM  R0,15,0(RF)          AND STORE NEW TOTAL                         
**       LA    R3,3(R3)                                                         
         LA    R8,3(R8)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         B     RMAD10                                                           
*                                                                               
RMADX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* ADJUST RADIO TIME SPEND LISTENING DEMO INTO TIME                              
ADJTSL   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,DTDEMSU                                                       
***      LA    R3,DEMOLST2                                                      
         LA    R8,DEMOLST2                                                      
*                                                                               
ADJTSL10 CLI   0(R8),X'FF'         IF NOT EOL OF DEMOLIST,                      
         BE    ADJTSLX                                                          
         CLI   1(R8),C'X'                                                       
         BNE   ADJTSL20                                                         
         SR    R0,R0                                                            
         ICM   R1,15,0(R4)          TAKE DEMO VALUE,                            
         M     R0,=F'15'                                                        
         D     R0,=F'600'                                                       
         LR    RF,R0                REMAINDER  = NUMBER OF MINUTES              
         SR    R0,R0                                                            
         M     R0,=F'100'           R1=# OF HOURS                               
*                                                                               
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         AR    R1,RF                R1= TOTAL TIME HOUR:MINUTES                 
*                                                                               
         STCM  R1,15,0(R4)          AND STORE NEW TOTAL                         
ADJTSL20 LA    R8,3(R8)                                                         
         LA    R4,4(R4)                                                         
         B     ADJTSL10                                                         
*                                                                               
ADJTSLX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*======================================================================         
* ROUTINE TO DEAL WITH END OF YEAR SPLIT WEEK SCENARIO                          
* FOR SET METERED MARKETS WHERE WEEK CROSSING INTO NEXT WEEKS DEMO REC          
* SET THE CORRECT DBSELDAY TO REREAD FOR THE SPLIT WEEK                         
*======================================================================         
SMMSPLIT NTR1  BASE=*,LABEL=*                                                   
         CLI   VPHSWK+1,52         WAS THE WEEK ASKED FOR 52ND WEEK?            
         BNE   SMMSPLTX                                                         
         CLI   WKINFLAG,C'N'       AND NOT WEEK IN FLAG IS SET                  
         BNE   SMMSPLTX                                                         
*                                                                               
SMMSPSUN DS    0C                   SUN-SA OOWR WEEK                            
         TM    BDSTAT2,BDSDSKDQ     DAILY HAVE TO CHECK ESTIMATE OOWR           
         BZ    SMMSSUN1             TO SEE IF WE HAVE END OF YR SPLIT           
         CLI   ESTEOW,X'07'         SUN-SA OOWR                                 
         BNE   SMMSPSAT                                                         
         B     SMMSSUN2                                                         
SMMSSUN1 CLC   SPOTDAY,=C'SUN'                                                  
         BNE   SMMSPSAT                                                         
SMMSSUN2 TM    DBSELDAY,B'01111101' IF 1 DAY BETWEEN SUN-FRI                    
         BZ    SMMSPLTX             AND                                         
         TM    DBSELDAY,B'00000010' SAT                                         
         BZ    SMMSPLTX                                                         
***      CLI   SVWTPDQH,X'60'      IF WE READ SAT THEN                          
***      BE    SMMSPLTX            WE HAD 53 WEEKS ON RECORD  ELSE              
         TM    DAYREAD,B'00000010' IF WE READ SAT THEN WE HAD                   
         BNZ   SMMSPLTX            53 WEEKS ON THE RECORD                       
*                                  NO NEED FOR NEXT YEAR'S REC                  
*                                   SAT NEED TO BE REREAD                       
         NI    DBSELDAY,B'00000010' NO NEED TO LOOKUP SU-FRI  ANYMORE           
         NI    SPLKDAY,B'00000010'  NO NEED TO LOOKUP SU-FRI  ANYMORE           
         B     SMMSPL80                                                         
*                                                                               
SMMSPSAT TM    BDSTAT2,BDSDSKDQ     DAILY HAVE TO CHECK ESTIMATE OOWR           
         BZ    SMMSSAT1             TO SEE IF WE HAVE END OF YR SPLIT           
         CLI   ESTEOW,X'06'         SAT HAVE NO SPLIT WEEK                      
         BNE   SMMSPFRI             JUST EXIT                                   
         B     SMMSPLTX             JUST EXIT                                   
SMMSSAT1 DS    0C                   SAT-FRI OOWR WEEK                           
         CLC   SPOTDAY,=C'SAT'      SAT-FRI  HAVE NO SPLIT WEEK EXIT            
         BE    SMMSPLTX             JUST EXIT                                   
SMMSPFRI DS    0C                   FRI TO THUR OOWR WEEK                       
         TM    BDSTAT2,BDSDSKDQ     DAILY HAVE TO CHECK ESTIMATE OOWR           
         BZ    SMMSFRI1             TO SEE IF WE HAVE END OF YR SPLIT           
         CLI   ESTEOW,X'05'         FRI-THU OOWR                                
         BNE   SMMSPTHU                                                         
         B     SMMSFRI2                                                         
SMMSFRI1 CLC   SPOTDAY,=C'FRI'                                                  
         BNE   SMMSPTHU                                                         
SMMSFRI2 TM    DBSELDAY,B'00000100' IF ITS A ROTATION WITH FRI                  
         BZ    SMMSPLTX             AND                                         
         TM    DBSELDAY,B'01111011' 1 DAY BETWEEN SA-TH                         
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01111011' IF WE READ SAT - THU THEN WE HAD             
         BNZ   SMMSPLTX            53 WEEKS ON THE RECORD                       
SMMFRI20 NI    DBSELDAY,B'01111011' NO NEED TO LOOKUP ANY FRI ANYMORE           
         NI    SPLKDAY,B'01111011'  NO NEED TO LOOKUP ANY FRI ANYMORE           
         B     SMMSPL80                                                         
SMMSPTHU DS    0C                   THU-WED OOWR WEEK                           
         TM    BDSTAT2,BDSDSKDQ     DAILY HAVE TO CHECK ESTIMATE OOWR           
         BZ    SMMSTHU1             TO SEE IF WE HAVE END OF YR SPLIT           
         CLI   ESTEOW,X'04'         THU-WED OOWR                                
         BNE   SMMSPWED                                                         
         B     SMMSTHU2                                                         
SMMSTHU1 CLC   SPOTDAY,=C'THU'                                                  
         BNE   SMMSPWED                                                         
SMMSTHU2 TM    DBSELDAY,B'00001100' IF ITS A ROTATION WITH TH-FRI               
         BZ    SMMSPLTX             AND                                         
         TM    DBSELDAY,B'01110011' 1 DAY BETWEEN SA-W                          
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01110011' IF WE READ SAT - WED THEN WE HAD             
         BNZ   SMMSPLTX            53 WEEKS ON THE RECORD                       
*                                                                               
SMMTHU20 NI    DBSELDAY,B'01110011' NO NEED TO LOOKUP ANY TH-F ANYMORE          
         NI    SPLKDAY,B'01110011'  NO NEED TO LOOKUP ANY TH-F ANYMORE          
         B     SMMSPL80                                                         
SMMSPWED DS    0C                   WED-TUE OOWR WEEK                           
         TM    BDSTAT2,BDSDSKDQ     DAILY HAVE TO CHECK ESTIMATE OOWR           
         BZ    SMMSWED1             TO SEE IF WE HAVE END OF YR SPLIT           
         CLI   ESTEOW,X'03'         WED-TUE OOWR                                
         BNE   SMMSPTUE                                                         
         B     SMMSWED2                                                         
SMMSWED1 CLC   SPOTDAY,=C'WED'                                                  
         BNE   SMMSPTUE                                                         
SMMSWED2 TM    DBSELDAY,B'00011100' IF ITS A ROTATION WITH WE-FRI               
         BZ    SMMSPLTX             AND                                         
         TM    DBSELDAY,B'01100011' 1 DAY BETWEEN SA-TU                         
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01100011' IF WE READ SAT - TUE THEN WE HAD             
         BNZ   SMMSPLTX            53 WEEKS ON THE RECORD                       
SMMWED20 NI    DBSELDAY,B'01100011' NO NEED TO LOOKUP ANY W-F ANYMORE           
         NI    SPLKDAY,B'01100011'  NO NEED TO LOOKUP ANY W-F ANYMORE           
         B     SMMSPL80                                                         
SMMSPTUE DS    0C                                                               
         TM    BDSTAT2,BDSDSKDQ     DAILY HAVE TO CHECK ESTIMATE OOWR           
         BZ    SMMSTUE1             TO SEE IF WE HAVE END OF YR SPLIT           
         CLI   ESTEOW,X'02'         TUE-MON OOWR                                
         BNE   SMMSPMON                                                         
         B     SMMSTUE2                                                         
SMMSTUE1 CLC   SPOTDAY,=C'TUE'                                                  
         BNE   SMMSPMON                                                         
SMMSTUE2 TM    DBSELDAY,B'00111100' IF ITS A ROTATION WITH TU-FRI               
         BZ    SMMSPLTX             AND                                         
         TM    DBSELDAY,B'01000011' 1 DAY BETWEEN SA-MO                         
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01000011' IF WE READ SAT - MON THEN WE HAD             
         BNZ   SMMSPLTX            53 WEEKS ON THE RECORD                       
SMMTUE20 NI    DBSELDAY,B'01000011' NO NEED TO LOOKUP ANY TU-F ANYMORE          
         NI    SPLKDAY,B'01000011'  NO NEED TO LOOKUP ANY TU-F ANYMORE          
         B     SMMSPL80                                                         
*                                                                               
* MON-SUN STANDARD WEEK SPLIT WEEK DEFINITION IS M-F ON AND SA/SUN ON           
SMMSPMON TM    DBSELDAY,B'01111100' IF ITS A ROTATION OF ATLEAST 1              
         BZ    SMMSPLTX             DAY BEFORE SATURDAY AND                     
         TM    DBSELDAY,B'00000011' 1 DAY SAT OR SUNDAY                         
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'00000011' IF WE READ SAT - SUN THEN WE HAD             
         BNZ   SMMSPLTX            53 WEEKS ON THE RECORD                       
         NI    DBSELDAY,B'00000011' NO NEED TO LOOKUP ANY M-F ANYMORE           
         NI    SPLKDAY,B'00000011'  NO NEED TO LOOKUP ANY M-F ANYMORE           
         B     SMMSPL80                                                         
*                                                                               
SMMSPL80 LLC   RE,DBSELBK          BUMP TO NEXT YEAR'S 00 RECORD                
         LA    RE,1(RE)                                                         
         STC   RE,DBSELBK                                                       
         MVI   DBSELBK+1,0                                                      
         MVI   NEXTYRLK,C'Y'        SET FLAG TO INDICATE NEXT YR                
*                                                                               
*                                                                               
*                                                                               
SMMSPLTX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* ROUTINE TO DEAL WITH DSTATION CALL LETTER LINK                                
* SPLIT ROTATION SCENARIO WHERE PART OF ROTATION COMES FROM                     
* DIFFERNT STATIONS DEMO RECORDS                                                
*======================================================================         
DSTASPLIT NTR1  BASE=*,LABEL=*                                                  
         CLI   WKINFLAG,C'N'       AND NOT WEEK IN FLAG IS SET                  
         BNE   DSTASPTX                                                         
DSTASSUN DS    0C                   SUN-SA OOWR WEEK                            
         TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    DSTASUN1                                                         
         CLI   ESTEOW,X'07'         SUN-SA OOWR                                 
         BNE   DSTASSAT                                                         
         B     DSTASUN2                                                         
DSTASUN1 CLC   SPOTDAY,=C'SUN'                                                  
         BNE   DSTASSAT                                                         
DSTASUN2 TM    DBSELDAY,B'01111101' IF 1 DAY BETWEEN SUN-FRI                    
         BZ    DSTASPTX             AND                                         
         TM    DBSELDAY,B'00000010' SAT                                         
         BZ    DSTASPTX                                                         
         TM    DAYREAD,B'00000010'  IF SAT READ THEN                            
         BNZ   DSTASPTX             WE HAD 53 WEEKS ON THE FILE                 
         NI    DBSELDAY,B'00000010' NO NEED TO LOOKUP SU-FRI  ANYMORE           
         NI    SPLKDAY,B'00000010'  NO NEED TO LOOKUP SU-FRI  ANYMORE           
         B     DSTA200                                                          
                                                                                
DSTASSAT DS    0C                   SAT-FRI OOWR WEEK                           
         TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    DSTASAT1                                                         
         CLI   ESTEOW,X'06'         SAT TO FRI WEEK HAVE NO SPLIT               
         BE    DSTASPTX                                                         
         B     DSTASFRI                                                         
DSTASAT1 CLC   SPOTDAY,=C'SAT'      SAT HAVE NO SPLIT WEEK EXIT                 
         BE    DSTASPTX                                                         
                                                                                
DSTASFRI DS    0C                   FRI TO THUR OOWR WEEK                       
         TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    DSTAFRI1                                                         
         CLI   ESTEOW,X'05'         FRI TO THU WEEK HAVE NO SPLIT               
         BNE   DSTASTHU                                                         
         B     DSTAFRI2                                                         
DSTAFRI1 CLC   SPOTDAY,=C'FRI'                                                  
         BNE   DSTASTHU                                                         
DSTAFRI2 TM    DBSELDAY,B'00000100' IF ITS A ROTATION WITH FRI                  
         BZ    DSTASPTX             AND                                         
         TM    DBSELDAY,B'01111011' 1 DAY BETWEEN SA-TH                         
         BZ    DSTASPTX                                                         
         TM    DAYREAD,B'01111011'  IF READ 1 DAY BETWEEN SA-TH                 
         BNZ   DSTASPTX             MEANS WE HAD 53 WEEKS ON THE FILE           
DSTAFR20 NI    DBSELDAY,B'01111011' NO NEED TO LOOKUP ANY FRI ANYMORE           
         NI    SPLKDAY,B'01111011'  NO NEED TO LOOKUP ANY FRI ANYMORE           
         B     DSTA200                                                          
                                                                                
DSTASTHU DS    0C                   THU-WED OOWR WEEK                           
         TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    DSTATHU1                                                         
         CLI   ESTEOW,X'04'         THU TO WED WEEK HAVE NO SPLIT               
         BNE   DSTASWED                                                         
         B     DSTATHU2                                                         
DSTATHU1 CLC   SPOTDAY,=C'THU'                                                  
         BNE   DSTASWED                                                         
DSTATHU2 TM    DBSELDAY,B'00001100' IF ITS A ROTATION WITH TH-FRI               
         BZ    DSTASPTX             AND                                         
         TM    DBSELDAY,B'01110011' 1 DAY BETWEEN SA-W                          
         BZ    DSTASPTX                                                         
         TM    DAYREAD,B'01110011'  IF READ 1 DAY BETWEEN SA-WED                
         BNZ   DSTASPTX             MEANS WE HAD 53 WEEKS ON THE FILE           
DSTATH20 NI    DBSELDAY,B'01110011' NO NEED TO LOOKUP ANY TH-F ANYMORE          
         NI    SPLKDAY,B'01110011'  NO NEED TO LOOKUP ANY TH-F ANYMORE          
         B     DSTA200                                                          
                                                                                
DSTASWED DS    0C                   WED-TUE OOWR WEEK                           
         TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    DSTAWED1                                                         
         CLI   ESTEOW,X'03'         WED TO TUE WEEK HAVE NO SPLIT               
         BNE   DSTASTUE                                                         
         B     DSTAWED2                                                         
DSTAWED1 CLC   SPOTDAY,=C'WED'                                                  
         BNE   DSTASTUE                                                         
DSTAWED2 TM    DBSELDAY,B'00011100' IF ITS A ROTATION WITH WE-FRI               
         BZ    DSTASPTX             AND                                         
         TM    DBSELDAY,B'01100011' 1 DAY BETWEEN SA-TU                         
         BZ    DSTASPTX                                                         
         TM    DAYREAD,B'01100011'  IF READ 1 DAY BETWEEN SA-TUE                
         BNZ   DSTASPTX             MEANS WE HAD 53 WEEKS ON THE FILE           
*                                                                               
DSTAWD20 NI    DBSELDAY,B'01100011' NO NEED TO LOOKUP ANY W-F ANYMORE           
         NI    SPLKDAY,B'01100011'  NO NEED TO LOOKUP ANY W-F ANYMORE           
         B     DSTA200                                                          
                                                                                
DSTASTUE DS    0C                                                               
         TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    DSTATUE1                                                         
         CLI   ESTEOW,X'02'         TUE TO MON WEEK HAVE NO SPLIT               
         BNE   DSTASMON                                                         
         B     DSTATUE2                                                         
DSTATUE1 CLC   SPOTDAY,=C'TUE'                                                  
         BNE   DSTASMON                                                         
DSTATUE2 TM    DBSELDAY,B'00111100' IF ITS A ROTATION WITH TU-FRI               
         BZ    DSTASPTX             AND                                         
         TM    DBSELDAY,B'01000011' 1 DAY BETWEEN SA-MO                         
         BZ    DSTASPTX                                                         
         TM    DAYREAD,B'01000011'  IF READ 1 DAY BETWEEN SA-MON                
         BNZ   DSTASPTX             MEANS WE HAD 53 WEEKS ON THE FILE           
DSTATU20 NI    DBSELDAY,B'01000011' NO NEED TO LOOKUP ANY TU-F ANYMORE          
         NI    SPLKDAY,B'01000011'  NO NEED TO LOOKUP ANY TU-F ANYMORE          
         B     DSTA200                                                          
*                                                                               
* MON-SUN STANDARD WEEK                                                         
DSTASMON TM    DBSELDAY,B'01111100' IF ITS A ROTATION OF ATLEAST 1              
         BZ    DSTASPTX             DAY BEFORE SATURDAY AND                     
         TM    DBSELDAY,B'00000011' 1 DAY SAT OR SUNDAY                         
         BZ    DSTASPTX                                                         
         TM    DAYREAD,B'00000011'  IF READ 1 DAY BETWEEN SA-SUN                
         BNZ   DSTASPTX             MEANS WE HAD 53 WEEKS ON THE FILE           
         NI    DBSELDAY,B'00000011' NO NEED TO LOOKUP ANY M-F ANYMORE           
         NI    SPLKDAY,B'00000011'  NO NEED TO LOOKUP ANY M-F ANYMORE           
*                                                                               
DSTA200  MVC   DBSELSTA,DSTACALL                                                
*                                                                               
         MVI   DSTASPLT,C'Y'        INDICATE DSTATION CALL LETTER               
         MVI   NEXTYRLK,C'N'        RESET FLAG                                  
DSTASPTX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* READ USTV MARKET REC FOR DBSELUMK TO GET LPM START DATE                       
*======================================================================         
*                                                                               
GETMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
         MVC   DMCB+12(4),DBAREC                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
*                                                                               
         LA    R9,WORK                                                          
         USING MKTRECD,R9                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVI   MKTKTYPE,C'M'       RECORD TYPE                                  
         MVI   MKTKMED,C'T'        MEDIA                                        
         SR    R0,R0                                                            
         ICM   R0,3,DBSELUMK       MARKET NUMBER                                
         BNZ   GETMKT2                                                          
         XC    SVMKTKEY,SVMKTKEY                                                
         B     GETMKTX                                                          
*                                                                               
GETMKT2  CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,DBSELAGY    AGENCY CODE                                  
*                                                                               
         L     RE,DBCOMFCS                                                      
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         BNZ   GETMKT4                AND NO SAVE AREA AVAILABLE                
*                                                                               
         CLC   SVMKTKEY(8),MKTKEY     TEST SAME MARKET AS PREV                  
         BNE   GETMKT3                                                          
         MVC   LPMDTB,SVLPMDTB                                                  
         MVC   LPMDTP,SVLPMDTP                                                  
         OC    DBSELSYC,DBSELSYC      CABLE?                                    
         BZ    GETMKTX                NO                                        
         MVC   DBSELALF,SVMKALPH      ALPHA MARKET                              
         CLI   SVMKSRC,0              HAVE A SAVED SOURCE FROM CBL?             
         BE    GETMKTX                NO                                        
         MVC   DBSELSRC,SVMKSRC       SOURCE                                    
         B     GETMKTX                                                          
*                                                                               
GETMKT3  MVC   SVMKTKEY,MKTKEY        ELSE SAVE MKTKEY NOW                      
         DROP  R9                                                               
*                                                                               
GETMKT4  XC    LPMDTB,LPMDTB                                                    
         XC    LPMDTP,LPMDTP                                                    
         XC    SVMKALPH,SVMKALPH                                                
         MVI   SVMKSRC,0                                                        
*                                                                               
         GOTO1 (RF),(R1)          AND READ THE RECORD                           
*                                                                               
         L     R9,DBAREC                                                        
         USING MKTRECD,R9                                                       
*                                                                               
         CLC   WORK(8),0(R9)                                                    
         BNE   GETMKTX                                                          
***                                                                             
* SET SVMKALPH AND SVMKSRC IN CASE WE ARE NOT CABLE THIS TIME, BUT              
* THE NEXT TIME AROUND!                                                         
***                                                                             
         MVC   SVMKALPH,MKTALST                                                 
*                                                                               
         CLI   MKTCDEM,C'0'        SUPRESS LOOKUPS FOR THIS MARKET?             
         BE    GETMKT4B            YES                                          
*                                                                               
         ICM   RE,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    GETMKT4A            NO                                           
         USING SPLKXTD,RE                                                       
         TM    SPXTFLAG,SPXTFUS    OVERRIDE TO FUSION                           
         BZ    *+8                 NO                                           
         MVI   MKTCDEM,C'F'                                                     
         TM    SPXTFLAG,SPXTNLS    OVERRIDE TO NIELSON                          
         BZ    *+8                 NO                                           
         MVI   MKTCDEM,C'N'                                                     
         TM    SPXTFLAG,SPXTNONE   OVERRIDE TO NO CABLE                         
         BZ    *+8                 NO                                           
         MVI   MKTCDEM,0                                                        
         TM    SPXTFLAG,SPXTLPM    OVERRIDE TO NIELSON FOR LPM?                 
         BZ    *+8                 NO                                           
         MVI   MKTCDEM,C'N'                                                     
         DROP  RE                                                               
*                                                                               
GETMKT4A CLI   MKTCDEM,C'N'        NSI?                                         
         BE    *+8                 YES, OVERRIDE 00A PROFILE                    
         CLI   MKTCDEM,C'F'        FUSION?                                      
         BNE   *+10                NO, 00A IS DEFAULT                           
GETMKT4B MVC   SVMKSRC,MKTCDEM                                                  
*                                                                               
         OC    DBSELSYC,DBSELSYC   CABLE?                                       
         BZ    GETMKT4C            NO                                           
*                                                                               
         CLI   SVMKSRC,0           OVERRIDE THE SRC FROM MKT OR OPTION?         
         BE    *+10                NO, LEAVE SOURCE ALONE                       
         MVC   DBSELSRC,SVMKSRC    MOVE RATING SERVICE                          
         MVC   DBSELALF,MKTALST    MOVE ALPHA MARKET (NOT MKTAMKTC)             
*                                                                               
GETMKT4C ICM   RE,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    GETMKT5             NO                                           
         USING SPLKXTD,RE                                                       
         TM    SPXTFLAG,SPXTNLPM   SUPRESS LPM START DATE?                      
         BNZ   GETMKTX             YES                                          
         DROP  RE                                                               
*                                                                               
GETMKT5  MVC   LPMDTP,MKTLPMDT     SAVE 2-BYTE PACKED DATE                      
         OC    LPMDTP,LPMDTP       TEST FOR ANY 2-BYTE DATE                     
         BZ    GETMKTX             NONE - EXIT                                  
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,LPMDTP),(3,LPMDTB)                                  
         DROP  R9                                                               
*                                                                               
GETMKTX  MVC   SVLPMDTB,LPMDTB                                                  
         MVC   SVLPMDTP,LPMDTP                                                  
* LOOK FOR DARK WEEK FOR LPM MARKET                                             
*                                                                               
         XC    LPMDRKWK,LPMDRKWK                                                
         OC    LPMDTP,LPMDTP       TEST FOR ANY 2-BYTE DATE                     
         BZ    GETMKTXX            NONE - EXIT                                  
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WKLYLPM                                                
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
*                                                                               
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
         USING WKLYLPMD,RE                                                      
GETMKX10 CLI   0(RE),X'FF'                                                      
         JE    GETMKTXX                                                         
         CLC   SVMKALPH,WKLYAMKT   LOOK FOR MARKET IN TABLE                     
         JE    GETMKX20                                                         
         AR    RE,R0                                                            
         J     GETMKX10                                                         
GETMKX20 MVC   LPMDRKWK,WKLYDARK    SAVE DARK WEEK                              
         DROP  RE                                                               
                                                                                
GETMKTXX DS    0H                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN STORAGE PROTECTION BACK ON              
*                                                                               
         J     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
GETMKTCS CSECT                                                                  
SVMKTKEY DS    CL8                                                              
SVLPMDTB DS    XL3                                                              
SVLPMDTP DS    XL2                                                              
SVMKSRC  DS    CL1                                                              
SVMKALPH DS    CL3                                                              
         EJECT                                                                  
GETDEMO  RSECT                                                                  
*                                                                               
FUSION   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*&&DO                                                                           
         ICM   R3,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    FNX                                                              
         USING SPLKXTD,R3                                                       
*&&                                                                             
         ICM   R8,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    FNX                                                              
         USING SPLKXTD,R8                                                       
*                                                                               
         TM    SPXTFLG2,SPXTSDEM   CALLER SPOT DESKTOP DEMO ENGINE?             
         BZ    *+14                KEEP ORIGINAL SOURCE                         
         MVC   WORK+56(L'DBSELSRC),DBSELSRC                                     
         B     FN06                SKIP READING 00A PROF                        
*                                                                               
         TM    SPXTFLAG,SPXTRAPP   CALLER A RESEARCH APPLICATION ?              
         BNZ   FNX                 YES                                          
         XC    SPXTSYSC,SPXTSYSC   CLEAR THIS!                                  
*                                                                               
         CLI   DBSELMED,C'T'       TEST USTV                                    
         BNE   FNX                 NO                                           
*                                                                               
         XC    WORK+40(32),WORK+40 READ 00A PROFILE                             
         MVC   WORK+40(4),=C'S00A'                                              
         NI    WORK+40,X'BF'       MAKE 'S' LOWERCASE                           
         MVC   WORK+44(2),DBSELAGY                                              
         L     RF,DBCOMFCS                                                      
         L     R0,CDATAMGR-COMFACSD(RF)                                         
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'90',WORK+40),WORK+56,(R0)                           
*                                                                               
         TM    SPXTFLAG,SPXTFUS    OVERRIDE TO FUSION                           
         BZ    *+8                 NO                                           
         MVI   WORK+56,C'F'                                                     
         TM    SPXTFLAG,SPXTNLS    OVERRIDE TO NIELSON                          
         BZ    *+8                 NO                                           
         MVI   WORK+56,C'N'                                                     
         TM    SPXTFLAG,SPXTNONE   OVERRIDE TO NO CABLE                         
         BZ    *+8                 NO                                           
         MVI   WORK+56,0                                                        
         TM    SPXTFLAG,SPXTLPM    OVERRIDE TO NIELSON FOR LPM?                 
         BZ    *+8                 NO                                           
         MVI   WORK+56,C'N'                                                     
*                                                                               
         CLI   WORK+56,C'N'        NSI DEFAULT?                                 
         BE    *+12                YES                                          
         CLI   WORK+56,C'F'        FUSION?                                      
         BNE   FNX                 NO, SUPRESS LOOKUP                           
*                                                                               
FN06     OC    SPXTHEAD,SPXTHEAD   PASSED A HEADEND?                            
         BZ    FNX                                                              
*                                                                               
         PACK  DUB,SPXTHEAD        PACK THE CABLE STATION                       
         CVB   R0,DUB                                                           
         STCM  R0,3,SPXTSYSC       SYSCODE                                      
*                                                                               
         CHI   R0,7000             FOR 7000-7500 CHECK FOR ALTERNATE            
         BL    FN10                SYSCODE FOR DEMOS                            
         CHI   R0,7500                                                          
         BH    FN10                                                             
*                                                                               
         BRAS  RE,GETSYSC          READ CABLE STA MASTER REC                    
         OC    CBLKUP,CBLKUP       TEST FOUND ALT SYSCODE                       
         BZ    FNX                 NO - JUST EXIT                               
         MVC   SPXTSYSC,CBLKUP                                                  
*                                                                               
FN10     XC    WORK(40),WORK                                                    
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'X'        TRANSLATE 3CHAR NET TO 4CHAR                 
         MVC   STAPACOM,DBCOMFCS                                                
         MVC   STAPQNET,SPLKSTA                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   STAPQSTA,C' '       FOUND 4 CHAR NET?                            
         BE    FNX                 NO                                           
         MVC   SPLKSTA(4),STAPQSTA MOVE 4 CHAR NETWORK CODE                     
         MVI   SPLKSTA+4,C'T'                                                   
         DROP  R1                                                               
*                                                                               
         MVC   DBSELSYC,SPXTSYSC   SAVE SYSCODE                                 
*                                                                               
         MVC   DBSELSRC,WORK+56    SAVE SOURCE                                  
*        DROP  R3                                                               
         DROP  R8                                                               
*                                                                               
FNX      J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* READ STATION MASTER FOR ALTERNATE CABLE SYSCODE                               
*============================================================                   
GETSYSC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         L     R4,=A(GETSYSCC)                                                  
         A     R4,RELO                                                          
         USING GETSYSCC,R4                                                      
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
         MVC   DMCB+12(4),DBAREC                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
*                                                                               
         LA    R9,WORK             BUILD KEY OF STATION RECORD                  
         USING STARECD,R9                                                       
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVI   STAKMED,C'T'                                                     
*                                                                               
*        ICM   R3,15,SPLKXTND      POINT TO EXTENSION AREA                      
         ICM   R8,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    GETSYSC4                                                         
*                                                                               
*        USING SPLKXTD,R3                                                       
         USING SPLKXTD,R8                                                       
         MVC   STAKCALL(4),SPXTHEAD                                             
*        DROP  R3                                                               
         DROP  R8                                                               
*                                                                               
         MVI   STAKCALL+4,C'T'     NEED TO APPEND A T                           
         MVC   STAKAGY,DBSELAGY    AGENCY CODE                                  
         MVC   STAKCLT,=C'000'     READ AGENCY RECORD ONLY                      
*                                                                               
         L     RE,DBCOMFCS                                                      
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         BNZ   GETSYSC2            AND NO SAVE AREA AVAILABLE                   
         CLC   SVCBLKEY(12),STAKEY TEST SAME STA/CLT AS PREV                    
         BE    GETSYSC4                                                         
         MVC   SVCBLKEY,STAKEY     ELSE SAVE STA/CLT NOW                        
         DROP  R9                                                               
*                                                                               
GETSYSC2 GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC                                                        
         USING STARECD,R9                                                       
*                                                                               
         CLC   STAKEY(12),WORK     DID WE FIND MASTER RECORD?                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCBLKUP,SCBLLKUP   MOVE CODE                                    
         DROP  R9                                                               
*                                                                               
GETSYSC4 MVC   CBLKUP,SVCBLKUP                                                  
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN STORAGE PROTECTION BACK ON              
*                                                                               
         J     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
GETSYSCC CSECT                                                                  
SVCBLKEY DS    CL12                                                             
SVCBLKUP DS    H                                                                
         EJECT                                                                  
GETDEMO  RSECT                                                                  
*                                                                               
* SETUP VARIOUS PARAMETERS                                                      
SETUP1   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
**  MOVED FROM MAIN CODE ON TOP                                                 
         LA    RE,DEXTRA1                                                       
         MVC   4(4,RE),DBEXTEND                                                 
         STCM  RE,15,DBEXTEND                                                   
****     ST    RE,DBEXTEND         SET AS EXTENSION 1                           
         MVC   0(4,RE),=C'UID '                                                 
         MVC   8(2,RE),SPLKUID     SET USERID                                   
*                                                                               
         ICM   RE,15,SPLKAREC                                                   
         CLC   0(8,RE),=C'DBEXTEND' PUNT - ALLOW APPS TO GET THIS               
         BNE   *+10                        CONTROL TO DEMO SYSTEM               
         MVC   DEXTRA1+4(4),8(RE)          SET IT IF IT BE THERE                
*                                                                               
* 08/06/2018 THIS WILL GO IN 18.3                                               
* COMMENT OUT FOR AUGUST INTERMIM RELEASE OF LIVE+1 CODE                        
         MVI   RMBKFLG,C'N'                                                     
         CLI   SPLKMED,C'R'                                                     
         BNE   SETUP1D                                                          
* RADIO MBK LOOKUP?                                                             
         ICM   RE,15,DBEXTEND                                                   
SETUP1A  BZ    SETUP1D                                                          
         CLC   =C'MBKS',0(RE)                                                   
         BNE   SETUP1B                                                          
         MVI   RMBKFLG,C'Y'                                                     
         ST    RE,SVRMBKEX                                                      
         B     SETUP1D                                                          
SETUP1B  ICM   RE,15,4(RE)                                                      
         B     SETUP1A                                                          
SETUP1D  DS    0H                                                               
*                                                                               
*                                                                               
         ICM   RE,15,SPLKA1W                                                    
         BZ    NO1W                                                             
         MVC   SV1WPROF,0(RE)      SAVE 1W PROFILE VALUES                       
*                                                                               
*                                                                               
         CLI   SPLKMED,C'T'        USTV ALWAYS IMP BASED                        
         BE    *+8                                                              
         CLI   SV1WPROF+5,C'I'     IMP BASED CALCULATED DEMOS                   
         BNE   NO1W                                                             
         MVI   TAPEOPT,C'Y'        CHECK FOR NSI/USTV BELOW                     
NO1W     DS   0H                                                                
********************                                                            
         MVI   TAPEHHSR,C'N'       FORCE RECALC HOMSESHARES                     
         XC    SVSELSYC,SVSELSYC                                                
         XC    SVSELSRC,SVSELSRC                                                
                                                                                
         MVC   DBAREC,SPLKAREC                                                  
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'                                                      
         CLI   SPLKTPTT,C'P'       ALLOW A SPECIFIC TP REQUEST                  
         BNE   *+8                  TO GO THROUGH                               
         MVI   DBTPTT,C'P'         SET FOR 4 WEEK AVG. ONLY                     
*                                                                               
         MVC   ADBUY,SPLKABUY                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         MVC   DBCOMFCS,SPLKAFAC                                                
         MVC   DBFILE,=C'TP '                                                   
         CLI   SPLKFIL,C'T'                                                     
         BE    *+10                                                             
         MVC   DBFILE,=C'PAV'    **********                                     
         CLI   SPLKFIL,C'R'                                                     
         BNE   SETUP2                                                           
         CLI   SPLKSRC,C'H'      IHEART                                         
         BE    *+8                                                              
         CLI   SPLKSRC,C'A'                                                     
         BE    *+8                                                              
         CLI   SPLKSRC,C'T'                                                     
         BNE   *+10                                                             
         MVC   DBFILE,=C'RDP'    **********                                     
         CLI   SPLKMED,C'U'                                                     
         BNE   *+10                                                             
         MVC   DBFILE,=C'RUA'    **********                                     
SETUP2   MVC   DBSELMED,SPLKMED                                                 
         MVC   DBSELSRC,SPLKSRC                                                 
         MVC   DBSELAGY,SPLKAGY                                                 
         MVC   DBSELCLI,SPLKCLI                                                 
         MVC   DBSELUMK,SPLKUMK    USER MKT NUM FOR RTG SVC OVRD                
         MVC   DBSELBK,SPLKDBK                                                  
         MVC   DBBTYPE,SPLKBTYP                                                 
*                                                                               
         L     RF,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RF                                                      
*                                                                               
SETUP05  CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+12                                                             
         MVI   DBBTYPE,0           YES: SUPPRESS INVALID BOOKTYPES              
         B     SETUP10                                                          
*                                                                               
         MVC   BKTYPIND,SPBTYIND   SAVE BOOKTYPE INDICATOR IN TABLE             
         MVC   OVEFFBK,SPOVEFFB                                                 
* PROTECTIVE CODE IN CASE DEMTABS GETS BACKED OUT                               
* COMPARE NEW ELONGATED TABLE LENGTH AGAINST TABLE LENGTH RETURNED              
* BY DEMTABS.  IF NOT EQUAL IGNORE NEW CODE.                                    
* DELETE THIS CODE NEXT TIME TABLE GETS ELONGATED.  DUPLICATE LOGIC             
* NEXT TIME TABLE GETS ELONGATED FOR NEW FIELDS.                                
         LA    R0,SPBKTYLQ                                                      
         CH    R0,BKTYPTBL                                                      
         BE    *+10                                                             
         XC    OVEFFBK,OVEFFBK                                                  
*                                                                               
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BE    SETUP10                                                          
         AH    RF,BKTYPTBL         NO: TRY NEXT                                 
         B     SETUP05                                                          
         DROP  RF                                                               
*                                                                               
* FOR SRC DATA NEED TO CHANGE DBSELSRC                                          
SETUP10  CLI   DBSELMED,C'R'                                                    
         BE    *+20                                                             
         CLI   DBBTYPE,C'S'                                                     
         BNE   *+12                                                             
         MVI   DBSELSRC,C'S'                                                    
         MVI   DBBTYPE,0                                                        
                                                                                
* FOR NETWORK ALLOW ANY BOOK TYPE TO COME IN                                    
                                                                                
         CLI   DBSELMED,C'N'       FOR NETWORK                                  
         BNE   *+10                                                             
         MVC   DBBTYPE,SPLKBTYP    MOVE IN STRAIGHT FROM CALLER                 
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
*======================================================================         
* LPM SUPPORT CODE                                                              
*======================================================================         
*                                                                               
LPM      NTR1  BASE=*,LABEL=*                                                   
         CLI   DBSELMED,C'O'                                                    
         BE    *+8                 NO                                           
         CLI   DBSELMED,C'W'                                                    
         BE    *+8                 NO                                           
         CLI   DBSELMED,C'T'       TEST USTV                                    
         BNE   LPMX                NO                                           
         OC    DBSELSYC,DBSELSYC   TEST CABLE LOOKUP                            
         BNZ   LPM1                YES - GO CHECK MARKET RECORD                 
* THESE ARE ALL THE BOOKTYPES WE NEED TO READ MKT RECORDS FOR                   
* BECAUSE WE NEED IT FOR WEEKLY OR OVN POSTING                                  
* POSSIBLE LPM BOOKTYPES                                                        
         CLI   DBBTYPE,0           STANDARD                                     
         BE    LPM1                YES -                                        
         CLI   DBBTYPE,C'P'        TEST PEOPLE METER SPECIFIED                  
         BE    LPM1                YES -                                        
         CLI   DBBTYPE,C'H'        TEST HISPANIC                                
         BE    LPM1                YES -                                        
         CLI   DBBTYPE,C'G'        TEST GENERAL BOOKTYPE                        
         BE    LPM1                YES - CHECK FOR PEOPLE METER                 
*                                                                               
         L     RE,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RE                                                      
         XC    OVEFFBK,OVEFFBK                                                  
LPM0     CLI   0(RE),X'FF'         EOT?                                         
         BE    LPM1                YES: CHECK FOR LPM                           
         MVC   BKTYPIND,SPBTYIND   SAVE BOOKTYPE INDICATOR IN TABLE             
         MVC   OVEFFBK,SPOVEFFB                                                 
* PROTECTIVE CODE IN CASE DEMTABS GETS BACKED OUT                               
* COMPARE NEW ELONGATED TABLE LENGTH AGAINST TABLE LENGTH RETURNED              
* BY DEMTABS.  IF NOT EQUAL IGNORE NEW CODE.                                    
* DELETE THIS CODE NEXT TIME TABLE GETS ELONGATED.  DUPLICATE LOGIC             
* NEXT TIME TABLE GETS ELONGATED FOR NEW FIELDS.                                
         LA    R0,SPBKTYLQ                                                      
         CH    R0,BKTYPTBL                                                      
         BE    *+10                                                             
         XC    OVEFFBK,OVEFFBK                                                  
*                                                                               
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BNE   LPM0A                                                            
*                                                                               
         TM    SPBTYIND,SPBKWKLY   IF BOOKTYPE IS IN TABLE AND                  
         BO    LPM1                IS DEFINED AS OVN OR WKLY BOOKTYPE           
         TM    SPBTYIND,SPBKOVN    THEN WE NEED MKT REC FOR LPMDATE             
         BO    LPM1                                                             
         B     LPMX                                                             
*                                                                               
LPM0A    AH    RE,BKTYPTBL         NO: TRY NEXT                                 
         B     LPM0                                                             
         DROP  RE                                                               
*                                                                               
LPM1     BRAS  RE,GETMKT           READ MARKET RECORD FOR LPM START             
         CLI   DBSELMED,C'O'       ALL WE NEED IS THE LPM DATE INFO             
         BE    LPM4                FOR OVERNIGHTS                               
         CLI   DBSELMED,C'W'       ALL WE NEED IS THE LPM DATE INFO             
         BE    LPM4                FOR WEEKLY                                   
         OC    LPMDTP,LPMDTP       TEST HAVE LPM START DATE                     
         BZ    LPM4                NO - SO NO PEOPLE METER                      
*                                                                               
         OC    DBSELBK,DBSELBK     TEST BOOK SPECIFIED                          
         BZ    LPM2                NO                                           
         CLC   DBSELBK,LPMDTB      COMPARE BOOK YYMM TO LPM YYMM                
         BL    LPM4                LOW - SO NO PEOPLE METER                     
         B     LPM3                                                             
*                                                                               
LPM2     CLC   BDEND(2),LPMDTB     BUY END YM TO LPM START                      
         BL    LPM4                LOW - SO NO PEOPLE METER                     
*-----------------------------------------------------------------*             
* ONLY IF ARE ARE ASKING FOR LPM BOOKTYPES OR STANDARD SHOULD WE  *             
* OVERRIDE THE BOOKTYPE                                           *             
* CHECK FOR BOOKTYPES THAT COULD BE USED FOR LPM                  *             
* IF WE DONT REGONIZE THEM DONT OVVERIDE                          *             
*-----------------------------------------------------------------*             
LPM3     CLI   DBBTYPE,C'P'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,0                                                        
         BE    *+8                                                              
         CLI   DBBTYPE,C'G'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'H'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BNE   LPMX                                                             
*                                                                               
*                                                                               
         LA    R0,C'P'             SET TO MAKE BOOK A P                         
         CLI   DBBTYPE,C'G'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'P'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,0                                                        
         BNE   *+8                                                              
         OI    BKTYPIND,SPBKWKLY   ENABLE WEEKLY POST FOR BOOKTYPE P            
*                                                                               
         CLI   DBBTYPE,0           ENABLE STANDARD FOR OVERNIGHTS               
         BE    LPM3C                                                            
         CLI   DBBTYPE,C'P'        ENABLE P BOOKTYPE FOR OVERNIGHTS             
         BE    LPM3C                                                            
         CLI   DBBTYPE,C'G'        ENABLE G BOOKTYPE FOR OVERNIGHTS             
         BE    LPM3C                                                            
         CLI   DBBTYPE,C'I'        ENABLE I BOOKTYPE FOR OVERNIGHTS             
         BE    *+8                                                              
         CLI   DBBTYPE,C'H'        UNLESS IT'S AN H                             
         BNE   LPM3X                                                            
         LA    R0,C'I'                                                          
LPM3C    OI    BKTYPIND,SPBKOVN    ENABLE OVR POSTING FOR BOOKTYPE I            
*                                                                               
LPM3X    DS    0C              NO LONGER HAVE PRELIMINARY LPM BOOKTYPES         
****PM3X    STC   R0,DBBTYPE   DO NOT US PRELIM BKTYPE ANYMORE 10/16            
         B     LPMX            IT MESSES SMM POSTING W LPM DATE                 
*                                                                               
*LPM4     ICM   R3,15,SPLKXTND      POINT TO EXTENSION AREA                     
LPM4     ICM   R8,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    LPMX                                                             
*        USING SPLKXTD,R3                                                       
         USING SPLKXTD,R8                                                       
         TM    SPXTFLAG,SPXTLPM    SUPRESS CABLE EXCEPT FOR LPM?                
         BZ    LPMX                NO                                           
         MVC   SVSELSYC,DBSELSYC   SAVE OFF BEFORE CLEARING                     
         MVC   SVSELSRC,DBSELSRC                                                
         XC    DBSELSYC,DBSELSYC   SUPRESS CABLE                                
         MVI   DBSELSRC,0          SUPRESS CABLE                                
*        DROP  R3                                                               
         DROP  R8                                                               
*                                                                               
LPMX     J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET A(STAPACK), A(DEFINE), A(GETIUN) FROM COMFAC IF SET. OTHERWISE  *         
* CALL CALLOV TO GET ADDRESSES AND SET THEM IN COMFAC FOR NEXT TIME   *         
***********************************************************************         
         PRINT GEN                                                              
SETOVERL NTR1  BASE=*,LABEL=*                                                   
         PRINT NOGEN                                                            
* PATCHABLE OPTION FOR TAPE BASED CALCULATIONS                                  
         MVI   TAPEOPT,C'Y'        TAPE BASED DEMO CALCS                        
         MVI   TAPEOPT,C'N'        BOOK BASED DEMO CALCS                        
* PATCHABLE OPTION FOR METERED MARKET WEEKLYS                                   
         MVI   WVPHOPT,C'Y'        MM WEEKLY VPH CALCS                          
         MVI   WVPHOPT,C'P'        MM WEEKLY DEMO CALCS                         
         MVI   WVPHOPT,C'N'        NORMAL LOOKUPS                               
         MVI   OVPHOPT,C'N'        MORMAL LOOKUPS                               
         MVI   LPMLKOPT,C'N'       NORMAL LOOKUPS                               
         MVI   SMMMONTH,C'N'                                                    
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVI   CALLERF,0                                                        
*                                                                               
         MVI   0(R1),0                  CLEAR ERROR BYTE                        
         MVI   SAVER1,X'FF'             SET PARAM LIST STYLE FLAG               
         L     RE,0(R1)                 GET A(BLOCK)                            
         MVC   SPDEMLK(SPDEMLKL),0(RE)  MOVE PARAMS TO WORK AREA                
*                                                                               
         ICM   RF,15,SPLKXTND      POINT TO EXTENSION AREA                      
         BZ    SOV05                                                            
         USING SPLKXTD,RF                                                       
         TM    SPXTFLG2,SPXTOP2I   2 DECIMAL IMPRESSIONS?                       
         JZ    SOV05                                                            
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
         DROP  RF                                                               
*                                                                               
SOV05    TM    SPLKOPT2,SPLKOPT2_LPOST  LPMWK OPTION                            
         BZ    *+8                                                              
         MVI   LPMLKOPT,C'Y'                                                    
         TM    SPLKOPT2,SPLKOPT2_OPOST  DO WE WANT OVERNIGHTS                   
         BZ    *+8                                                              
         MVI   OVPHOPT,C'Y'                                                     
         TM    SPLKOPT2,SPLKOPT2_SMMM   DO WE TO POST MONTHLY                   
         BZ    *+8                      FOR SMM                                 
         MVI   SMMMONTH,C'Y'                                                    
* ADD CODE HERE TO LOOK AT D0 FOR OVERNIGHTS PROFILE                            
* IF OVERNIGHT POSTING IS TURNED ON THEN SET OVPHOPT=Y                          
*                                                                               
         TM    SPLKOPT,SPLKOWTP    MERGE WEEKLY HOMES AND SWEEP DEMS            
         BZ    *+8                                                              
         MVI   WVPHOPT,C'Y'                                                     
*                                                                               
SETOV1A  MVI   TWODEC,C'N'                                                      
         CLI   SPLKMED,C'T'                                                     
         BE    *+8                                                              
         CLI   SPLKMED,C'W'                                                     
         BE    *+8                                                              
         CLI   SPLKMED,C'C'                                                     
         BE    *+8                                                              
         CLI   SPLKMED,C'O'                                                     
         BNE   *+16                                                             
         TM    SPLKOPT,SPLKOP2D    TEST 2-DECIMAL OPTION                        
         BZ    *+8                                                              
         MVI   TWODEC,C'Y'                                                      
*                                                                               
* SET UP FOR DAY OPTION (NSI WEEKLY STARTS ON SAT)                              
         CLI   SPLKMED,C'W'        WEEKLY FILE                                  
         BNE   WKDAYOX                                                          
         CLI   SPLKSRC,C'N'        AND NSI                                      
         BNE   WKDAYOX                                                          
         ICM   RF,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    WKDAYOX             NO                                           
         USING SPLKXTD,RF                                                       
         TM    SPXTFLAG,SPXTRAPP   CALLER A RESEARCH APPLICATION ?              
         BZ    WKDAYOX                                                          
         OI    FLAGS,WTPDAYOP      SET TO INDICATE OPT REQUIRED                 
         DROP  RF                                                               
WKDAYOX  DS    0C                                                               
*                                                                               
         LA    R0,DBLOCK                                                        
         ST    R0,SPLKDBLK-SPDEMLK(RE)  SET DBLOCK ADDRESS                      
         ST    R0,SPLKDBLK              AND SET IT IN MY BLOCK                  
*                                                                               
*                                                                               
         L     RF,SPLKAFAC                                                      
         LR    R2,RF                                                            
*                                                                               
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   RE,15,0(R1)         A(BOOKTYPE TABLE) RETURNED IN P1             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,VBKTYPTB         SAVE A(BOOKTYPE TABLE)                       
         MVC   BKTYPTBL,6(R1)      SAVE L'BOOKTYPE TABLE ENTRY                  
         LR    RF,R2               RESTORE A(COMFACS)                           
*                                                                               
         MVC   VDEFINE,CDEFINE-COMFACSD(RF)                                     
         MVC   VGETIUN,CSPGTIUN-COMFACSD(RF)                                    
*                                                                               
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,0,X'D9000A7A'  STAPACK                                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,0(R1)                                                   
*                                                                               
         XC    VCOMINTR,VCOMINTR                                                
         GOTO1 (RF),DMCB,0,X'D9000A5A'  COMINTER                                
         CLI   4(R1),X'FF'                                                      
***      BNE   *+6                                                              
***      DC    H'0'                                                             
         BE    *+10                                                             
         MVC   VCOMINTR,0(R1)                                                   
*                                                                               
         OC    VDEFINE,VDEFINE          HAVE A(DEFINE)?                         
         BNZ   SOV10                    YES                                     
*                                                                               
         GOTO1 (RF),DMCB,0,X'D9000A26'  DEFINE                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEFINE,0(R1)                                                    
         MVC   CDEFINE-COMFACSD(4,R2),VDEFINE                                   
*                                                                               
SOV10    OC    VGETIUN,VGETIUN          HAVE A(GETIUN)?                         
         BNZ   SOVX                     YES                                     
*                                                                               
         GOTO1 (RF),DMCB,0,X'D9000A24'  GETIUN                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETIUN,0(R1)                                                    
         MVC   CSPGTIUN-COMFACSD(4,R2),VGETIUN                                  
*                                                                               
SOVX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
* SUBROUTINE RETURNS == ADJ == VALUES AND SVIS IN USER AREA *                   
* R1 POINTS TO ADJ VALUES ON ENTRY                          *                   
*************************************************************                   
         SPACE 1                                                                
SETVALS  NTR1  BASE=*,LABEL=*                                                   
         L     R8,SPLKAVAL                                                      
         SR    R5,R5                                                            
         IC    R5,SPLKALST         GET NUMBER OF DEMOS                          
         L     R6,SPLKALST         POINT TO DEMO LIST                           
         LA    R7,SVIS                                                          
*                                                                               
SETVAL2  DS    0H                                                               
*                                                                               
         L     RF,0(R1)                                                         
*                                                                               
         CLC   0(4,R1),=X'FFFFFFFF'  COMSCORE ERROR VALUE                       
         BE    SETVAL2F                                                         
*                                                                               
         AHI   RF,50                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
SETVAL2F ST    RF,0(R8)            RETURN DEFLATED VALUE                        
*                                                                               
         CLI   TWODEC,C'Y'         TEST 2-DECIMAL RATINGS                       
         BNE   SETVAL4             RATING & IMP PREC MUST BE THE SAME           
* IS THIS COMSCORE DEMO                                                         
         CLI   2(R6),0                                                          
         BNE   SETVAL3                                                          
         LR    R0,R1                                                            
         GOTOR ISCOMRAT,DMCB,(R6)                                               
         LR    R1,R0                                                            
         BE    SETVAL3B                                                         
         B     SETVAL3A            COMSCORE IMP DEMO REQUESTED                  
*                                                                               
SETVAL3  CLI   1(R6),C'R'                                                       
         BE    SETVAL3B                                                         
         CLI   1(R6),C'E'                                                       
         BE    SETVAL3B                                                         
         CLI   1(R6),C'I'          IMPRESSIONS?                                 
         BNE   SETVAL4                                                          
SETVAL3A TM    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS?                       
         BZ    SETVAL4                                                          
SETVAL3B OI    0(R8),X'40'         SET 2-DECIMAL VALUE FLAG                     
*                                                                               
SETVAL4  SR    R0,R0                                                            
         ICM   R0,15,0(R7)                                                      
         BNZ   *+8                                                              
         LA    R0,100                                                           
         ST    R0,4(R8)            AND SVI                                      
*                                                                               
         LA    R8,8(R8)            NEXT OUTPUT POSITION                         
         LA    R7,4(R7)            NEXT SVI                                     
         LA    R6,3(R6)                                                         
         LA    R1,4(R1)            NEXT DEMO VALUE                              
         BCT   R5,SETVAL2                                                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SUBROUTINE MOVED HERE FOR ADDRESSABILITY PROBLEMS                             
* ON ENTRY R1 POINTS TO ADCON LIST                                              
*================================================================               
                                                                                
SETADCON NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ADCONS                                                        
*                                                                               
         LR    RE,R1                                                            
         LA    RF,WKADCONS                                                      
         LHI   R0,(ADCONSX-ADCONS)/4                                            
*                                                                               
SETADC2  L     R1,0(RE)                                                         
         AR    R1,RC                                                            
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,SETADC2                                                       
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CSWITCH-COMFACSD(RF) USE SWITCH TO PICK UP V(SSB)             
         LTR   RF,RF                                                            
         BNZ   SETADC4                                                          
         L     RF,DBCOMFCS                                                      
         L     RF,CMASTC-COMFACSD(RF)      RF=AMASTC                            
         ICM   RF,15,MCSSB-MASTD(RF)       RF=ASSB                              
         BNZ   SETADCX                                                          
         DC    H'0'                DON'T HAVE ALET                              
*                                                                               
SETADC4  DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         GOTO1 (RF),DUB                                                         
         ICM   RF,15,0(R1)         RF=V(SYSFACS)                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,VSSB-SYSFACD(RF)                                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETADCX  MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
         MVI   COMPASS,0                                                        
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CMASTC-COMFACSD(RF)      RF=AMASTC                            
         ST    RF,AMASTC                                                        
         ICM   RF,15,MCAEXTRA-MASTD(RF)    EXTRA DATA AREA                      
         BZ    SETADCXX                                                         
         CLI   MCCSPASS-MCEXTRA(RF),MCCSP001                                    
         BNE   *+8                                                              
         MVI   COMPASS,1                                                        
         CLI   MCCSPASS-MCEXTRA(RF),MCCSP002                                    
         BNE   *+8                                                              
         MVI   COMPASS,2                                                        
*                                                                               
SETADCXX XIT1                                                                   
         LTORG                                                                  
ADCONS   DS    0A                                                               
         DC    A(DTUNVS-WORKD)                                                  
         DC    A(DTDEMS-WORKD)                                                  
         DC    A(SVIREC-WORKD)                                                  
         DC    A(IUNWK-WORKD)                                                   
         DC    A(IUNVS-WORKD)                                                   
         DC    A(IUNOLD-WORKD)                                                  
         DC    A(IRTGOLD-WORKD)                                                 
         DC    A(IPUTOLD-WORKD)                                                 
         DC    A(IIMPOLD-WORKD)                                                 
         DC    A(ITOTOLD-WORKD)                                                 
         DC    A(IUNOLDX-WORKD)                                                 
         DC    A(IUNNEW-WORKD)                                                  
         DC    A(IRTGNEW-WORKD)                                                 
         DC    A(IPUTNEW-WORKD)                                                 
         DC    A(IIMPNEW-WORKD)                                                 
         DC    A(ITOTNEW-WORKD)                                                 
         DC    A(IUNNEWX-WORKD)                                                 
         DC    A(IUNXTRA-WORKD)                                                 
         DC    A(DBLOCKS-WORKD)                                                 
         DC    A(SPDTTAB-WORKD)                                                 
         DC    A(GDEXSPDT-WORKD)                                                
         DC    A(DBLOCK2-WORKD)                                                 
         DC    A(COMEXT-WORKD)                                                  
ADCONSX  EQU   *                                                                
         EJECT                                                                  
**************************************************************                  
* DETERMINE IF THIS MARKET IS A TRUE LPM MARKET                                 
**************************************************************                  
* INPUT : STABMKT SET TO MARKET                                                 
*                                                                               
TRUELPM  NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
* REMOVING THIS LOGIC AND REPLACING WITH SEARCH IN NEW DEMTABS TABLE            
* BEN 09/07/2017                                                                
         MVI   TRUELPMF,C'N'                                                    
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,FUSNENDP                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         USING FUSENDPD,RE                                                      
TRUEL10  CLC   =X'FFFF',0(RE)                                                   
         BE    TRUELPMN                                                         
         CLC   STABKMKT,FUSNMKT                                                 
         BE    TRUELPMY                                                         
         AR    RE,RF                                                            
         B     TRUEL10                                                          
TRUELPMY MVI   TRUELPMF,C'Y'                                                    
         J     TRUELPMX                                                         
TRUELPMN DS    0C                                                               
         XC    LPMDTP,LPMDTP                                                    
*&&                                                                             
* NEW CODE. 09/07/2017.                                                         
*                                                                               
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
         MVI   TRUELPMF,C'N'                                                    
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,NSIMKTS                                                
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         USING NMKTD,RE                                                         
TRUEL10  CLI   0(RE),X'FF'                                                      
         BE    TRUELPMN                                                         
         CLC   STABKMKT,NMKTNUM                                                 
         BE    TRUEL20                                                          
         CLC   SVMKALPH,NMKTALF                                                 
         BE    TRUEL20                                                          
         AR    RE,RF                                                            
         B     TRUEL10                                                          
TRUEL20  DS    0C                                                               
         CLI   NMKTTYPE,NMKTLQ                                                  
         BNE   TRUELPMN                                                         
TRUELPMY MVI   TRUELPMF,C'Y'                                                    
         J     TRUELPMX                                                         
TRUELPMN DS    0C                                                               
         XC    LPMDTP,LPMDTP                                                    
TRUELPMX XIT1                                                                   
*                                                                               
**************************************************************                  
**************************************************************                  
* ROUTINE TO SET THE MARKET TYPE                                                
* CALL THIS ROUTINE ONLY TO DETERMINE WHAT TYPE OF NON LPM                      
* MKT WE ARE DEALING WITH.                                                      
* EXIT- MKTTYPE SET TO TYPE OF MARKET  (SET METERED, DIARY)                     
*                                                                               
**************************************************************                  
SETMKTYP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
*                                                                               
         MVC   SVMED,DBSELMED      SAVE AWAY                                    
         MVC   SVDBFUNC,DBFUNCT                                                 
         MVC   SVDBTYPE,DBBTYPE                                                 
         XC    STABKMKT,STABKMKT                                                
         MVC   SVSELSTA,DBSELSTA                                                
         MVC   HALF,DBSELBK                                                     
         MVC   DSTACALL,DBSELSTA                                                
         MVI   MKTTYPE,MKTDIARY    DEFAULT AS DIARY                             
         MVC   SVSELAGY,DBSELAGY                                                
         MVC   DBSELAGY,=C'SJ'     SJR ACCESS TO GET MKT INFO                   
***      MVI   DBSELMED,C'W'                                                    
         MVI   DBSELMED,C'O'                                                    
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
*                                                                               
* SEE IF  WE ARE LOOKING UP LIVE ONLY BOOKTYPE                                  
* IF SO, LOOK UP THE KEYS FOR THE  LIVE PLUS INSTEAD BECAUSE                    
* WE DON T ALWAYS GET LIVE ONLY                                                 
*                                                                               
SETMKT16 L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,LIVEBKTY                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         USING LVBKTYD,RE                                                       
SETMKT18 CLC   =X'FFFF',0(RE)                                                   
         BE    SETMKT21                                                         
         CLC   DBBTYPE,LVONLYBT                                                 
         BE    SETMKT20                                                         
         AR    RE,RF                                                            
         B     SETMKT18                                                         
SETMKT20 MVC   DBBTYPE,LVPLUSBT                                                 
*                                                                               
                                                                                
SETMKT21 MVI   DBFUNCT,DBVLSTBK                                                 
*   CALL DEMAND TO VALIDATE STATION/BOOK                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         MVC   DBFUNCT,SVDBFUNC                                                 
         MVC   DBBTYPE,SVDBTYPE                                                 
         MVC   DBSELSTA,SVSELSTA                                                
*                                                                               
         CLI   DBSELMED,C'T'                                                    
         BNE   SETMKT22                                                         
         MVC   DBSELBK,HALF                                                     
         MVI   DBSELMED,C'W'                                                    
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JE    SETMKT23            IF ERROR JUST EXIT                           
*****    J     SETMKTYX            IF ERROR JUST EXIT                           
* EVEN THOUGH WE CANT FIND THE PASSIVE KEYS TO VALIDATE THIS STATION            
* IN DEMO DIRECTORY THIS MIGHT BE A FAKE LOCAL CABLE CALL LETTER                
* THAT IS NOT ON THE DIRECTORY.  SEARCH DEMTABS TABLE BASED ON                  
* THE SVMKALPH IF IT IS SET                                                     
         CLC   SVMKALPH,=X'404040' TRY USING THE ALPHA MARKET SAVED             
         BNH   SETMKTYX            IF NO SVMKALPH THEN EXIT                     
         B     SETMKT25                                                         
*                                                                               
SETMKT22 CLI   DBERROR,0                                                        
         BE    SETMKT23                                                         
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELBK,SVSELBK                                                  
*******  J     SETMKT21                                                         
         J     SETMKT16                                                         
*                                                                               
*                                                                               
SETMKT23 MVC   DSTACALL,DBKEY+(BSSTAT-BSKEY)    STATION READ                    
         MVC   STABKMKT,DBKEY+(BSRMKT-BSKEY)   MKT NUMBER                       
         CLC   DSTACALL,DBSELSTA      SET DBSELSTA TO                           
         BE    *+16                   THE LINKED STATION FOR THIS BOOK          
         MVC   DBSELSTA,DSTACALL                                                
         MVC   DSTACALL,SVSELSTA                                                
*                                                                               
* IF CALL LETTERS ARE THE SAME DSTACALL=DBSELSTA                                
* THEN TRY ALSO TO READ FOR THE WEEK AFTER (SPLIT WEEK)                         
* SAT/SUN IS A WEEK AHEAD ON THE DEMOS FILE                                     
* IN ORDER TO SEE IF THERE IS A DSTATION LINK SET UP                            
*                                                                               
         CLC   DSTACALL,DBSELSTA                                                
         BNE   SETMKT25                                                         
         TM    DBSELDAY,B'01111100' IF ITS A ROTATION OF ATLEAST 1              
         BZ    SETMKT25             DAY BEFORE SATURDAY AND                     
         TM    DBSELDAY,B'00000011' 1 DAY SAT OR SUNDAY                         
         BZ    SETMKT25                                                         
         OC    VHSDAT,VHSDAT                                                    
         BZ    SETMKT25                                                         
         MVC   HALF,DBSELBK                     SAVE BOOK                       
***                                                                             
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VHSDAT),(0,DUB3)                                    
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB3,DUB3,7                                            
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
***                                                                             
         MVI   DBFUNCT,DBVLSTBK                                                 
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         MVC   DBBTYPE,SVDBTYPE                                                 
         MVC   DBSELSTA,SVSELSTA                                                
         MVC   DBFUNCT,SVDBFUNC                                                 
         MVC   DBSELBK,HALF                     RESTORE BOOK                    
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   SETMKT25                                                         
         CLC   DSTACALL,DBKEY+(BSSTAT-BSKEY)                                    
         BE    *+10                                                             
         MVC   DSTACALL,DBKEY+(BSSTAT-BSKEY)    STATION READ                    
*                                                                               
*                                                                               
* GET SET METER MKT TABLE FROM DEDEMTABS                                        
SETMKT25 L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,SETMETER                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         MVC   DBSELMED,SVMED      RESTORE TO WHATEVER IT WAS                   
*                                                                               
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
         USING SETMETRD,RE                                                      
SETMKT26 CLI   0(RE),X'FF'              IN TABLE                                
         BE    SETMKTYX                 IF NOT IN LPM TABLE EXIT                
         CLC   SETMKNUM,STABKMKT                                                
         BE    SETMKT28                                                         
         CLC   SETMKALF,SVMKALPH                                                
         BE    SETMKT28                                                         
         AR    RE,R0                                                            
         B     SETMKT26                                                         
*                                                                               
SETMKT28 MVI   MKTTYPE,MKTSETM      FOUND MKT IN SET METER TAB                  
*                                                                               
SETMKTYX XC    DBKEY,DBKEY                                                      
         MVI   DBERROR,0                                                        
         MVC   DBSELAGY,SVSELAGY                                                
         MVC   DBSELMED,SVMED      RESTORE TO WHATEVER IT WAS                   
         DROP  RE                                                               
         XIT1                                                                   
         LTORG                                                                  
************************************************************                    
* GET VPHSWK- LOGIC WE USE TO DETERMINE WEEKLY BOOK        *                    
************************************************************                    
GETVHSWK NTR1  BASE=*,LABEL=*                                                   
         XC    VHSDAT(4),VHSDAT                                                 
         CLC   =X'0000',AFDATE     ANY AFFIDS                                   
         BE    GETVHSK2                                                         
         MVC   VHSDAT,AFDATE       YES - USE THEM                               
         B     *+10                                                             
GETVHSK2 MVC   VHSDAT(4),SPLKAFST  NO - USE REQUEST DATES                       
*                                                                               
         CLC   VHSDAT,=X'0000'     WE NEED DATES HERE                           
         JE    EXIT                                                             
         CLC   VHEDAT,=X'0000'                                                  
         BNE   *+10                                                             
         MVC   VHEDAT,VHSDAT                                                    
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VHSDAT),(0,DUB)                                     
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DUB,SPOTDAY                                            
         MVC   BISPTDAY,0(R1)                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
*                                                                               
*                                                                               
         CLI   DBSELMED,C'O'      START DAY=MONDAY                              
         BNE   *+8                                                              
         MVI   DMCB+4,1                                                         
*                                                                               
         GOTO1 VNSIWEEK,DMCB,DUB                                                
         MVC   VPHSWK(1),DMCB+4                                                 
         MVC   VPHSWK+1(1),DMCB                                                 
*                                                                               
* OOWR ROTATION CROSS WEEK SCNEARIO                                             
* WE HAVE TO SEE IF WE HAVE TO TELL GETTP TO SPLIT THE WEEK                     
* AND HOW TO SPLIT THE WEEK                                                     
*                                                                               
         MVI   DBBEST,0                                                         
         CLI   AFDSW,C'Y'         TEST AFFIDS LOOKED UP                         
         BE    GETVHSWX                                                         
*                                                                               
*                                                                               
         CLI   DBSELMED,C'O'                                                    
         BNE   GETVHSWX                                                         
*                                                                               
GETVHSUN TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    GETVSUN1                                                         
         CLI   ESTEOW,X'07'       SUNDAY -SAT WEEK                              
         BNE   GETVHSAT                                                         
         MVI   DBBEST,X'16'       M-SAT NEXT WEEK SPLIT                         
         CLI   BISPTDAY,X'06'     M-SAT SUBTRACT 1 WEEK BACK                    
         BNH   GETVHSUB                                                         
         B     GETVHSWX                                                         
GETVSUN1 CLC   SPOTDAY,=C'SUN'                                                  
         BNE   GETVHSAT                                                         
         MVI   DBBEST,X'16'       M-SAT NEXT WEEK SPLIT                         
         B     GETVHSWX                                                         
                                                                                
GETVHSAT TM    BDSTAT2,BDSDSKDQ   DAILY ON IN THE BUY                           
         BZ    GETVSAT1                                                         
         CLI   ESTEOW,X'06'       SAT-FRI WEEK                                  
         BNE   GETVHFRI                                                         
         MVI   DBBEST,X'15'       M-FRI NEXT WEEK SPLIT                         
         CLI   BISPTDAY,X'05'     M-FRI SUBTRACT 1 WEEK BACK                    
         BNH   GETVHSUB                                                         
         B     GETVHSWX                                                         
GETVSAT1 CLC   SPOTDAY,=C'SAT'                                                  
         BNE   GETVHFRI                                                         
         MVI   DBBEST,X'15'       M-FRI NEXT WEEK SPLIT                         
         B     GETVHSWX                                                         
                                                                                
GETVHFRI TM    BDSTAT2,BDSDSKDQ   DAILY ON IN THE BUY                           
         BZ    GETVFRI1                                                         
         CLI   ESTEOW,X'05'       FRI-THU WEEK                                  
         BNE   GETVHTHU                                                         
         MVI   DBBEST,X'14'       M-THU NEXT WEEK SPLIT                         
         CLI   BISPTDAY,X'04'     M-THU SUBTRACT 1 WEEK BACK                    
         BNH   GETVHSUB                                                         
         B     GETVHSWX                                                         
GETVFRI1 CLC   SPOTDAY,=C'FRI'                                                  
         BNE   GETVHTHU                                                         
         MVI   DBBEST,X'14'       M-THU NEXT WEEK SPLIT                         
         B     GETVHSWX                                                         
                                                                                
GETVHTHU TM    BDSTAT2,BDSDSKDQ   DAILY ON IN THE BUY                           
         BZ    GETVTHU1                                                         
         CLI   ESTEOW,X'04'       THU-WED WEEK                                  
         BNE   GETVHWED                                                         
         MVI   DBBEST,X'13'       M-W NEXT WEEK SPLIT                           
         CLI   BISPTDAY,X'03'     M-WED SUBTRACT 1 WEEK BACK                    
         BNH   GETVHSUB                                                         
         B     GETVHSWX                                                         
GETVTHU1 CLC   SPOTDAY,=C'THU'                                                  
         BNE   GETVHWED                                                         
         MVI   DBBEST,X'13'       M-W NEXT WEEK SPLIT                           
         B     GETVHSWX                                                         
                                                                                
GETVHWED TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    GETVWED1                                                         
         CLI   ESTEOW,X'03'       WED-TUE WEEK                                  
         BNE   GETVHTUE                                                         
         MVI   DBBEST,X'12'       M-W TUE WEEK SPLIT                            
         CLI   BISPTDAY,X'02'     M-TUE SUBTRACT 1 WEEK BACK                    
         BNH   GETVHSUB                                                         
         B     GETVHSWX                                                         
GETVWED1 CLC   SPOTDAY,=C'WED'                                                  
         BNE   GETVHTUE                                                         
         MVI   DBBEST,X'12'       M-W TUE WEEK SPLIT                            
         B     GETVHSWX                                                         
                                                                                
GETVHTUE TM    BDSTAT2,BDSDSKDQ   DAILY ON IN BUY?                              
         BZ    GETVTUE1                                                         
         CLI   ESTEOW,X'02'       TUE-MONDAY WEEK                               
         BNE   GETVHMON                                                         
         MVI   DBBEST,X'10'       MON TUE WEEK SPLIT                            
         CLI   BISPTDAY,X'01'     MON SUBTRACT 1 WEEK BACK                      
         BE    GETVHSUB                                                         
         B     GETVHSWX                                                         
GETVTUE1 CLC   SPOTDAY,=C'TUE'                                                  
         BNE   GETVHSWX                                                         
         MVI   DBBEST,X'10'       MON TUE WEEK SPLIT                            
         B     GETVHSWX                                                         
*                                                                               
GETVHMON DS    0H                 M-SU STANDARD WEEK                            
         B     GETVHSWX           M-SU NO SPLIT                                 
***                                                                             
GETVHSUB DS    0C                                                               
*         LLC   RE,DBSELBK+1          SUBTRACT A WEEK BACK                      
*         SHI   RE,1                                                            
*         STC   RE,DBSELBK+1                                                    
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         XC    DUB3,DUB3                                                        
         GOTO1 (RF),DMCB,(2,VHSDAT),(0,DUB3)                                    
         L     RF,DBCOMFCS                    ADJUST A WEEK BACK                
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB3,DUB3,-7                                           
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
GETVHSWX J     EXIT                                                             
         LTORG                                                                  
**************************************************************                  
* ADJUST VPH TO WEEKLY HOMES                                                    
**************************************************************                  
GETVHNTR NTR1  BASE=*,LABEL=*                                                   
         XC    VHSDAT(4),VHSDAT                                                 
         CLC   =X'0000',AFDATE     ANY AFFIDS                                   
         BE    GETVADTE                                                         
         MVC   VHSDAT,AFDATE       YES - USE THEM                               
         B     *+10                                                             
GETVADTE MVC   VHSDAT(4),SPLKAFST  NO - USE REQUEST DATES                       
*                                                                               
         CLC   VHSDAT,=X'0000'     WE NEED DATES HERE                           
         JE    EXIT                                                             
         CLC   VHEDAT,=X'0000'                                                  
         BNE   *+10                                                             
         MVC   VHEDAT,VHSDAT                                                    
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
*        GOTO1 (RF),DMCB,(2,SPLKAFST),(0,DUB)                                   
         GOTO1 (RF),DMCB,(2,VHSDAT),(0,DUB)                                     
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 VNSIWEEK,DMCB,DUB                                                
         MVC   VPHSWK(1),DMCB+4                                                 
         MVC   VPHSWK+1(1),DMCB                                                 
***************** DAILY BUY SCENARIO  *****************************             
* CHECK SPLKAFST TO SEE WHAT DAY OF THE WEEK THE DATE FALLS UNDER               
* THE WEEK WE SHOULD LOOK UP SHOULD BE THE WEEK THE ROTATION STARTS             
* SO WE HAVE TO CHECK OOWR'S AND WHICH DAY THE SPOT DAY IS ON                   
*********************************************************************           
         CLI   AFDSW,C'Y'      ONLY DO THIS FOR ACHIEVED NOT AFFID              
         BE    CHKDLY50        LOOKUPS                                          
         TM    CALLERF,SPXTSPWR  NON POSTING CALLER SKIP NEW                    
         BZ    CHKDLY50          POSTING CODE                                   
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DUB,SPOTDAY                                            
         MVC   BISPTDAY,0(R1)                                                   
         TM    BDSTAT2,BDSDSKDQ DAILY ON IN BUY?                                
         BZ    CHKDLY50                                                         
CHKDLSUN CLI   ESTEOW,X'07'    SUNDAY-SAT WEEK                                  
         BNE   CHKDLSAT                                                         
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT                           
         BE    CHKDLSUB                                                         
         B     CHKDLY50                                                         
CHKDLSAT CLI   ESTEOW,X'06'    SAT-FRI WEEK                                     
         BNE   CHKDLFRI                                                         
CHKDLFRI CLI   ESTEOW,X'05'    FRI-THU WEEK                                     
         BNE   CHKDLTHU                                                         
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   CHKDLSUB                SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'04'          IF SPOTDAY=M-THU                         
         BNH   CHKDLSUB                SUBTRACT A WEEK BACK                     
CHKDLTHU CLI   ESTEOW,X'04'    THU-WED WEEK                                     
         BNE   CHKDLWED                                                         
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   CHKDLSUB                SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'03'          IF SPOTDAY=M-WED                         
         BNH   CHKDLSUB                SUBTRACT A WEEK BACK                     
CHKDLWED CLI   ESTEOW,X'03'    WED-TUE WEEK                                     
         BNE   CHKDLTUE                                                         
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   CHKDLSUB                SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'02'          IF SPOTDAY=M-TUE                         
         BNH   CHKDLSUB                SUBTRACT A WEEK BACK                     
CHKDLTUE CLI   ESTEOW,X'02'    TUE-MON WEEK                                     
         BNE   CHKDLMON                                                         
         CLI   BISPTDAY,X'06'          IF SPOTDAY=SAT-SUN                       
         BNL   CHKDLSUB                SUBTRACT A WEEK BACK                     
         CLI   BISPTDAY,X'01'          IF SPOTDAY=MON                           
         BE    CHKDLSUB                SUBTRACT A WEEK BACK                     
                                                                                
*                                                                               
*   MON-SUN STANDARD WEEK                                                       
CHKDLMON CLI   0(R1),7         SUNDAY SPOT DAY ?                                
         BE    *+12                                                             
         CLI   0(R1),6         SAT SPOT DAY ?                                   
         BNE   CHKDLY50                                                         
         CLI   SPLKDAY,X'03'   IS THE ROTATION STARTED BEFORE SATURDAY?         
         BNH   CHKDLY50                                                         
         B     CHKDLSUB                                                         
*                                                                               
CHKDLSUB LLC   RE,VPHSWK+1     READJUST BOOK BACK TO THE WEEK OF                
         SHI   RE,1            THE START OF THE ROTATION                        
         STC   RE,VPHSWK+1                                                      
*  IF THE WEEK NUMBER IS ZERO WE NEED TO REREAD LAST YRS 00 REC                 
         CLI   VPHSWK+1,0      DO WE NEED TO EXIT AND REREAD FOR LAST           
         BNE   CHKDLY50        YRS RECORD?                                      
         CLI   LASTYRLK,C'Y'   DID WE ALREADY EXITED AND REREAD LAST            
         BNE   CHKDLY25        YEARS 00 REC?                                    
*                                                                               
* WE HAVE ALREADY EXITED AND NOW HAVE LAST YRS 00 REC                           
* FIGURE OUT THE LAST WEEK OF THE YEAR BY SUBTRACTING 7 DAYS                    
* FROM THE SPOT                                                                 
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         XC    DUB3,DUB3                                                        
         GOTO1 (RF),DMCB,(2,VHSDAT),(0,DUB3)                                    
         L     RF,DBCOMFCS                    ADJUST A WEEK BACK                
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB3,DUB3,-7                                           
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   VPHSWK(1),DMCB+4                                                 
         MVC   VPHSWK+1(1),DMCB                                                 
         B     CHKDLY50                                                         
*                                                                               
* WE NEED TO SET FLAG AND EXIT TO READ LAST YRS 00 REC                          
CHKDLY25 DS    0H                                                               
         J     EXIT                                                             
CHKDLY50 DS    0H                                                               
**********************************************************************          
*                                                                               
*                                                                               
         L     RF,WORK+8                                                        
*        GOTO1 (RF),DMCB,(2,SPLKAFND),(0,DUB)                                   
         GOTO1 (RF),DMCB,(2,VHEDAT),(0,DUB)                                     
         MVC   DMCB+4(12),WORK                                                  
         GOTO1 VNSIWEEK,DMCB,DUB                                                
         PRINT NOGEN                                                            
         MVC   VPHEWK(1),DMCB+4                                                 
         MVC   VPHEWK+1(1),DMCB                                                 
         CLC   VPHSWK,=X'6635'                                                  
         BNE   *+10                                                             
         MVC   VPHSWK,=X'6701'                                                  
*                                                                               
         MVC   VPHEWK(2),VPHSWK                                                 
*                                                                               
* CHECK FOR IN SWEEP PERIOD                                                     
*                                                                               
         LA    RF,SWPWKTAB                                                      
CHKSWK   CLI   0(RF),X'FF'                                                      
         BE    CHKSWKX                                                          
         CLC   SPLKAGY,=C'FM'      FORCE WTP LOOKUP IN SWEEP                    
         BE    CHKSWKX              FOR MPG                                     
         CLC   SVSELBK(2),0(RF)    FIND THE BASE SWEEP                          
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     CHKSWK                                                           
         CLC   VPHSWK(1),0(RF)     SAME YEAR                                    
         BNE   CHKSWKX                                                          
         CLC   VPHSWK+1(1),2(RF)    BOOK START LT CURR WEEK                     
         BL    CHKSWKX              OK TO USE                                   
         CLC   VPHSWK+1(1),3(RF)    CURR WEEK GT BOOK END                       
         BH    CHKSWKX             OK TO USE                                    
         XC    VPHHOMES,VPHHOMES   IN SWEEP - KILL IT                           
         J     EXIT                                                             
CHKSWKX  DS    0C                                                               
*                                                                               
         L     R1,DBAQUART                                                      
         LLC   RF,VPHSWK+1                                                      
         LLC   R0,VPHEWK+1                                                      
         SR    R0,RF               SAVE NUMBER OF WEEKS                         
         BP    *+8                                                              
         LA    R0,0                                                             
         AHI   R0,1                                                             
         ST    R0,VPNWKS                                                        
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         CLI   0(R1),X'51'                                                      
         BE    *+14                                                             
         XC    VPHHOMES,VPHHOMES   NO VALUES- KILL IT                           
         J     EXIT                                                             
*                                                                               
         LLC   R5,2(R1)            SET FIELD WIDTH                              
         SR    R4,R4                                                            
         IC    R4,VPHMASK-1(R5)    SET UP ICM MASK                              
         STC   R4,MASK51                                                        
         LLC   RF,1(R1)            SET ELEMENT END                              
         AR    RF,R1                                                            
         ST    RF,END51                                                         
         STH   R5,P6               SAVE FIELD LENGTH                            
         CLI   0(RF),X'53'         SET UP FOR UNIVERSE ELEMENT                  
         BE    *+14                                                             
         XC    VPHHOMES,VPHHOMES   NO VALUES- KILL IT                           
         J     EXIT                                                             
         ST    RF,ELEM53                                                        
*        LR    R3,RF                                                            
*        LLC   R5,2(R3)            SET FIELD WIDTH                              
         LR    R8,RF                                                            
         LLC   R5,2(R8)            SET FIELD WIDTH                              
         SR    R4,R4                                                            
         IC    R4,VPHMASK-1(R5)    SET UP ICM MASK                              
         STC   R4,MASK53           SAVE MASK                                    
         STH   R5,P6+2             AND FIELD LENGTH                             
*                                                                               
         LLC   RF,VPHSWK+1         ADJUST TO START WEEK                         
                                                                                
         MVI   QHRDARK,C'N'                                                     
*                                                                               
         CLI   NEXTYRLK,C'N'       DOING A FORCED NEXT YR LOOKUP FOR            
         BE    GETVHOM0            END OF YR SPLIT WEEK SITUATION?              
         LA    RF,1                1ST WEEK OF THE YEAR LOOKUP                  
         MVC   VPHSWK,DBSELBK      SPLKDAY IS ALWAYS SAT/SUN OR BOTH            
         MVI   VPHSWK+1,1          FPR NEXTYRLK LOOKUPS                         
*                                                                               
GETVHOM0 MVI   NXTWKFLG,C'N'       SET FLAG WE ARE PROCESSING NEXT WEEK         
         CLI   ESTEOW,X'01'        OOWR OTHER THAN M-SU WEEK                    
         BH    *+20                                                             
         CLI   DSTASPLT,C'Y'       DSTATION CALL LETTER LINK SPLIT              
         BE    *+12                WEEK SITUATION                               
         TM    SPLKDAY,B'01111100'                                              
         BZ    NOROT                                                            
         CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         BE    NOROT                                                            
         L     RE,DBAQUART                                                      
         CLI   2(RE),X'95'         M-F ROTATION ON 00 REC                       
         JE    NOROT                                                            
*                                                                               
         CLI   NEXTYRLK,C'Y'                                                    
         BE    NOROT                                                            
*                                                                               
         TM    CALLERF,SPXTSPWR    NON POSTING SKIP NEW POSTING CODE            
         BZ    MONROT              ASSUME ALWAYS MONDAY-FRI                     
*                                                                               
* SPECIAL OOWR CASES WHERE WE ARE THE START DAY OF OOWR IS NOT MONDAY           
* WE HAVE TO FIGURE OUT THE CURRENT WEEK TO READ FOR CURRENT DBAQUART           
* SUN SPOT DAY.  OOWR CODE. SUN START DAY OF WEEK OF OOWR                       
SUNROT   TM    BDSTAT2,BDSDSKDQ    DAILY ON IN BUY?                             
         BZ    SUNROT1             IF DAILY BUY WE TO CHECK ESTEOW TO           
         CLI   ESTEOW,X'07'        DETERMINE THE OOWR AND THE BOOK IS           
         BNE   SATROT              ALREADY ADJUSTED TO THE BEGINNING            
         B     SUNROT2             OF THE ROTATION SPOTDAY FOR DAILY            
*                                  IS THE EXACT DATE OF DAILY SPOT              
SUNROT1  CLC   SPOTDAY,=C'SUN'     NON DAILY BUYS WE HAVE TO CHECK              
         BNE   SATROT              SPOTDAY BECAUSE VPHSDAT COMING               
*                                  IN IS ALREADY ADJUSTED TO EXACT DAY          
*                                  ROTATION STARTS                              
SUNROT2  CLI   2(RE),X'70'         SUNDAY-READ  CURRENT SUNDAY                  
         BE    NOROT                                                            
         CLI   2(RE),X'60'         SAT READ NEXT WEEKS'S SAT                    
         BNE   NOROT                                                            
         MVI   NXTWKFLG,C'Y'       SET FLAG WE ARE PROCESSING NEXT WEEK         
         B     NEXTWK                                                           
                                                                                
SATROT   TM    BDSTAT2,BDSDSKDQ    DAILY ON IN BUY?                             
         BZ    SATROT1             IF DAILY BUY WE TO CHECK ESTEOW TO           
         CLI   ESTEOW,X'06'        DETERMINE THE OOWR AND THE BOOK IS           
         BE    NOROT               ALREADY ADJUSTED TO THE BEGINNING            
         BNE   FRIROT              OF THE ROTATION SPOTDAY FOR DAILY            
*                                  IS THE EXACT DATE OF DAILY SPOT              
SATROT1  CLC   SPOTDAY,=C'SAT'     SAT START DAY- SAT/SUN READ CURRENT          
         BE    NOROT               WEEK                                         
         BNE   FRIROT                                                           
                                                                                
FRIROT   TM    BDSTAT2,BDSDSKDQ    DAILY ON IN BUY?                             
         BZ    FRIROT1             IF DAILY BUY WE TO CHECK ESTEOW TO           
         CLI   ESTEOW,X'05'        DETERMINE THE OOWR AND THE BOOK IS           
         BNE   THUROT              ALREADY ADJUSTED TO THE BEGINNING            
         B     FRIROT2             OF THE ROTATION SPOTDAY FOR DAILY            
*                                  IS THE EXACT DATE OF DAILY SPOT              
FRIROT1  CLC   SPOTDAY,=C'FRI'     FRI START DAY                                
         BNE   THUROT                                                           
FRIROT2  CLI   2(RE),X'60'          SAT-SUN READ NEXT WEEK'S SAT/SUN            
         BNL   MONROT                                                           
         CLI   2(RE),X'40'          M-TH READ NEXT WEEK'S M-TH                  
         BH    NOROT                                                            
         MVI   NXTWKFLG,C'Y'       SET FLAG WE ARE PROCESSING NEXT WEEK         
         B     NEXTWK              FRI SAME CURRENT WEEK                        
*                                                                               
THUROT   TM    BDSTAT2,BDSDSKDQ    DAILY ON IN BUY?                             
         BZ    THUROT1             IF DAILY BUY WE TO CHECK ESTEOW TO           
         CLI   ESTEOW,X'04'        DETERMINE THE OOWR AND THE BOOK IS           
         BNE   WEDROT              ALREADY ADJUSTED TO THE BEGINNING            
         B     THUROT2             OF THE ROTATION SPOTDAY FOR DAILY            
*                                  IS THE EXACT DATE OF DAILY SPOT              
THUROT1  CLC   SPOTDAY,=C'THU'     THU START DAY                                
         BNE   WEDROT                                                           
THUROT2  CLI   2(RE),X'60'          SAT-SUN READ NEXT WEEK'S SAT/SUN            
         BNL   MONROT                                                           
         CLI   2(RE),X'30'          M-TH READ NEXT WEEK'S M-W                   
         BH    NOROT                                                            
         MVI   NXTWKFLG,C'Y'       SET FLAG WE ARE PROCESSING NEXT WEEK         
         B     NEXTWK              THU/FRI SAME CURRENT WEEK                    
*                                                                               
WEDROT   TM    BDSTAT2,BDSDSKDQ    DAILY ON IN BUY?                             
         BZ    WEDROT1             IF DAILY BUY WE TO CHECK ESTEOW TO           
         CLI   ESTEOW,X'03'        DETERMINE THE OOWR AND THE BOOK IS           
         BNE   TUEROT              ALREADY ADJUSTED TO THE BEGINNING            
         B     WEDROT2             OF THE ROTATION SPOTDAY FOR DAILY            
*                                  IS THE EXACT DATE OF DAILY SPOT              
WEDROT1  CLC   SPOTDAY,=C'WED'     WED START DAY                                
         BNE   TUEROT                                                           
WEDROT2  CLI   2(RE),X'60'          SAT-SUN READ NEXT WEEK'S SAT/SUN            
         BNL   MONROT                                                           
         CLI   2(RE),X'20'          M-TH READ NEXT WEEK'S M-TU                  
         BH    NOROT                                                            
         MVI   NXTWKFLG,C'Y'       SET FLAG WE ARE PROCESSING NEXT WEEK         
         B     NEXTWK              WED-FRI SAME CURRENT WEEK                    
*                                                                               
*                                                                               
TUEROT   TM    BDSTAT2,BDSDSKDQ    DAILY ON IN BUY?                             
         BZ    TUEROT1             IF DAILY BUY WE TO CHECK ESTEOW TO           
         CLI   ESTEOW,X'03'        DETERMINE THE OOWR AND THE BOOK IS           
         BNE   MONROT              ALREADY ADJUSTED TO THE BEGINNING            
         B     TUEROT2             OF THE ROTATION SPOTDAY FOR DAILY            
*                                  IS THE EXACT DATE OF DAILY SPOT              
TUEROT1  CLC   SPOTDAY,=C'TUE'     TUE START DAY                                
         BNE   MONROT                                                           
TUEROT2  CLI   2(RE),X'60'          SAT-SUN READ NEXT WEEK'S SAT/SUN            
         BNL   MONROT                                                           
         CLI   2(RE),X'10'          MON READ NEXT WEEK'S MON                    
         BH    NOROT                                                            
         MVI   NXTWKFLG,C'Y'       SET FLAG WE ARE PROCESSING NEXT WEEK         
         B     NEXTWK              TUE-FRI SAME CURRENT WEEK                    
*                                                                               
*                                  FILE WEEKS SAT/SUN -READ CURRENT             
*                                                                               
MONROT   CLI   2(RE),X'50'         DEMO FILE DAYTIME                            
         JH    *+6                 HIGHER THAN FRIDAY                           
NOROT    BCTR  RF,0                                                             
NEXTWK   STC   RF,BYTE                                                          
         MH    RF,P6                                                            
*                                                                               
* CHECK IF WE ARE DEALING WITH DARKWEEK                                         
         CLC   LPMDRKWK(1),VPHSWK                                               
         BNE   NOTDARKW                                                         
         LLC   R4,BYTE                                                          
         AHI   R4,1                                                             
         STC   R4,BYTE                                                          
         CLC   BYTE,LPMDRKWK+1    DARK WEEK?                                    
         BNE   NOTDARKW                                                         
         MVI   QHRDARK,C'Y'                                                     
NOTDARKW DS    0C                                                               
*                                                                               
         LA    RF,3(RF)            GET PAST HEADER INFO                         
         AR    R1,RF               ADDR OF DMA IMP DATA                         
         SR    RF,RF                                                            
         LLC   RF,VPHSWK+1         ADJUST TO START WEEK                         
         CLI   DSTASPLT,C'Y'       DSTATION CALL LETTER LINK SPLIT              
         BE    *+12                WEEK SITUATION                               
         TM    SPLKDAY,B'01111100'                                              
         BZ    NOROT2                                                           
         L     RE,DBAQUART                                                      
         CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         BE    NOROT2                                                           
         CLI   2(RE),X'95'         M-F ROTATION ON 00 REC                       
         JE    NOROT2                                                           
         CLI   NXTWKFLG,C'Y'                                                    
         BE    NEXTWK2                                                          
         CLI   NEXTYRLK,C'Y'                                                    
         BE    NEXTWK2                                                          
         CLI   2(RE),X'50'         DEMO FILE DAYTIME                            
         JH    *+6                 HIGHER THAN FRIDAY                           
NOROT2   BCTR  RF,0                                                             
NEXTWK2  MH    RF,P6+2                                                          
         LA    RF,3(RF)            GET PAST HEADER INFO                         
*        AR    R3,RF               ADDR OF UNIVERSE DATA                        
         AR    R8,RF               ADDR OF UNIVERSE DATA                        
         SR    R4,R4                                                            
         SR    R7,R7                                                            
GETVHOM1 C     R1,END51            END OF DATA                                  
         BL    GETVHOM2                                                         
**       B     GETVHOM3            CHANGED TO GETVHM2B BECAUSE                  
*                                                                               
         OC    SVWTPDQH,SVWTPDQH   CHECK TO SEE IF WE READ A PREVIOUS           
         BZ    GETVHM1A            QHR FOR THE SAME DAY SUCESSFULLY             
         CLC   SVWTPDQH(1),2(RE)   IF SO WE DONT HAVE NUMBER BUT DATA           
         BNE   GETVHM1A            IS IN.                                       
         B     GETVHM1B                                                         
GETVHM1A MVI   WKINFLAG,C'N'       WEEK NOT IN YET                              
* SET UP MISSING DATA COUNT TO GET COUNT # OF MISSING DATA                      
* FOR A CURRENT DAY SO IF WE COULD GET DATA                                     
* FOR THAT DAY EVENTUALLY THEN WE WANT TO ADD THE COUNT TO WEIGHT               
* CORRECTLY AS ZERO VALUES                                                      
         CLI   WKINFLAG,C'N'                                                    
         BNE   GETVHM1B                                                         
         CLI   MISCOUNT,0                                                       
         BE    *+10                IF DAY IS DIFFERENT THAN SAVED               
         CLC   MISSDAY,2(RE)       IN MISSDAY THEN CLEAR COUNT                  
         BE    *+12                                                             
         MVI   MISCOUNT,0                                                       
         MVI   MISSDAY,0                                                        
*                                                                               
         MVC   MISSDAY,2(RE)       SAVE DAY OF MISSING DATA                     
         LLC   RE,MISCOUNT         MISSING DATA COUNT                           
         AHI   RE,1                                                             
         STC   RE,MISCOUNT                                                      
*                                                                               
GETVHM1B B     GETVHM2B            NEED TO FALL THROUGH                         
GETVHOM2 SR    RE,RE                                                            
         SR    RF,RF                                                            
         SR    R5,R5                                                            
         MVI   WKINFLAG,C'Y'       WEEK IS IN                                   
         IC    R4,MASK51                                                        
         EX    R4,*+8                                                           
         B     *+8                                                              
         ICM   RF,0,0(R1)          WEEKLY VALUE                                 
**       BZ    GETVHOM3            END OF DATA                                  
         BZ    GETVHM2B            END OF DATA                                  
         IC    R4,MASK53                                                        
         EX    R4,*+8                                                           
         B     *+8                                                              
*        ICM   R5,0,0(R3)          WEEKLY UNIVERSE                              
         ICM   R5,0,0(R8)          WEEKLY UNIVERSE                              
         JNZ   GETVHM2A                                                         
*                                                                               
         L     RE,VPNWKS           DECREMENT THE NUMBER OF WEEKS BY 1           
         BCTR  RE,0                                                             
         ST    RE,VPNWKS                                                        
         J     GETVHM2B                                                         
*                                                                               
* CALCULATE THE RATING                                                          
GETVHM2A M     RE,=F'2000'                                                      
         DR    RE,R5                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
         AR    R7,RF                                                            
GETVHM2B AH    R1,P6                                                            
*        AH    R3,P6+2                                                          
         AH    R8,P6+2                                                          
         BCT   R0,GETVHOM1                                                      
*                                                                               
GETVHOM3 STC   R0,VPWKSTPT         SAVE BASE WEEKS NEEDED                       
         LR    RF,R7                                                            
         L     RE,VPNWKS                                                        
         SR    RE,R0                                                            
         ST    RE,VPNWKS           ADJUST WEEKS FROM YEAR RECORD                
         LTR   RE,RE                                                            
         JZ    EXIT                                                             
*                                                                               
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,VPNWKS           ADJUST FOR THE WEEKS                         
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         SR    RE,RE                                                            
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),DBFACTOR  AND THE FACTOR                               
         L     R1,FULL                                                          
                                                                                
* SAVE OFF LAST DAY QHR HOUR OF WTP RECORD READ SO WE CAN CONTROL               
* WHETHER WE WANT TO USE THE SAME END DAY/QH FOR THE MONTHLY FILE               
* READ TO INDEX THE DEMOS                                                       
         CLI   WKINFLAG,C'N'      IF CURRENT WEEK IS NOT IN YET                 
         BE    *+8                OR                                            
         CLI   QHRDARK,C'Y'       IF CURRENT QTR HOUR IS DARK WEEK              
         BE    GETVHEXC           DONT WANT TO SAVE                             
         L     RE,DBAQUART                                                      
         MVC   SVWTPDQH,2(RE)     SAVE DAY AND QH                               
* SAVE OFF BITS TO INDICATE THE WEEKS READ                                      
         CLI   SVWTPDQH,X'95'          M-F RECORD                               
         BNE   *+8                                                              
         OI    DAYREAD,B'01111100'                                              
         CLI   SVWTPDQH,X'70'     SAVE OFF BITS OF DAYS READ                    
         BH    GETVHEXC                                                         
         CLI   SVWTPDQH,X'10'                                                   
         BL    GETVHEXC                                                         
         LLC   RE,SVWTPDQH                                                      
         SRL   RE,4                                                             
         LA    R0,X'80'                                                         
         SRL   R0,1                                                             
         BCT   RE,*-4                                                           
         LLC   RE,DAYREAD                                                       
         OR    R0,RE                                                            
         STC   R0,DAYREAD                                                       
*                                                                               
*                                                                               
GETVHEXC DS    0C                                                               
*                                                                               
* COMPARE SVWTPDQH TO DBAQUART                                                  
* IF ITS NOT EQUAL THAT MEANS WE ARE NOT INCLUDING THIS QTR HOUR IN             
* THE FACTOR,                                                                   
*                                                                               
         L     R5,DBAQUART                                                      
         CLC   SVWTPDQH,2(R5)                                                   
         BNE   GETVHOM5                                                         
         A     R1,VPHFACT                                                       
         ST    R1,VPHFACT                                                       
*                                                                               
         CLC   MISSDAY,2(R5)                                                    
         BNE   GETVHOM4                                                         
         LLC   R1,MISCOUNT         GET ALL # OF ZERO DATA COUNT                 
         A     R1,VPHFACT                                                       
         ST    R1,VPHFACT                                                       
GETVHOM4 MVI   MISCOUNT,0          CLEAR MISSING DATA COUNT AND DAY             
         MVI   MISSDAY,0                                                        
GETVHOM5 DS    0C                                                               
*                                                                               
         M     RE,FULL                                                          
         A     RF,VPHHOMES                                                      
         ST    RF,VPHHOMES                                                      
         J     EXIT                                                             
         SPACE 2                                                                
VPHMASK  DC    AL1(1,3,7,15)                                                    
* SWEEP BOOK/START WEEK/END WEEK                                                
SWPWKTAB DC    AL1(98,01,03,06)                                                 
         DC    AL1(98,02,07,10)                                                 
         DC    AL1(98,03,11,14)                                                 
         DC    AL1(98,05,18,21)                                                 
         DC    AL1(98,07,29,32)                                                 
         DC    AL1(98,10,40,43)                                                 
         DC    AL1(98,11,45,48)                                                 
         DC    AL1(99,01,02,05)                                                 
         DC    AL1(99,02,06,09)                                                 
         DC    AL1(99,03,10,13)                                                 
         DC    AL1(99,05,18,21)                                                 
         DC    AL1(99,07,28,31)                                                 
         DC    AL1(99,10,40,43)                                                 
         DC    AL1(99,11,45,48)                                                 
         DC    AL1(100,01,02,05)                                                
         DC    AL1(100,02,06,09)                                                
         DC    AL1(100,03,10,13)                                                
         DC    AL1(100,05,18,21)                                                
         DC    AL1(100,07,28,31)                                                
         DC    AL1(100,10,40,43)                                                
         DC    AL1(100,11,45,48)                                                
         DC    AL1(101,01,02,05)                                                
         DC    AL1(101,02,06,09)                                                
         DC    AL1(101,03,10,13)                                                
         DC    AL1(101,05,18,21)                                                
         DC    AL1(101,07,28,31)                                                
         DC    AL1(101,10,40,43)                                                
         DC    AL1(101,11,45,48)                                                
         DC    AL1(102,01,02,05)                                                
         DC    AL1(102,02,06,09)                                                
         DC    AL1(102,03,10,13)                                                
         DC    AL1(102,05,18,21)                                                
         DC    AL1(102,07,28,31)                                                
         DC    AL1(102,10,40,43)                                                
         DC    AL1(102,11,45,48)                                                
         DC    X'FFFF'                                                          
WORKD    DSECT                                                                  
*                                                                               
MAXDEMS  EQU   42                  *******************                          
*                                                                               
SAVERD   DS    F                                                                
SAVER1   DS    F                                                                
CALLRD   DS    F                                                                
DUB      DS    D                                                                
DUBP     DS    D                                                                
DUB2     DS    D                                                                
DUB3     DS    D                                                                
DPWORK   DS    CL16                                                             
RELO     DS    A                                                                
ALET     DS    A                                                                
ADBUY    DS    A                                                                
ADDEMEL  DS    A                                                                
DMCB     DS    6F                                                               
DMCB2    DS    6F                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
WORK     DS    CL80                                                             
BSDATE   DS    XL3                                                              
BENDDATE DS    XL3                                                              
RMBKFLG  DS    CL1                                                              
SVACTSQC DS    CL1                                                              
SVACTEQC DS    CL1                                                              
ADJQPTR  DS    CL1                                                              
ADJQTAB  DS    XL(DBXTDQTL-DBDQXFXL)                                            
         ORG   ADJQTAB                                                          
ADJQDAY  DS    XL1                                                              
ADJQSQH  DS    XL1                                                              
ADJQEQH  DS    XL1                                                              
ADJQWGT  DS    XL1                                                              
         ORG                                                                    
*                                                                               
AMASTC   DS    A                   A(MASTC)                                     
ADISPTAB DS    A                   MASTER DSPL TABLE ADDRESS                    
ADEMLSTX DS    A                   DEMO LIST END ADDRESS                        
NUMDEMS  DS    F                   ACTUAL NUMBER OF DEMOS IS DEMOLIST           
DEMOLIST DS    XL((MAXDEMS+1)*3)   DEMO LIST FROM BUYREC                        
*                                   USER AND OVERRIDDEN DEMOS EXCLUDED          
DEMOLST2 DS    XL((MAXDEMS+1)*3)   DEMO LIST  #2                                
RDEMLIST DS    XL((MAXDEMS+1)*5)   DEMO LIST FOR USE W/ MEDIA=R                 
*                                   (SEE RDEMLSTD)                              
SAVEHUT  DS    XL1                 USER SVI ADJUSTMENT MONTH(S)                 
SAVEMKT  DS    XL2                 MARKET NUMBER FOR SVI READ                   
SVADJHUT DS    CL1                 SAVED SVI SOURCE CODE                        
SAVEWGTS DS    XL(MAXDEMS)         WEIGHTED DEMO LIST OF WEIGHTS                
*                                                                               
ORBWGT   DS    F                   COUNT OF ORBDEM ITEMS                        
*                                                                               
SVIS     DS    XL(MAXDEMS*4)       MONTH WEIGHTED SVI VALUES (1/DEMO)           
SVITYPES DS    XL(MAXDEMS)         SVI TYPE FOR EACH ENTRY ABOVE                
SVIWGT   DS    F                   COUNT OF SVI ITEMS                           
*                                                                               
         DS    0F                                                               
QHUNIV   DS    XL12                UNIVERSE FOR QH CALL                         
QHVUTS   DS    XL12                VUTS FOR QH CALL                             
DTVUTS   DS    XL12                ACCUMULATED VUTS                             
OHOMSHR  DS    XL(3*4)             OUTPUT AREA FOR ORIGINAL HOMES SHARE         
TOTHMSHR DS    XL(3*4)             ACCUMULATED ORIGINAL HOMES SHARE             
*                                                                               
QHDEMSA  DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR QH                       
QHDEMSU  DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
DTDEMSA  EQU   QHDEMSA                                                          
DTDEMSU  EQU   QHDEMSU                                                          
AFDEMSA  DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR AFFID                    
AFDEMSU  DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
BUDEMSA  DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR BUY                      
BUDEMSU  DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
RADEMSA  DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR RADIO LOOKUP             
RADEMSU  DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
RADEMSU2 DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
*                                                                               
WKADCONS DS    0A                                                               
ADTUNVS  DC    A(DTUNVS-WORKD)                                                  
ADTDEMS  DC    A(DTDEMS-WORKD)                                                  
ASVIREC  DC    A(SVIREC-WORKD)                                                  
AIUNWK   DC    A(IUNWK-WORKD)                                                   
AIUNVS   DC    A(IUNVS-WORKD)                                                   
AIUNOLD  DC    A(IUNOLD-WORKD)                                                  
AIRTGOLD DC    A(IRTGOLD-WORKD)                                                 
AIPUTOLD DC    A(IPUTOLD-WORKD)                                                 
AIIMPOLD DC    A(IIMPOLD-WORKD)                                                 
AITOTOLD DC    A(ITOTOLD-WORKD)                                                 
AIUNOLDX DC    A(IUNOLDX-WORKD)                                                 
AIUNNEW  DC    A(IUNNEW-WORKD)                                                  
AIRTGNEW DC    A(IRTGNEW-WORKD)                                                 
AIPUTNEW DC    A(IPUTNEW-WORKD)                                                 
AIIMPNEW DC    A(IIMPNEW-WORKD)                                                 
AITOTNEW DC    A(ITOTNEW-WORKD)                                                 
AIUNNEWX DC    A(IUNNEWX-WORKD)                                                 
AIUNXTRA DC    A(IUNXTRA-WORKD)                                                 
ADBLOCKS DC    A(DBLOCKS-WORKD)                                                 
ASPDTTAB DC    A(SPDTTAB-WORKD)                                                 
AGDEXSPD DC    A(GDEXSPDT-WORKD)                                                
ADBLOCK2 DC    A(DBLOCK2-WORKD)                                                 
ACOMEXT  DC    A(COMEXT-WORKD)                                                  
WKADCONX EQU   *                                                                
*                                                                               
WTDEMA   DS    F                                                                
WTDEMU   DS    F                                                                
         EJECT                                                                  
TAPEOPT  DS    CL1                 OPTION FOR TAPE BASED CALCULATIONS           
DEMANDSW DS    XL1                 NON-ZERO IF DEMAND HOOK CALLED               
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
COMPASS  DS    XL1                 X'01' =PASS 1 / X'02'=PASS2                  
COMCSD   DS    C                   COMSCORE SURVEY DATE (B,S)                   
REQSDATE DS    XL2                 REPORT REQUEST START DATE                    
REQEDATE DS    XL2                 REPORT REQUEST END DATE                      
ASPXT50E DS    A                   A(50 ELEMENT PASSED IN)                      
SVISW    DS    XL1                 FIRST TIME SWITCH FOR SVI SEARCH             
SHRSW    DS    XL1                 INDICATES SHARE COMP REQUIRED (TBC)          
SVACTBK  DS    XL2                 ACTUAL BOOK (FUCKED UP BY DEMOUT)            
SVISQH   DS    XL1                 ACTUAL SVI QH FOR CURRENT DEMO REC           
SVIEQH   DS    XL1                 END SVI QH FOR CURRENT DEMO REC              
SVIDAY   DS    XL1                 SVIFILE DAY FOR CURRENT DEMO REC             
XQHNUM   DS    XL1                 USED FOR XQH QTR HR COUNT                    
WKLYIND  DS    XL1                 X'80'=WEEKLY DATA,X'40'=UNWEEKLY             
WKLYOPT  DS    XL1                 X'80'=WEEKLY REQ ,X'40'=MISSING DATA         
AFDOPT   DS    XL1                 AFFID OPTION                                 
AFDO7MIN EQU   X'80'               AFFID 7 MINUTE OPTION                        
AFDO7SET EQU   X'40'               7 MINUTE OPTION TIMES HAVE BEEN SET          
IUNTYPE  DS    XL1                 1 FOR RTGS/IMPS ONLY, 4 FOR ALL              
STNAME   DS    CL16                START QH PROGRAM NAME                        
ENDNAME  DS    CL16                END QH PROGRAM NAME                          
STNAMEOV DS    CL16                START QH PROGRAM NAME OVERNIGHTS             
ENDNAMEO DS    CL16                END QH PROGRAM NAME OVERNIGHTS               
NEWNAME  DS    CL16                NEXT PROGRAM FOR XQH LOOKUP                  
SV1WPROF DS    CL16                WEEKLY DATA PROFILE                          
         ORG   SV1WPROF                                                         
SV1WPR1  DS    CL1                 WEEKLY DATA FOR ALL BUYS (Y/N)               
SV1WPR2  DS    CL1                 WEEKLY DATA FOR -S BUYS  (Y/N)               
SV1WPR3  DS    CL1                 WEEKLY DATA IF PROGRAM STARTS WITH X         
SV1WPR4  DS    CL1                 ANGLO/FRANCO OPTION (CANADA ONLY)            
SV1WPR5  DS    CL1                 NO WEEKLY DATA IF PGM STARTS WITH X          
SV1WPR6  DS    CL1                 DMA=I/R                                      
SV1WPR7  DS    CL1                 BUY/POST RADIO DEMOS                         
SV1WPR8  DS    CL1                 NORMALIZE HPT                                
SV1WPR9  DS    CL1                 CANADIAN METERED MARKET TREATMENT            
SV1WPR10 DS    CL1                 USE BBM MEETER MRKT IF AVAILABLE             
         ORG                                                                    
*                                                                               
ASVIMOD  DS    A                   A(SVI MODIFIER TABLE)                        
ASVIDEM  DS    A                   A(SVI DEMO TABLE)                            
SVIWGTS  DS    XL24                MONTHLY SVI WEIGHTS                          
SVIWGTSM DS    F                   SUM OF SVIWGTS                               
*                                                                               
DATADISP DS    H                                                                
*                                                                               
SQHVAL   DS    F                                                                
EQHVAL   DS    F                                                                
SQHNUM   DS    X                                                                
EQHNUM   DS    X                                                                
LQHLIST  DS    XL4                                                              
*                                                                               
NUMSPOTS DS    H                                                                
ELCODE   DS    XL1                                                              
AFDSW    DS    XL1                                                              
AFDPRD   DS    XL1                                                              
AFDSLN   DS    XL1                                                              
AFDSDT   DS    XL2                                                              
AFDEDT   DS    XL2                                                              
NOAFDCNT DS    H                   COUNT OF MISSING AFFIDS                      
AFDCNT   DS    H                   COUNT OF AFFIDS                              
AF7QHST  DS    H                   7 MIN OPT - ST TIME OF LAST QH - 2           
AF7QHEND DS    H                   7 MIN OPT - END TIME + 2                     
AF7QHSET DS    H                   7 MIN OPT - TIME TO SET AFFID TO             
AFDATE   DS    XL2                 AFFID DATE PROCESSED THIS TIME               
AFDTIME  DS    XL2                 AFFID TIME                                   
AFDBOOK  DS    XL2                 AFFID BOOK                                   
COMERRF  DS    X                   COMSCORE ERROR FLAG                          
*                                                                               
FLAGS    DS    X                   VARIAOU FLAGS                                
CANHPT   EQU   X'80'               CANADIAN HUTS, PUTS & TOTS                   
WTPDAYOP EQU   X'40'               START WEEK ON SATURDAY (NSI WTP)             
*        EQU   X'20'               UNUSED                                       
*        EQU   X'10'               UNUSED                                       
*        EQU   X'08'               UNUSED                                       
*        EQU   X'04'               UNUSED                                       
*        EQU   X'02'               UNUSED                                       
*        EQU   X'01'               UNUSED                                       
*                                                                               
VNSIWEEK DS    A                                                                
VNETWEEK DS    A                                                                
VGETIUN  DS    A                                                                
VDEFINE  DS    A                                                                
VBKTYPTB DS    A                   A(BOOKTYPE TABLE) IN DEMTABS                 
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
READCUNV DS    C                                                                
ANYACTBK DS    C                                                                
SVDIVSOR DS    XL(L'DBDIVSOR)                                                   
BKTYPTBL DS    H                   L'BOOKTYPE TABLE ENTRY                       
TWODEC   DS    C                                                                
MASK51   DS    C                   BIT MASK FOR 51 ELEMENT - IMP                
MASK53   DS    C                   BIT MASK FOR 53 ELEMENT - UNIV               
VPWKSTPT DS    C                   NUM OF WKS COMING FROM TPT                   
VPNWKS   DS    F                   NUM OF WKS COMING FROM WEEKLY                
ELEM53   DS    F                   A(UNIVERSE ELEMENT)                          
HOMUNIV  DS    F                   HOMES UNIVERSE                               
END51    DS    F                   END OF WEEKLY ELEMENT                        
VHSDAT   DS    CL2                 VPH START DATE                               
VHEDAT   DS    CL2                 VPH END DATE                                 
VPHFULL  DS    F                   JUST SOME WORK AREA                          
OVHOMUNV DS    F                   OV HOM UNIV                                  
OVPHHMES DS    XL4                 OVERNIGHTS RAW HOMES VALUE                   
VPHHOMES DS    XL4                 WEEKLY HOMES VALUE                           
VPHFACT  DS    XL4                 WEEKLY HOMES DBFACTOR                        
VPHSWK   DS    XL2                 START WEEK FOR VPHOMES                       
VPHEWK   DS    XL2                 END WEEK FOR VPHHOMES                        
*&&DO                                                                           
*  TAKE OUT DONT THINK WE NEED THIS ANYMORE BEN 03/2007                         
DARKFLAG DS    X                   LPM DARK WEEK FLAG                           
DARKONLY EQU   X'80'                                                            
NOTDARK  EQU   X'00'                                                            
*&&                                                                             
DMINDEX  DS    C                   DEMO INDEX FLAG                              
*******************************************************                         
* DO NOT SEPERATE WVPHOPT, OVPHOPT                    *                         
LPMLKOPT DS    CL1                                                              
WKOVOPT  DS    0CL2                                   *                         
WVPHOPT  DS    CL1                 WEEKLY VPH OPTION  *                         
OVPHOPT  DS    CL1                 WEEKLY VPH OPTION  *                         
*******************************************************                         
SMMMONTH DS    C                   NEW SMM POSTING OPTION                       
DIGITALF DS    C                   Y/N  DIGITAL TRANSITION FLAG                 
*                                                                               
CALLERF  DS    X                   SAVE CALLER INDICATOR                        
BISPTDAY DS    X                                                                
SPOTDAY  DS    CL3                 DAY OF SPOT DATE                             
MKTTYPE  DS    X                                                                
MKTLPM   EQU   X'80'               LPM MARKET                                   
MKTSETM  EQU   X'40'               SET METERED                                  
MKTDIARY EQU   X'20'               DIARY MKT                                    
SVAFFMED DS    CL1                 SAVE AFFID MEDIA                             
SVSELBK  DS    XL2                 SAVE BOOK AREA                               
SVMED    DS    C                                                                
ANYOVFLG DS    C                                                                
ANTDELEM DS    A                    A(BUY NON TRADITIONAL DEMO LIST)            
NTDFLAG  DS    C                    DO WE HAVE NON TRADITIONAL DEMOS            
NTDCOMD  EQU   X'80'                HAVE COMSCORE DEMOS                         
NTDNSID  EQU   X'40'                HAVE NSI DEMOS                              
SVSELSTA DS    CL(L'DBSELSTA)                                                   
DSTACALL DS    CL(L'DBSELSTA)                                                   
SVDBFUNC DS    X                                                                
SVDBTYPE DS    C                                                                
SVDBBEST DS    CL(L'DBBEST)                                                     
SVNSIERR DS    X                                                                
SVSELDAY DS    C                                                                
SVSELAGY DS    CL(L'DBSELAGY)                                                   
SVSELSRC DS    C                                                                
SVSELSYC DS    XL(L'DBSELSYC)                                                   
SVLPMLKOPT DS  CL(L'LPMLKOPT)                                                   
SVWVPHOPT  DS  CL(L'WVPHOPT)                                                    
QHRDARK  DS    C                                                                
NEXTYRLK DS    C                   Y/N FORCE NEXT YEAR WKLY LOOKUP?             
NXTWKFLG DS    C                   Y/N -                                        
DSTASPLT DS    C                   INDICATE DSTATION SPLIT WEEK                 
WKINFLAG DS    C                   FLAG TO INDICATE IF WEEK IS IN YET           
MISSDAY  DS    C                                                                
MISCOUNT DS    X                                                                
LASTYRLK DS    C                   Y/N TO FORCE LAST YR LOOKUP                  
ESTEOW   DS    X                   ESTIMATE OUT OF WEEK ROTATOR                 
SVWTPDQH DS    XL2                 SAVE WTP 1 BYTE DAY/ 1 BYTE QH               
DAYREAD  DS    X                   BITS (0,MON,TUE,WED,THU,FRI,SAT,SUN)         
BKTYPIND DS    X                   BOOKTYPE INDICATOR                           
OVEFFBK  DS    XL2                 BOOKTYPE EFFECTIVE BOOK -OVERNIGHTS          
STABKMKT DS    XL2                 NSI MKT FOR STATION/BOOK                     
LPMDTB   DS    XL3                 LPM 3-BYTE START DATE                        
LPMDTP   DS    XL2                 LPM 2-BYTE START DATE                        
LPMDRKWK DS    XL2                 LPM DARKWEEK                                 
CBLKUP   DS    XL2                 ALT SYSCODE FOR STA 7000-7500                
TAPEHHSR DS    C                   Y= ORIGINAL HOMESHARES FROM TAPE             
VSTAPACK DS    A                                                                
VCOMINTR DS    A                                                                
TRUELPMF DS    C                                                                
SVNUMMKT DS    XL2                                                              
SVCMPNAM DS    CL(L'DBCMPNAM)                                                   
SVRMBKEX DS    A                                                                
PREVSPDT DS    XL2                                                              
*                                                                               
         DS    0F                                                               
DBXTDQT  DS    0X                  DBLOCK EXTENSION FOR DAYS/QHS TABLE          
         DS     XL(DBDQXFXL)          FIXED-LENGTH PART OF LINK                 
         DS     (8*3)XL(DBDQLEN)      ALTERNATE DAY/QHR CONTROL AREA            
         DS     XL1                   EOL                                       
DBXTDQTX EQU   *                                                                
DBXTDQTL EQU   DBXTDQTX-DBXTDQT                                                 
                                                                                
ADBDQD   DS    A                   A(DAY/QHR TABLE, I.E. DBDQTAB)               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*        STORE FOR EXTENDED DBLOCK                                              
*                                                                               
         DS    XL80                DBLOCK SLOP OVER                             
DEXTRA2  DS    CL128               DBLOCK EXTENDED                              
DEXTRA3  DS    CL128               2ND DBLOCK EXTENSION                         
DEXTRA1  DS    CL16                EXTENSION 1 FOR USERID                       
DEXTSPT  DS    CL16                EXTENSION FOR SPOT POSTING DATES             
*                                                                               
VUTLST   DS   XL10                                                              
PUTLST   DS   XL10                                                              
UNILST   DS   XL10                                                              
SVRADUNV DS    XL255                                                            
         EJECT                                                                  
       ++INCLUDE DEDEMEQUS                                                      
         EJECT                                                                  
****************** I U N   R E C O R D *****************                        
         SPACE 1                                                                
IUNDEMS  EQU   32                  UNVS/RTGS/IMPS/PUTS/TOTS (EACH)              
IUNHMDSP EQU   80                  DSPL TO HMS RTGS IN IUN RTGS AREA            
*                                                                               
IUNWK    DS    0F                                                               
*                                                                               
IUNVS    DS    (IUNDEMS)F                                                       
IUNVX    EQU   *                                                                
*                                                                               
IUNOLD   EQU   *                                                                
IRTGOLD  DS    (IUNDEMS)F                                                       
IIMPOLD  DS    (IUNDEMS)F                                                       
IPUTOLD  DS    (IUNDEMS)F                                                       
ITOTOLD  DS    (IUNDEMS)F                                                       
IUNOLDX  EQU   *                                                                
*                                                                               
IUNNEW   EQU   *                                                                
IRTGNEW  DS    (IUNDEMS)F                                                       
IIMPNEW  DS    (IUNDEMS)F                                                       
IPUTNEW  DS    (IUNDEMS)F                                                       
ITOTNEW  DS    (IUNDEMS)F                                                       
IUNNEWX  EQU   *                                                                
*                                                                               
IUNXTRA  EQU   *                                                                
IUNSHMS  DS    F                                                                
IUNSMETA DS    F                                                                
IUNSMETB DS    F                                                                
*                                                                               
ILUNVS   DS    (IUNDEMS)F          LOONEYVERSES                                 
ILUNVX   EQU   *                                                                
         SPACE 2                                                                
****************    DEMO LOOKUP AREAS ************                              
         SPACE 1                                                                
         DS    0F                                                               
DTUNVS   DS    (IUNDEMS*4)X        UNIVERSES                                    
*                                                                               
DTDEMS   DS    0F                  ACCUMULATORS FOR DEMAND CALL                 
DTRTGS   DS    (IUNDEMS*4)X        RATINGS                                      
DTIMPS   DS    (IUNDEMS*4)X        IMPRESSIONS                                  
DTPUTS   DS    (IUNDEMS*4)X        PUTS                                         
DTTOTS   DS    (IUNDEMS*4)X        TOTALS                                       
DTXTRA   DS    7F                  SHARES(3)                                    
DTDEMX   EQU   *                                                                
         ORG                                                                    
GDEXSPDT DS    CL16                SPOT POSTING DATES                           
* 2 BYTE SPOT DATE , 1 BYTE DUPLICATION FACTOR                                  
SPDTTAB  DS    300XL3              TABLE OF SPOT DATES                          
DBLOCKS  DS    XL256               DBLOCK SAVE AREA                             
DBLOCK2  DS    XL256               2ND DBLOCK AREA                              
         DS    CL20                                                             
COMEXT   DS    CL(DBCMINDX-DBCMINTD)                                            
         EJECT                                                                  
         DS    1024C               MAXIMUM SVI RECORD SIZE                      
         ORG   *-1024                                                           
       ++INCLUDE DESVIREC                                                       
         ORG                                                                    
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
*                                                                               
*-------------------------- RADIO DEMO LIST --------------------------*         
RDEMLSTD DSECT                                                                  
RDLDMOD  DS    CL1                                                              
RDLDNUM  DS    XL1                                                              
RDLDMOD1 DS    CL1                                                              
RDLDMOD2 DS    CL1                                                              
RDLOPER  DS    CL1                                                              
RDEMLSTQ EQU   *-RDEMLSTD                                                       
         DS    0CL(5-RDEMLSTQ+1)                                                
***********************************************************************         
       ++INCLUDE SPDEMLKXTD                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE SPOTTABD                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPSTAPACKD                                                     
COMFACSD DSECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDMONYREQU                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'158SPGETDEMF 02/18/21'                                      
         END                                                                    
