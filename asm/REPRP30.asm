*          DATA SET REPRP30    AT LEVEL 060 AS OF 12/06/07                      
*          DATA SET REPRP30AA  AT LEVEL 028 AS OF 03/22/07                      
*PHASE T81A30A                                                                  
*                                                                               
T81A30   TITLE 'REPRP30 - REACH AND FREQUENCY ROUTINES'                         
*********************************************************************           
*                                                                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
*  HISTORY AS OF 10/7/99                                              *         
*                                                                     *         
* 03/08/2000   JRD   DON'T DUMP WHEN THERE ARE NO BOOKS ON FILE       *         
*                                                                     *         
* 09/04/2000   FJD   TEMPORARY DEMO LIMITING BASED ON USERID#         *         
*                                            (VAL4MKT ROUTINE)        *         
*                                                                     *         
* 09/08/2000   FJD   CUME OVERRIDE FUNCTIONALITY ADDED                *         
*                                                                     *         
* 10/05/2006   BU    CABLE STATION ALIAS                              *         
*                                                                     *         
* 11/01/2006   BU    STANDARD LPM BOOK LIST CHANGES                   *         
*                                                                     *         
* 02/05/2007   BU    SATELLITE / STATION ALIAS PROBLEM                *         
*                                                                     *         
* 02/12/2007   BU    SEPARATE QUARTERLY / MONTHLY INDICATORS          *         
*                                                                     *         
* 02/13/2007   BU    TWO-PASS VERSION OF QTRLY / MONTHLY              *         
*                                                                     *         
* 02/20/2007   BU    ELIMINATE DUMP WHEN COMP STATION ENTERED AS      *         
*                    "KXXX-1"                                         *         
*                                                                     *         
* 02/22/2007   BU    ADD JAN/OCTOBER MARKETS TO TABLE                 *         
*                                                                     *         
* 03/21/2007   BU    CORRECT LISTHOOK DUMP                            *         
*                                                                     *         
* 03/22/2007   BU    FIX DUPLICATION OF MONTHLY DATA                  *         
*                                                                     *         
* 03/27/2007   BU    REPAIR MISSING BOOK SITUATION                    *         
*                                                                     *         
* 03/28/2007   BU    VERSION CHECK FOR MONTHLY BOOKS                  *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
T81A30   CSECT                                                                  
         NMOD1 OVERWRKQ,*T81A30*,R7,RR=RE,CLEAR=YES                             
         LR    R9,RC                                                            
         USING OVERWRKD,R9                                                      
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         L     RA,AMAPTAB                                                       
         USING MAPTABD,RA                                                       
DB       USING DBLOCK,MYDBLOCK                                                  
*                                                                               
         MVC   OVPARMS,0(R1)                                                    
         ST    RE,OVRELO                                                        
*                                                                               
         L     RE,ACOMFACS                                                      
         L     R8,CDEMAND-COMFACSD(RE)                                          
         ST    R8,VDEMAND                                                       
         L     R8,CDEMOUT-COMFACSD(RE)                                          
         ST    R8,VDEMOUT                                                       
*                                                                               
         LR    R8,RB                                                            
         AH    R8,=Y(COMMON-T81A30)                                             
         USING COMMON,R8                                                        
*                                                                               
         SRL   RF,32-8                                                          
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SLL   RF,2                                                             
         L     RF,ROUTS(RF)                                                     
         A     RF,OVRELO                                                        
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
ROUTS    DS    0F                                                               
         DC    A(HARRIS)                                                        
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
COMMON   DS    0D                                                               
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
ETOOBIG  MVC   ERROR,=Y(804)                                                    
         B     EXITL                                                            
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     DS    0H                                                               
         XIT1  ,                   EXIT WITH CC SET                             
DIE      DC    H'0'                                                             
***********************************************************************         
* CHECK FOR A HISPANIC STATION CALL LETTER SWITCH                               
*                                                                               
*  R1 A(STATION CALL LETTERS)                                                   
*                                                                               
*  TEL-H --> TELE-T ON EJOR(B3) AND TELEMUNDO(B1)                               
*  KOB1 --> KOB 1 ON PETRY(PV)                                                  
*                                                                               
* IMPORTANT!!!! MUST MATCH SWITCH IN REPRP00!!!!!!!!!!!!!!!                     
*                                                                               
***********************************************************************         
SWHISP   DS    0H                                                               
         CLC   REPALPHA,=C'B3'     TELE-H                                       
         BE    *+14                                                             
         CLC   REPALPHA,=C'B1'                                                  
         BNE   SWH010                                                           
*                                                                               
         CLC   =C'TEL H',0(R1)                                                  
         BNE   SWH010                                                           
         MVC   0(5,R1),=C'TELE '                                                
         B     SWHISPX                                                          
*                                                                               
SWH010   DS    0H                                                               
         L     RF,ATWA                                                          
         USING T81AFFD,RF                                                       
         CLC   VERSION,=XL4'02020008'                                           
         BNL   SWH020                                                           
         DROP  RF                                                               
*                                                                               
         CLC   REPALPHA,=C'PV'                                                  
         BNE   SWH020                                                           
*                                                                               
         CLC   =C'KOB 1',0(R1)                                                  
         BNE   SWH020                                                           
         MVC   0(5,R1),=C'KOB1 '                                                
         B     SWHISPX                                                          
*                                                                               
SWH020   DS    0H                                                               
*                                                                               
SWHISPX  DS    0H                                                               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
QTRS     DC    X'0205070B0205070B'                                              
*                                      PRIME MONTH IN QUARTER                   
MONS     DC    X'010203040506'                                                  
         DC    X'0708090A0B0C'                                                  
         DC    X'010203040506'                                                  
         DC    X'0708090A0B0C'                                                  
*                                      MONTH-BY-MONTH TABLE                     
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
*                                                                               
STALENQ  EQU   8                                                                
DPTLENQ  EQU   5                                                                
DEMLENQ  EQU   3                                                                
BKLENQ   EQU   5                                                                
UPGLENQ  EQU   11+14+1                                                          
RCDLENQ  EQU   8+1+1                                                            
FLTLENQ  EQU   6                                                                
         EJECT                                                                  
*********************************************************************           
* R/F FOR THE HARRIS MODEL                                                      
*********************************************************************           
HARRIS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*   TEST                                                                        
         XC    TESTCTR2,TESTCTR2                                                
*   TEST                                                                        
*                                                                               
         L     R2,OVPARMS+4        SAVE OFF PARAMETER INFO                      
         LA    R2,VHPARMLQ(R2)                                                  
*                                                                               
         ZIC   RE,0(R2)            SKIP BOOKS                                   
         MHI   RE,BKLENQ                                                        
         LA    R2,1(RE,R2)                                                      
*                                                                               
         CLI   0(R2),0                                                          
         BE    HARRISX             NO DEMOS QUIT                                
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=Y(DEMLENQ)      LENGTH OF DEMOS                              
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,*+4                                                           
         MVC   DEMOS(0),0(R2)                                                   
         LA    R2,1(RF,R2)         BUMP TO RATECARDS                            
         LA    RF,DEMOS+1(RF)      POINT TO END OF DEMO LIST                    
         MVI   0(RF),X'FF'                                                      
*                                                                               
         MVC   CUMES,DEMOS                                                      
         LA    RF,CUMES            CONVERT RATINGS TO CUMES                     
HARR0020 DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    HARR0040                                                         
         CLI   1(RF),C'R'                                                       
         BNE   *+8                                                              
         MVI   1(RF),C'C'                                                       
         LA    RF,3(RF)                                                         
         B     HARR0020                                                         
*                                                                               
HARR0040 DS    0H                                                               
         ZIC   RE,0(R2)            SKIP DAYPARTS                                
         MHI   RE,DPTLENQ                                                       
         LA    R2,1(RE,R2)                                                      
         ZIC   RE,0(R2)            SKIP COMMENTS                                
         MHI   RE,60                                                            
         LA    R2,1(RE,R2)                                                      
*                                                                               
         MVC   NUMSTAS,0(R2)       NUMBER OF STATIONS                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         ST    RF,FRSTSTA2         SAVE ADDITIONAL COPY                         
         ZIC   RF,NUMSTAS                                                       
         MH    RF,=Y(STALENQ)                                                   
         AR    R2,RF                                                            
*                                                                               
         L     R2,OVPARMS+4        DETERMINE BROADCAST QUARTERS                 
         USING VHPARMD,R2                                                       
         GOTO1 VDATCON,DMCB,(8,VHPFLS),(0,WORK+6)                               
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),WORK+12,VGETDAY,VADDAY                  
         GOTO1 VDATCON,DMCB,(0,WORK+18),(3,WORK)                                
*                                                                               
         GOTO1 VDATCON,DMCB,(8,VHPFLE),(0,WORK+6)                               
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),WORK+12,VGETDAY,VADDAY                  
         GOTO1 VDATCON,DMCB,(0,WORK+18),(3,WORK+3)                              
         DROP  R2                                                               
*                                                                               
         MVC   STYEAR,WORK         SAVE START YEAR                              
         MVC   ENYEAR,WORK+3       SAVE END YEAR                                
         MVC   STMONTH,WORK+1      SAVE START MONTH                             
         MVC   ENMONTH,WORK+4      SAVE END   MONTH                             
*                                                                               
         ZIC   RF,WORK+1                                                        
         SR    RE,RE                                                            
         LA    R0,3                                                             
         DR    RE,R0                                                            
         LTR   RE,RE               ADD 1 IF THERES A REMAINDER                  
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,STQTR            STORE STARTING QUARTER                       
*                                                                               
         ZIC   RF,WORK+4                                                        
         SR    RE,RE                                                            
         LA    R0,3                                                             
         DR    RE,R0                                                            
         LTR   RE,RE               ADD 1 IF THERES A REMAINDER                  
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,ENQTR            STORE END QUARTER                            
*                                                                               
         ZIC   RE,STQTR            DETERMINE NUMBER OF QUARTERS                 
         BCTR  RE,0                INCLUSIVE                                    
         SR    RF,RE                                                            
         CLC   STYEAR,ENYEAR                                                    
         BE    *+8                                                              
         AHI   RF,4                                                             
         STC   RF,NUMQTRS                                                       
         EJECT                                                                  
*------------------------------------------------------------                   
* BUILD LIST OF VALID STATIONS                                                  
*------------------------------------------------------------                   
         L     RE,AIO2                                                          
         LHI   RF,LENIO             IN THE MARKET IN AIO2                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVC   DB.DBSELSRC,RTSRVC                                               
*                                                                               
         GOTO1 =A(SETALIAS),DMCB,(RC),RR=Y                                      
*                                                                               
         GOTO1 =A(SETCALIA),DMCB,(RC),RR=Y                                      
*                                  SET COMPETITIVE ALIASES                      
*                                                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         MVC   DB.DBSELSTA,VHPSTA                                               
         DROP  RE                                                               
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         CLC   CABLESTA,SPACES     HAS AN ALIAS BEEN FOUND?                     
         BNH   HARR0060            NO                                           
         MVC   DB.DBSELSTA,CABLESTA   YES - REPLACE STATION WITH ALIAS          
         MVC   DB.DBSELALF,CABLEMKT   REPLACE / INSERT ALPHA MKT                
         MVI   DB.DBBTYPE,C'C'        SET TO 'CABLE OUTLET'                     
HARR0060 EQU   *                                                                
*                                                                               
         MVI   DB.DBFUNCT,DBGETMK  GET THE RATING SERVICE MARKET                
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,0,0                                       
*                                                                               
         OC    DB.DBACTRMK,DB.DBACTRMK                                          
         BZ    HARRISX             NO DEMO INFO AVAILABLE                       
*                                                                               
         MVC   MARKET,DB.DBACTRMK                                               
*                                                                               
*   TEST TO TRANSFER BASED ON MARKET NUMBER, SET MONTHLY BOOKS IF               
*        LPM MARKET.  CONSIDER SPECIAL MARKETS                                  
*        ONLY SET LPMMKT IF VERSION AFTER 02.06.56 : PER JENNY                  
*                                                                               
         MVI   LPMMKT,C'N'         SET 'NOT LPM MKT'                            
         MVI   LPMPASS,C'N'        SET 'NOT LPM MKT'                            
*                                                                               
         L     RE,ATWA                                                          
         USING T81AFFD,RE                                                       
         CLC   VERSION,=X'02060038'   >02.06.56+ SETS LPM FLAG                  
         BNH   HARR0070                                                         
         DROP  RE                                                               
*                                                                               
         GOTO1 =A(SETLPMKT),RR=Y    SET LPMMKT = (Y)ES OR (N)O                  
*                                                                               
*                                                                               
*        TEMPORARY KLUGE TO PROTECT NIELSEN MARKET DATA FROM                    
*          LOCAL STATIONS USING AN UMBRELLA AGENCY CODE                         
                                                                                
HARR0070 EQU   *                                                                
*                                                                               
         BAS   RE,VAL4MKT          IS THIS USERID/MKT ALLOWED?                  
         BNE   HARRISX             NO, DO NOT RETRIEVE DEMOS                    
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVC   DB.DBSELSRC,RTSRVC                                               
         MVI   DB.DBFUNCT,DBGETTLB    GET THE LATEST BOOK                       
         EDIT  (B2,MARKET),(4,DB.DBSELSTA),FILL=0                               
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,BOOKHOOK,0                                
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVC   DB.DBSELSRC,RTSRVC                                               
         MVI   DB.DBFUNCT,DBGETMS     GET THE STATIONS                          
         MVC   DB.DBSELRMK,MARKET                                               
         MVC   DB.DBSELBK,LATESTBK                                              
*                                                                               
         CLI   NUMSTAS,0                                                        
         BE    HARR0080            NO COMPETITIVE STATIONS IN REQUEST           
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,LISTHOOK,0                                
*                                                                               
         MVI   DB.DBBTYPE,C'W'     SET FOR CABLE STATION CALL                   
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,LISTHK2,0                                 
*                                                                               
HARR0075 EQU   *                                                                
         L     RE,AIO2             POINT TO VALIDATED STATIONS                  
         MVC   NUMSTAS,0(RE)       FOR THE REMAINDER OF ROUTINE                 
         LA    RE,1(RE)                                                         
         ST    RE,FRSTSTA                                                       
*                                                                               
HARR0080 DS    0H                                                               
*                                                                               
*   FOR LPM MARKETS, EVERY STATION WILL MAKE TWO PASSES.                        
*        THE FIRST  WILL RETRIEVE QUARTERLY DATA                                
*        THE SECOND WILL RETRIEVE MONTHLY DATA                                  
*                                                                               
         CLI   LPMPASS,LPMSKIP     LPM MARKET?                                  
         BE    HARR0090            NO                                           
         CLI   LPMPASS,LPMQTRS     YES - QTRS PASS?                             
         BE    HARR0090            YES                                          
         CLI   LPMPASS,LPMMONS     MONTHLY PASS?                                
         BE    HARR0330            PROCESS LPM MARKET                           
         DC    H'0'                UNRECOGNIZED VALUE                           
*                                                                               
HARR0090 DS    0H                                                               
         ZIC   R3,NUMQTRS          BUILD LIST OF BOOKS                          
         ZIC   R2,STQTR                                                         
         BCTR  R2,0                                                             
         LA    RE,QTRS(R2)         INDEX INTO MONTHS TABLE                      
         MVC   BYTE,0(RE)                                                       
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         B     HARR0160                                                         
*                                                                               
HARR0120 DS    0H                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
*                                                                               
         LA    RE,QTRS(R2)         INDEX INTO MONTHS FOR QUARTER TABLE          
         MVC   DB.DBSELBK(1),ENYEAR                                             
         CLC   BYTE,0(RE)                CURRENT QTR BEFORE START?              
         BNL   *+10                      NO USE END YEAR                        
HARR0160 MVC   DB.DBSELBK(1),STYEAR                                             
         MVC   DB.DBSELBK+1(1),0(RE)                                            
*                                                                               
*              SUPPORT CUME OVERRIDE RECORDS                                    
***      BAS   RE,GETOVER                LOOK FOR OVERRIDE BOOK                 
         GOTO1 =A(GETOVER),RR=Y          LOOK FOR OVERRIDE BOOK                 
         BE    HARR0240                                                         
*                                                                               
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVI   DB.DBTPTT,C'T'                                                   
         MVI   DB.DBFUNCT,DBVLSTBK                                              
         MVC   DB.DBSELSRC,RTSRVC                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         MVC   DB.DBSELSTA,VHPSTA                                               
         DROP  RE                                                               
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         CLC   CABLESTA,SPACES     HAS AN ALIAS BEEN FOUND?                     
         BNH   HARR0180            NO                                           
         MVC   DB.DBSELSTA,CABLESTA   YES - REPLACE STATION WITH ALIAS          
         MVC   DB.DBSELALF,CABLEMKT   REPLACE / INSERT ALPHA MKT                
****>>>  MVI   DB.DBBTYPE,C'C'        SET TO 'CABLE OUTLET'                     
         CLC   CABLEMKT,SPACES                                                  
         BNH      *+10                                                          
         MVC   DB.DBSELMK,MARKET                                                
HARR0180 EQU   *                                                                
         MVC   DB.DBSELDAY,=X'40'               M                               
         MVC   DB.DBSELTIM,=X'07D007DF'         8-815P                          
*                                                                               
*        SEE IF THE BOOK IS LOADED                                              
*                                                                               
         MVI   WORK+4,1            TRY COUNT                                    
HARR0200 DS    0H                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,0,0                                       
         CLI   DB.DBERROR,0        ERROR?                                       
         BE    HARR0220            NO                                           
*                                                                               
         CLI   DB.DBERROR,16       NOT FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         CLI   WORK+4,5            ONLY ALLOW 5 TRIES                           
         BNL   HARR0240            JUST USE THIS BOOK                           
***      BL    *+6                                                              
***      DC    H'0'                                                             
*                                                                               
         ZIC   RE,WORK+4                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WORK+4                                                        
*                                                                               
         MVC   WORK(1),DB.DBSELBK                                               
         MVC   WORK+1(2),=X'0101'                                               
         GOTO1 VDATCON,DMCB,(3,WORK),(0,WORK+6)                                 
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),(0,WORK+6),-1                          
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,WORK)                                 
         MVC   DB.DBSELBK(1),WORK                                               
         B     HARR0200                                                         
*                                                                               
HARR0220 DS    0H                                                               
         LTR   RB,RB               'FOUND' DATA: PERMIT A DUMP HERE             
HARR0240 DS    0H                                                               
         ZIC   RE,NUMQTRS          INDEX INTO BOOKS LIST                        
         SR    RE,R3                                                            
         MHI   RE,2                                                             
         LA    RE,BOOKS(RE)                                                     
         MVC   0(2,RE),DB.DBSELBK                                               
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HARR0120                                                      
         EJECT                                                                  
*------------------------------------------------------------                   
* GET MARKET CUMES                                                              
*------------------------------------------------------------                   
         ZIC   R3,NUMQTRS                                                       
         SR    R2,R2                                                            
*                                                                               
HARR0260 DS    0H                                                               
         LR    RE,R2                                                            
         MHI   RE,2                INDEX INTO BOOKS                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELBK,0(RE)                                                 
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'D'                                                 
         MVI   DB.DBFUNCT,DBGETDEM                                              
         MVC   DB.DBSELSRC,RTSRVC                                               
         EDIT  (B2,MARKET),(4,DB.DBSELSTA),FILL=0                               
         MVI   DB.DBSELSTA+4,C'T'                                               
         MVC   DB.DBSELDAY,=X'7F'               M-SU                            
         MVC   DB.DBSELTIM,=X'0258000C8'        6A-2A                           
         CLC   CABLESTA,SPACES     HAS AN ALIAS BEEN FOUND?                     
         BNH   HARR0270            NO                                           
****>>>  MVI   DB.DBBTYPE,C'C'     YES - SET TO 'CABLE OUTLET'                  
         CLC   CABLEMKT,SPACES                                                  
         BNH      *+10                                                          
         MVC   DB.DBSELMK,MARKET                                                
HARR0270 EQU   *                                                                
*                                                                               
*   TEST DUMP SITE                                                              
         LA    RF,MARKET                                                        
*                                                                               
         STC   R2,BYTE2            STORE QTR INDEX FOR THE HOOK                 
*                                                                               
*        GET CUMES FOR THE MARKET(BASIS CUMES)                                  
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,MKTHOOK,0                                 
         CLI   DB.DBERROR,0        ERROR?                                       
         B     *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HARR0260                                                      
         EJECT                                                                  
*------------------------------------------------------------                   
* GET STATION CUMES                                                             
*------------------------------------------------------------                   
         MVC   REMSTAS,NUMSTAS                                                  
         MVC   CURSTA,FRSTSTA                                                   
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         LA    R4,VHPSTA                                                        
         MVI   VHPSTIND,C'Y'       SET VHPSTA BEING PROCESSED                   
         DROP  RE                                                               
*                                                                               
HARR0280 DS    0H                                                               
         SR    R2,R2                                                            
*                                                                               
         ZIC   R3,NUMMONS          SET NUMBER OF MONTHS                         
*                                                                               
         CLI   LPMPASS,LPMMONS     LPM MONTHLY PASS?                            
         BE    HARR0300            YES - LOOP MONTHS                            
*                                  NO  - LOOP QUARTERS                          
         ZIC   R3,NUMQTRS          SET NUMBER OF QUARTERS                       
*                                                                               
HARR0300 DS    0H                                                               
         LR    RE,R2                                                            
         MHI   RE,2                INDEX INTO BOOKS                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELBK,0(RE)                                                 
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'D'                 DPT?                            
         MVI   DB.DBFUNCT,DBGETDEM              GET DEMO?                       
         MVC   DB.DBSELSRC,RTSRVC                                               
*                                                                               
*   R4 -> A STATION LIST.  EACH ENTRY IS 8 CHARS, SET LIKE THIS:                
*        POS 1 - 5    = STATION CALL LETTERS                                    
*        POS 6 - 7    = MARKET NUMBER (BINARY) OR ZEROS                         
*        POS 8        = CABLE (WIRED) INDICATOR, FOR DBBTYPE                    
*                                                                               
         MVC   DB.DBSELSTA(5),0(R4)                                             
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         CLI   VHPSTIND,C'Y'       VHPSTA IN PROCESS?                           
         BNE   HARR0310                                                         
*                                                                               
         CLC   CABLESTA,SPACES     HAS AN ALIAS BEEN FOUND?                     
         BNH   HARR0320            NO                                           
         MVC   DB.DBSELSTA,CABLESTA   YES - REPLACE STATION WITH ALIAS          
         MVC   DB.DBSELALF,CABLEMKT   REPLACE / INSERT ALPHA MKT                
****     MVI   DB.DBBTYPE,C'C'        SET TO 'CABLE OUTLET'                     
         CLC   CABLEMKT,SPACES                                                  
         BNH      *+10                                                          
         MVC   DB.DBSELMK,MARKET                                                
         B     HARR0320                                                         
HARR0310 EQU   *                                                                
         MVC   DB.DBSELMK,5(R4)    INSERT MARKET FROM LIST                      
***      MVC   DB.DBBTYPE,7(R4)    INSERT CABLE/WIRED BOOK TYPE                 
HARR0320 EQU   *                                                                
         STC   R2,BYTE2            STORE QTR INDEX FOR THE HOOK                 
*                                                                               
*        GET CUMES FOR THE STATION FOR ALL DAYPARTS/DEMOS                       
*                                                                               
         LARL  RF,STAHOOK                                                       
         ST    RF,FARADDR                                                       
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,FARADDR,0                                 
         CLI   DB.DBERROR,0        ERROR?                                       
         B     *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HARR0300                                                      
         B     HARR0340                                                         
****>>>>                                                                        
*                                                                               
*   PROCESS LPM MARKET CUMES : 12 BOOKS PER YEAR                                
*                                                                               
*                                                                               
HARR0330 DS    0H                                                               
         GOTO1 =A(MONBOOKS),DMCB,(RC),RR=Y                                      
*                                                                               
*   R4 PASSED OUT OF MONBOOKS IN 'DUB'                                          
*                                                                               
         L     R4,DUB              SET STATION IN PROGRESS                      
****>>>>                                                                        
*------------------------------------------------------------                   
*        SEND DATA FROM TABLE TO THE PC                                         
*------------------------------------------------------------                   
         LA    R3,1                                                             
HARR0340 DS    0H                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MHI   RE,3                                                             
         LA    RE,DEMOS(RE)                                                     
         CLI   1(RE),C'R'          RATING?                                      
         BNE   HARR0500            NO                                           
         CLI   LPMPASS,LPMMONS     LPM MONTHLY PASS                             
         BNE   HARR0350            NO  - NEXT TEST IS MONTHLY ONLY              
         GOTO1 =A(CUMECHK),DMCB,(RC),RR=Y                                       
*                                                                               
HARR0350 EQU   *                                                                
*                                                                               
*   BIG NOTE:  FOR MONTHLY DATA, THERE ARE TWO SITUATIONS THAT ARE              
*        ADDRESSED WITH REGARD TO EXISTENCE OF STATION CUMES.                   
*    1.  FOR THE STATION + DEMO, THERE ARE NO CUMES FOR ANY MONTH               
*        IN THE PROPOSAL.  IN THIS CASE, NO 1E OR 1F DATA WILL BE               
*        GENERATED.                                                             
*    2.  FOR THE STATION + DEMO, THERE ARE CUMES FOR AT LEAST ONE               
*        MONTH IN THE PROPOSAL.  IN THIS CASE, A 1E WILL BE GEN-                
*        ERATED, AND A 1F WILL BE GENERATED FOR EACH MONTH THAT                 
*        CONTAINS CUME DATA.                                                    
*    'SKIP1E1F' IS SET BY 'CUMECHK', WHICH PRE-SCANS THE INCOMING               
*        DATA.  "Y" INDICATES NO CUMES EXIST AT ALL FOR THE                     
*        STATION + DEMO.                                                        
*    A FURTHER CHECK WILL BE MADE AT THE DETAIL LEVEL IF A 1E IS                
*        BEING CREATED.                                                         
*    LAST ITEM:  AS THE TABLE IS CLEARED ON AN ITEM BY ITEM BASIS,              
*        THE CODING SIMPLY INHIBITS THE GENERATION OF THE DATA                  
*        ITEMS, RATHER THAN ATTEMPTING TO FACILITATE ANOTHER                    
*        METHOD OF CLEARING.                                                    
*                                                                               
         CLI   SKIP1E1F,C'Y'       NO STATION CUMES FOR DEMO?                   
         BE    HARR0360            NO CUMES FOUND - DON'T GENERATE 1E           
*                                  R/F INFO DATA ELEMENT                        
         GOTO1 ASETELEM,DMCB,AFABLK,RFIDATA,0                                   
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                                                               
*                                  STATION CALL LETTERS                         
         GOTO1 AADDDATA,DMCB,AFABLK,RFISTAEL,(R4),0                             
*                                                                               
         STC   R3,BYTE             DEMO SEQUENCE NUMBER                         
         GOTO1 AADDDATA,DMCB,AFABLK,RFIDEMEL,BYTE,0                             
*                                                                               
         CLI   ISCMT,C'Y'                                                       
         BNE   HARR0360                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RFICMTEL,0,0                                
*                                                                               
HARR0360 DS    0H                                                               
         LR    R2,R3                                                            
         BCTR  R2,0                                                             
         MHI   R2,CTDLENQ          INDEX TO DEMO                                
         LA    R2,CUMETAB(R2)                                                   
KILLIT   EQU   *                                                                
         USING CUMETAB,R2                                                       
*                                                                               
         LA    R5,1                INITIAL QUARTER OR MONTH                     
         CLI   LPMPASS,LPMMONS     LPM MONTHLY PASS?                            
         BE    HARR0370            YES                                          
*                                                                               
         ZIC   RE,STQTR            NO  - SET FOR QUARTERLY LOOP                 
         MVC   BYTE2,STQTR                                                      
         B     HARR0400                                                         
*                                                                               
HARR0370 DS    0H                  SET FOR MONTHLY LOOP                         
         ZIC   RE,STMONTH                                                       
         MVC   BYTE2,STMONTH                                                    
         B     HARR0400                                                         
*                                                                               
HARR0380 DS    0H                  MONTHLY LOOP                                 
         ZIC   RE,STMONTH                                                       
         AR    RE,R5                                                            
         BCTR  RE,0                                                             
         CHI   RE,12                                                            
         BNH   *+8                                                              
         AHI   RE,-12                                                           
         STC   RE,BYTE2                                                         
*                                                                               
         MVC   BYTE,ENYEAR                                                      
         CLC   STMONTH,BYTE2       CURRENT QTR BEFORE START?                    
         BNL   HARR0410            NO USE END YEAR                              
         B     HARR0400                                                         
*                                                                               
HARR0390 DS    0H                  QUARTERLY LOOP                               
         ZIC   RE,STQTR                                                         
         AR    RE,R5                                                            
         BCTR  RE,0                                                             
         CHI   RE,4                                                             
         BNH   *+8                                                              
         AHI   RE,-4                                                            
         STC   RE,BYTE2                                                         
*                                                                               
         MVC   BYTE,ENYEAR                                                      
         CLC   STQTR,BYTE2         CURRENT QTR BEFORE START?                    
         BNL   HARR0410            NO USE END YEAR                              
         B     HARR0400                                                         
*                                                                               
HARR0400 MVC   BYTE,STYEAR                                                      
*                                  R/F INFO QUARTER DATA ELEMENT                
HARR0410 EQU   *                                                                
         CLI   SKIP1E1F,C'Y'       NO STATION CUMES FOR DEMO?                   
         BE    HARR0480            NO CUMES FOUND - DON'T GENERATE 1F           
*                                                                               
*   THIS IS THE TEST FOR DETAIL LEVEL STATION CUMES EXISTENCE.                  
*                                                                               
         CLI   LPMPASS,LPMMONS     LPM MONTHLY PASS?                            
         BNE   HARR0415            NO  - DON'T CHECK CUMES                      
*                                                                               
         OC    CTSTAC(CTSLENQ),CTSTAC    ANY VALUE IN STATION CUMES?            
         BZ    HARR0480            NO  - DON'T CREATE 1F DATA                   
HARR0415 EQU   *                                                                
         GOTO1 ASETELEM,DMCB,AFABLK,RFQDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
*                                  ADD YEAR                                     
         GOTO1 AADDDATA,DMCB,AFABLK,RFQYREL,BYTE,0                              
*                                  ADD QUARTER OR MONTH                         
         CLI   LPMPASS,LPMMONS     LPM MONTHLY PASS                             
         BE    HARR0420            PROCESS LPM MONTHLY DATA                     
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RFQQTREL,BYTE2,0                            
         B     HARR0450                                                         
*                                                                               
HARR0420 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,RFQMONEL,BYTE2,0                            
*                                                                               
HARR0450 EQU   *                                                                
         L     RE,ATWA                                                          
         USING T81AFFD,RE                                                       
         CLC   VERSION,=X'01100011'    1.16.17+ GETS BOOK USED                  
         BL    HARR0460                                                         
         DROP  RE                                                               
*                                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MHI   RE,2                                                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,16+8           FAKE 16 BYTE FIELD                           
         MVC   WORK+16+8+1(2),0(RE)                                             
         GOTO1 VUNBOOK,DMCB,(1,WORK+16+8),WORK,0,                      +        
               (C'+',=CL6' ')                                                   
*                                                                               
         ZIC   RF,WORK                                                          
         BCTR  RF,0                                                             
         LA    RF,WORK(RF)                                                      
         CLI   0(RF),C' '          REMOVE SPACES                                
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RE,WORK                                                          
         SR    RF,RE                                                            
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RFQSWPEL,WORK+8,(RF)                        
*                                                                               
HARR0460 DS    0H                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MHI   RE,4                                                             
         LA    RE,MKTBASIS(RE)                                                  
         LR    R0,RE               ADD MARKET BASIS                             
         GOTO1 AADDDATA,DMCB,AFABLK,RFQBCUEL,(R0),0                             
*                                                                               
*                                  ADD MARKET CUME                              
         GOTO1 AADDDATA,DMCB,AFABLK,RFQMCUEL,CTMKTC,0                           
*                                                                               
*                                  ADD STATION CUME                             
         GOTO1 AADDDATA,DMCB,AFABLK,RFQSCUEL,CTSTAC,0                           
*                                                                               
*                                  ADD STATION RATING                           
         GOTO1 AADDDATA,DMCB,AFABLK,RFQSRAEL,CTSTAR,0                           
*                                                                               
         LA    R0,13                                                            
         LA    R6,CTDPTC                                                        
HARR0470 DS    0H                                                               
*                                  ADD DAYPART CUME                             
         GOTO1 AADDDATA,DMCB,AFABLK,RFQDCUEL,(R6),0                             
         LA    R6,4(R6)                                                         
         BCT   R0,HARR0470                                                      
*                                                                               
HARR0480 DS    0H                                                               
         XC    CTSTAC(CTSLENQ),CTSTAC         CLEAR STATION INFO                
         LA    R2,CTQLENQ(R2)                 BUMP TO NEXT MON/QTR              
*                                                                               
         CLI   LPMPASS,LPMMONS     LPM MONTHLY PASS?                            
         BNE   HARR0490            NO  - LOOP QUARTERS                          
*                                  YES - LOOP MONTHS                            
         CLM   R5,1,NUMMONS        END OF MONTHS?                               
         BNL   HARR0500            YES                                          
         LA    R5,1(R5)                                                         
         B     HARR0380            LOOP FOR NEXT MONTH                          
*                                                                               
*                                                                               
HARR0490 DS    0H                                                               
         CLM   R5,1,NUMQTRS        END OF QUARTERS?                             
         BNL   HARR0500            YES                                          
         LA    R5,1(R5)                                                         
         B     HARR0390            LOOP FOR NEXT QUARTER                        
*                                                                               
HARR0500 DS    0H                                                               
         CLM   R3,1,NUMDEMS        END OF DEMOS?                                
         BNL   *+12                YES                                          
         LA    R3,1(R3)                                                         
         B     HARR0340            LOOP FOR NEXT DEMO                           
         DROP  R2                                                               
*                                                                               
*        LOOP BACK FOR NEXT STATION                                             
*                                                                               
         MVI   VHPSTIND,C'N'       SET 'NOT VHPSTA'                             
         L     R4,CURSTA                                                        
         LA    RE,STALENQ(R4)                                                   
         ST    RE,CURSTA                                                        
         ZIC   RE,REMSTAS                                                       
         BCTR  RE,0                                                             
         STC   RE,REMSTAS                                                       
*&&DO                                                                           
*  TEST                                                                         
         CLI   LPMPASS,LPMSKIP     LPM MARKET?                                  
         BE    TESTHAR1            NO                                           
         CLI   LPMPASS,LPMQTRS     LPM QUARTERS IN PROGRESS?                    
         BE    TESTHAR1            YES - ONLY KILL FOR MONTHS                   
         L     RF,TESTCTR2                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TESTCTR2                                                      
         CLI   TESTCTR2+3,1                                                     
         BNE   TESTHAR1                                                         
***      LA    R0,13                                                            
***      LA    RF,BOOKS                                                         
***      LA    R2,CUMETAB                                                       
***      MVC   KILLIT(2),=X'0000'                                               
***      DC    H'0'                                                             
TESTHAR1 EQU   *                                                                
*                                                                               
*  TEST END                                                                     
*&&                                                                             
         LTR   RE,RE                                                            
         BNM   HARR0280                                                         
HARR0520 EQU   *                                                                
         CLI   LPMPASS,LPMSKIP     LPM MARKET?                                  
         BE    HARRISX             NO  - FINISHED                               
         CLI   LPMPASS,LPMMONS     LPM MONTHS COMPLETED?                        
         BE    HARRISX             YES - FINISHED                               
         CLI   LPMPASS,LPMQTRS     LPM QUARTERS IN PROGRESS?                    
         BE    *+6                 YES                                          
         DC    H'0'                NO  - UNRECOGNIZED VALUE                     
         MVI   LPMPASS,C'M'        SET TO PROCESS MONTHS                        
         L     RE,AIO2                                                          
         LHI   RF,LENIO             IN THE MARKET IN AIO2                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   FRSTSTA,FRSTSTA2    RESET FIRST STATION TO ORIG VALUE            
*                                                                               
         B     HARR0070            CYCLE EVERYTHING AGAIN FOR MONTHLY           
*                                                                               
HARRISX  DS    0H                                                               
         B     EXITOK                                                           
TESTCTR  DC    XL1'0'                                                           
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*   TEMPORARY VALIDATION ROUTINE TO DETERMINE IF USERID ALLOWED ACCESS          
*     TO NIELSEN DATA FOR REQUESTED MARKET                                      
*                                                                               
***********************************************************************         
VAL4MKT  NTR1                                                                   
         LA    R1,USRMKTAB                                                      
VMKT010  CLI   0(R1),X'FF'       END OF TABLE?                                  
         BE    MKTOKAY           YES, NO RESTRICTIONS FOUND                     
         CLC   REPALPHA,0(R1)    MATCH ON REPCODE?                              
         BE    VMKT050           YES, BRANCH TO NEXT STEP                       
         ZICM  R2,2(R1),2        ELSE, PUT ENTRY LENGTH IN R2                   
         LA    R1,0(R2,R1)       BUMP R1 TO NEXT TABLE ENTRY                    
         B     VMKT010                                                          
                                                                                
*        THIS REPALPHA HAS RESTRICTIONS, CHK IF 1 FOR THIS ID                   
VMKT050  L     RE,ATWA           POINT RE AT TWA                                
         USING TWAD,RE                                                          
         ZICM  R2,2(R1),2        ENTRY LENGTH IN R2                             
         LA    R2,0(R2,R1)       POINT R2 AT END OF THIS ENTRY                  
         LA    R1,4(R1)          POINT R1 TO 1ST ID/MKT SUB ENTRY               
         SR    R3,R3             INITIALIZE SUB-ENTRY LENGTH HOLDER             
                                                                                
VMKT100  LA    R1,0(R3,R1)       POINT R1 TO NEXT SUB ENTRY                     
         CR    R1,R2             HAVE WE REACHED END OF MAIN ENTRY?             
         BNL   MKTOKAY           YES, NO RESTRICTION FOUND                      
         ZIC   R3,2(R1)          PUT SUB-ENTRY LENGTH IN R3                     
         CLC   TWAUSRID,0(R1)    MATCH ON USER ID?                              
         BNE   VMKT100           NO, CHECK NEXT                                 
                                                                                
         LA    R2,0(R3,R1)       POINT R2 AT END OF SUB ENTRY                   
         LA    R1,3(R1)          POINT R1 AT FIRST ALLOWED MKT                  
                                                                                
VMKT150  CR    R1,R2             HAVE WE REACHED END OF SUB ENTRY?              
         BNL   MKTNOTOK          YES, NO MATCH FOUND FOR CURRENT MKT            
         CLC   MARKET,0(R1)      NO,  CHK MATCH ON ALLOWED MARKET               
         BE    MKTOKAY                YES                                       
         LA    R1,2(R1)               NO, BUMP TO NEXT ALLOWED MKT              
         B     VMKT150                    AND REPEAT CHECK                      
         DROP  RE                                                               
                                                                                
MKTOKAY  SR    RC,RC                     SET CC CODE TO =                       
MKTNOTOK LTR   RC,RC                     SET CC CODE TO !=                      
         XIT1                                                                   
*  TABLE OF NIELSEN MARKET DATA RESTRICTIONS BY USERID                          
*    TABLE STRUCTURE:                                                           
*    REP ALPHA CODE CL2   ENTRY LENGTH AL2                                      
*        VARIABLE # OF 'LIMIT SUB-ENTRIES' OF TYPE:                             
*                       USER ID                        XL2                      
*                       SIZE OF SUB ENTRY              AL1                      
*                       MARKET CODES OF ALLOWED MKTS   XL2                      
*                       ALLOWS VARIABLE# VALID LOCAL MARKETS FOR USERID         
*                                                                               
USRMKTAB DS    0H                                                               
UV       DC    C'UV',AL2(UVX-UV)                                                
UV001    DC    X'1E91',AL1(UV002-UV001)   KUVSL=NO VALID MKTS                   
UV002    DC    X'1E8E',AL1(UV003-UV002)   KMEXL=NO VALID MKTS                   
UV003    DC    X'1E95',AL1(UV004-UV003)   WLTVL=NO VALID MKTS                   
UV004    DC    X'1E8F',AL1(UV005-UV004)   KTVWL=NO VALID MKTS                   
UV005    DC    X'1E93',AL1(UV006-UV005)   KXLNL=NO VALID MKTS                   
UV006    DC    X'1E8C',AL1(UV007-UV006)   KDTVL=NO VALID MKTS                   
UV007    DC    X'1E96',AL1(UV008-UV007)   WXTVL=NO VALID MKTS                   
UV008    DC    X'1F32',AL1(UV009-UV008)   KABEL=NO VALID MKTS                   
UV009    DC    X'1F64',AL1(UV010-UV009)   KUVIL=NO VALID MKTS                   
UV010    DC    X'1E86',AL1(UV011-UV010)   KFTVL=NO VALID MKTS                   
UV011    DC    X'1F33',AL1(UV012-UV011)   KUVEL=NO VALID MKTS                   
UV012    DC    X'1E90',AL1(UV013-UV012)   KUVNL=NO VALID MKTS                   
UV013    DC    X'1E94',AL1(UV014-UV013)   WGBOL=NO VALID MKTS                   
                                                                                
UV014    DC    X'179F',AL1(UV015-UV014),X'0065019301D2' UNAT=NY,LA,FRS          
UV015    DC    X'0799',AL1(UV016-UV015),X'0065019301D2' UNCH=NY,LA,FRS          
UV016    DC    X'0796',AL1(UV017-UV016),X'0065019301D2' UNDA=NY,LA,FRS          
UV017    DC    X'079A',AL1(UV018-UV017),X'0065019301D2' UNDE=NY,LA,FRS          
UV018    DC    X'0797',AL1(UV019-UV018),X'0065019301D2' UNLA=NY,LA,FRS          
UV019    DC    X'0798',AL1(UV020-UV019),X'0065019301D2' UNMI=NY,LA,FRS          
UV020    DC    X'078C',AL1(UV021-UV020),X'0065019301D20080'                     
*     UNNY=NY,LA,FRA,MIAMI                                                      
UV021    DC    X'174A',AL1(UV022-UV021),X'0065019301D2' UNSA=NY,LA,FRS          
UV022    DC    X'079C',AL1(UVX-UV022),X'0065019301D2'   UNSF=NY,LA,FRS          
UVX      DS    0H                                                               
         DC    X'FF'              END OF TABLE                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   CUMECHK:                                                                    
*        FOR MONTHLY DATA ONLY, CHECK ALL THE STATION CUMES                     
*        BY MONTH FOR EACH DEMO.  IF ALL ARE ZERO,                              
*        SKIP OVER THE OUTPUT OF BOTH THE 1E AND 1F DATA ELEMENTS.              
***********************************************************************         
CUMECHK  NTR1  LABEL=*,BASE=*                                                   
         MVI   SKIP1E1F,C'N'       SET 'SKIP=NO'                                
         ZIC   R0,NUMMONS          SET LOOP                                     
         LR    R2,R3               DEMO NUMBER IN PROGRESS                      
         BCTR  R2,0                ZERO RELATIVE                                
         MHI   R2,CTDLENQ          INDEX TO DEMO                                
         LA    R2,CUMETAB(R2)                                                   
         USING CUMETAB,R2                                                       
CUME0020 EQU   *                                                                
         OC    CTSTAC(CTSLENQ),CTSTAC    ANY VALUE IN STATION CUMES?            
         BNZ   CUME0100            YES - RETURN 'DATA FOUND'                    
         LA    R2,CTQLENQ(R2)      NO  - BUMP TO NEXT MONTH'S DATA              
         BCT   R0,CUME0020         PROCESS ALL MONTHS                           
*                                  DROP-THROUGH:  NO DATA IN ANY MONTH          
         MVI   SKIP1E1F,C'Y'       SET 'SKIP=Y'                                 
         SR    R0,R0               SET CC = ZERO: NO DATA FOUND                 
CUME0100 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET OVERRIDE ROUTINE                                                          
*         (R2) MUST CONTAIN QUARTER (INDEX) FOR QTRS TABLE                      
*         BYTE 1 OF DB.DBSELBK MUST CONTAIN YEAR FOR OVERRIDE SEARCH            
***********************************************************************         
GETOVER  NTR1  LABEL=*,BASE=*                                                   
                                                                                
**           BUILD SEARCH UNIT FOR OVERIDE LOOKUP                               
                                                                                
         MVC   OVYEAR,DB.DBSELBK   PLACE SEARCH YEAR                            
*                                                                               
*                     (R2) IS INDEX FOR QTRS TAB, AND IS 1 LESS THAN            
         AHI   R2,1           THE ACTUAL CURRENT QUARTER NUMBER                 
         CHI   R2,4           R2 MAY BE > 4 INDICATING QTR IN ENDYEAR           
         BNH   *+8                                                              
         AHI   R2,-4               IF SO, COMPENSATE                            
         STC   R2,OVQTR            PLACE SEARCH QUARTER                         
                                                                                
         XC    KEY,KEY                   BUILD SEARCH KEY                       
         LA    R4,KEY                                                           
         USING RCUMKEY,R4                                                       
         MVI   RCUMTYP,RCUMTYQ                TYPE                              
         MVI   RCUMSTY,RCUMSTQ                SUBTYPE                           
         MVC   RCUMREP,REPALPHA               REPCODE                           
         EDIT  (B2,MARKET),(4,RCUMMKT),FILL=0 1ST 4 ALPHANUM OF MKTCODE         
         MVI   RCUMMKT+4,C' '                 5TH CHAR IS SPACE                 
         DROP  R4                                                               
                                                                                
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'REPDIR',KEYSAVE,KEY,0               
         CLC   KEY(RCUCMPLQ),KEYSAVE     FOUND?                                 
         BNE   OVNOTFND                  NO, NO RECORD FOR THIS MARKET          
                                                                                
         GOTO1 VDMGR,DMCB,(0,=C'GETREC'),=C'REPFILE',                  +        
               KEY+(RCUMDA-RCUMKEY),AIO4,DMWORK                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO4                                                          
         MVC   DATADISP,=AL2(RCU1ST)     DISTANCE TO 1ST ELEMENT                
         MVI   MYELCODE,RCUMOVEQ         ALIAS ELEMENT CODE                     
         BRAS  RE,GETEL                                                         
         BNE   OVNOTFND                  NO OVERRIDE ELEMENT                    
         USING RCUMOVER,R4                                                      
                                                                                
GETOV050 CLC   OVDATE,RCUMOVYR           IF ELEMENT MATCHES SEARCH DATE         
         BNE   GETOV100                                                         
         MVC   DB.DBSELBK,RCUMBOOK+1     INSRT OVERRIDE BOOK YR/MNTH            
         B     OVFOUND                   AND EXIT WITH CC SET EQUAL             
                                                                                
GETOV100 BRAS  RE,NEXTEL                 ELSE, LOOK FOR ANOTHER ELMNT           
         BNE   OVNOTFND                  IF NOT FOUND, THEN NO OVERRIDE         
         B     GETOV050                  ELSE, CHECK FOR MATCH                  
         DROP  R4                                                               
                                                                                
OVFOUND  SR    RC,RC                     SET CC CODE TO =                       
OVNOTFND LTR   RC,RC                     SET CC CODE TO !=                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
***********************************************************************         
BOOKHOOK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   LATESTBK,DB.DBACTBK                                              
*                                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
***********************************************************************         
LISTHOOK NTR1  BASE=*,LABEL=*                                                   
         L     R4,DB.DBAREC                                                     
         USING MLKEY,R4                                                         
*                                                                               
         OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
         BNZ   LSTHX1              YES - IGNORE                                 
*                                                                               
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    LSTHX1              YES - IGNORE                                 
*                                                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
********                                                                        
         CLC   =C'USA',MLSTAT                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'CUSA',MLSTAT                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
********                                                                        
         CLC   VHPSTA,MLSTAT       TEST STATION = REQUEST STATION               
         BE    LSTHX2              YES - IGNORE                                 
         DROP  RE                                                               
*                                                                               
         L     RE,FRSTSTA          USE DISPLACEMENT FOR ADDRESS                 
         A     RE,OVPARMS+4                                                     
         ZIC   R0,NUMSTAS                                                       
LSTH010  DS    0H                  IS STATION IN THE REQUEST?                   
         LA    RF,3                                                             
         CLI   4(RE),C' '          ADJUST COMPARE                               
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),MLSTAT                                                   
         BE    LSTH020             YES                                          
*                                                                               
         LA    RE,5(RE)            BUMP TO NEXT STATION                         
         BCT   R0,LSTH010                                                       
         B     LSTHX3              NO - EXIT                                    
*                                                                               
LSTH020  DS    0H                                                               
*                                                                               
*   CHANGING LENGTH OF STATION AREA IN AIO2 TO EIGHT CHARACTERS,                
*        TO PROVIDE ADDITIONAL SPACE FOR CABLE DATA NEEDED:                     
*           1.  ALTERNATE MARKET NUMBER                                         
*           2.  CABLE INDICATOR                                                 
*                                                                               
         L     RE,AIO2                                                          
         ZIC   R1,0(RE)            GET STATION COUNT                            
         LA    R0,1(R1)            BUMP BY ONE                                  
         STC   R0,0(RE)            AND STORE                                    
*                                                                               
         MHI   R1,8                INDEX TO NEXT SPOT                           
         LA    RE,1(R1,RE)         BUMP PAST LENGTH                             
         MVC   0(5,RE),MLSTAT      STORE STATION                                
         CLI   4(RE),C'T'                                                       
         BNE   *+8                                                              
         MVI   4(RE),C' '                                                       
         XC    5(3,RE),5(RE)       SET NEW CHARS TO BIN ZERO                    
         B     LSTHX4                                                           
*                                                                               
LSTHX1   DS    0H                  NO CHECK DONE                                
         LA    R0,1                                                             
         B     LSTHXIT                                                          
LSTHX2   DS    0H                  TEST STATION = REQUEST STATION               
         LA    R0,2                                                             
         B     LSTHXIT                                                          
LSTHX3   DS    0H                  STATION NOT IN REQUEST                       
         LA    R0,3                                                             
         B     LSTHXIT                                                          
LSTHX4   DS    0H                  STATION IN REQUEST, ADDED TO LIST            
         LA    R0,4                                                             
         B     LSTHXIT                                                          
LSTHXIT  DS    0H                  NO CHECK DONE                                
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
***********************************************************************         
LISTHK2  NTR1  BASE=*,LABEL=*                                                   
         L     R4,DB.DBAREC                                                     
         USING MLKEY,R4                                                         
*                                                                               
*   CABLE STATIONS ALL SEEM TO BE SPILL:  ACCEPT THE RECORDS                    
*        RATHER THAN SKIPPING THEM.                                             
*                                                                               
***      OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
***      BNZ   LHK20100            YES - IGNORE                                 
*                                                                               
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    LHK20100            YES - IGNORE                                 
*                                                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
*&&DO                                                                           
*   TEST                                                                        
         CLC   =C'ESPN',MLSTAT     TEST FOR ESPN                                
         BNE   TEST0020            NOT ESPN                                     
         MVC   DIE2(2),=X'0000'    YES - KILL THE JOB                           
TEST0020 EQU   *                                                                
*   TEST END                                                                    
*&&                                                                             
         CLC   VHPSTA,MLSTAT       TEST STATION = REQUEST STATION               
         BE    LHK20120            YES - IGNORE                                 
         DROP  RE                                                               
*                                                                               
         LA    RE,CCBLASTA         SET A(ALIAS TABLE LIST)                      
         LA    R0,MAXCABLE         SET LOOP                                     
LHK20020 DS    0H                  IS STATION IN THE REQUEST?                   
         LA    RF,3                                                             
         CLI   DCABMED(RE),C' '    ADJUST COMPARE                               
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         EX    RF,*+8                                                           
         B     LHK20030                                                         
DIE2     EQU   *                                                                
         CLC   DCABALIA(0,RE),MLSTAT                                            
LHK20030 DS    0H                  IS STATION IN THE REQUEST?                   
         BE    LHK20040            YES                                          
*                                                                               
         LA    RE,LCCBLLEN(RE)     BUMP TO NEXT SLOT                            
         OC    0(4,RE),0(RE)       ANY ENTRY IN SLOT?                           
         BZ    LHK20130            NO  - EXIT                                   
         BCT   R0,LHK20020         YES - CHECK NEXT SLOT                        
         B     LHK20130            NO - EXIT                                    
*                                                                               
LHK20040 DS    0H                                                               
*                                                                               
*   CHANGING LENGTH OF STATION AREA IN AIO2 TO EIGHT CHARACTERS,                
*        TO PROVIDE ADDITIONAL SPACE FOR CABLE DATA NEEDED:                     
*           1.  ALTERNATE MARKET NUMBER                                         
*           2.  CABLE INDICATOR                                                 
*                                                                               
         L     RE,AIO2                                                          
         ZIC   R1,0(RE)            GET STATION COUNT                            
         LA    R0,1(R1)            BUMP BY ONE                                  
         STC   R0,0(RE)            AND STORE                                    
*                                                                               
         MHI   R1,STALENQ          INDEX TO NEXT SPOT                           
         LA    RE,1(R1,RE)         BUMP PAST LENGTH                             
         MVC   0(5,RE),MLSTAT                                                   
         CLI   4(RE),C'T'                                                       
         BNE   *+8                                                              
         MVI   4(RE),C' '                                                       
         MVC   5(2,RE),MLRMKT      INSERT MARKET NUMBER INTO TABLE              
         MVI   7(RE),C'W'          INDICATE CABLE: RETRIEVE "W" (WIRED)         
********                                                                        
         CLC   =C'USA',MLSTAT                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'CUSA',MLSTAT                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
********                                                                        
         B     LHK20140                                                         
*                                                                               
LHK20100 DS    0H                  NO CHECK DONE                                
         LA    R0,1                                                             
         B     LHK20900                                                         
LHK20120 DS    0H                  TEST STATION = REQUEST STATION               
         LA    R0,2                                                             
         B     LHK20900                                                         
LHK20130 DS    0H                  STATION NOT IN REQUEST                       
         LA    R0,3                                                             
         B     LHK20900                                                         
LHK20140 DS    0H                  STATION IN REQUEST, ADDED TO LIST            
         LA    R0,4                                                             
         B     LHK20900                                                         
LHK20900 DS    0H                  NO CHECK DONE                                
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR SIGN ON TO SIGN OFF CUMES FOR THE MARKET                      
*    - P2+ CUME                                                                 
*    - DEMO SPECIFIC CUMES FOR THE REQUEST                                      
***********************************************************************         
MKTHOOK  NTR1  BASE=*,LABEL=*                                                   
         L     RE,DB.DBAQUART                                                   
         CLC   =X'17044C',2(RE)                                                 
         BNE   MKTHX                                                            
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QTR/MON IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
MKH110   DS    0H                                                               
         MVC   CTMKTC,0(R5)        COPY DEMO VALUE FOR MARKET CUME              
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,MKH110                                                        
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'D',=X'00C37F'),DB.DBLOCK,AIO3,0                  
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET MON/QTR INDEX                            
         MHI   R6,4                                                             
         LA    R6,MKTBASIS(R6)     POINT TO THE MON/QTR                         
         MVC   0(4,R6),0(R5)                                                    
*                                                                               
MKTHX    DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING STATION CUME DATA FOR ALL DEMOS                    
*   SIGN ON TO SIGN OFF CUME (6A-2A)                                            
*   13 HARRIS DAYPARTS BASED ON TIME ZONE                                       
*                                                                               
***********************************************************************         
STAHOOK  NTR1  BASE=*,LABEL=*                                                   
         CLI   ISCMT,0                                                          
         BNE   LSTH004                                                          
*                                                                               
         MVI   ISCMT,C'N'          SET TO EASTERN/PACIFIC                       
         L     RE,DB.DBAREC                                                     
         AH    RE,DB.DBDTADSP                                                   
LSTH000  CLI   0(RE),X'00'         FIND DIELEM                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'03'                                                      
         BE    LSTH002                                                          
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     LSTH000                                                          
*                                                                               
LSTH002  DS    0H                                                               
         USING DIELEM,RE                                                        
         CLI   DITZ,C'1'           EASTERN?                                     
         BE    LSTH004                                                          
         CLI   DITZ,C'4'           PACIFIC?                                     
         BE    LSTH004                                                          
         DROP  RE                                                               
*                                                                               
         MVI   ISCMT,C'Y'          NO                                           
*                                                                               
LSTH004  DS    0H                                                               
         LA    R1,EPTDPTS                                                       
         CLI   ISCMT,C'Y'                                                       
         BNE   *+8                                                              
         LA    R1,CMTDPTS                                                       
         L     RE,DB.DBAQUART                                                   
STH010   DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    STH100                                                           
         CLC   2(LENDTABQ,RE),0(R1)                                             
         BE    STH020                                                           
         LA    R1,LENDTABQ(R1)                                                  
         B     STH010                                                           
*                                                                               
STH020   DS    0H                                                               
         LA    R0,EPTDPTS                                                       
         CLI   ISCMT,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,CMTDPTS                                                       
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         LA    RF,LENDTABQ                                                      
         DR    R0,RF                                                            
         LR    R3,R1               INDEX INTO DAYPARTS                          
         MHI   R3,4                                                             
*                                                                               
**TEST                                                                          
*        CLI   BYTE2,1                                                          
*        BNE   STH020X                                                          
*        L     RF,DB.DBAQUART                                                   
*        LA    RF,2(RF)                                                         
*        GOTO1 VHEXOUT,DMCB,(RF),WORK,LENDTABQ,0                                
*        GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
*        OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*        GOTO1 AADDDATA,DMCB,AFABLK,INVNUMEL,WORK,LENDTABQ*2                    
*TH020X  DS    0H                                                               
**TEST                                                                          
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
STH030   DS    0H                                                               
         LA    RE,CTDPTC(R3)       INDEX TO CORRECT DAYPART                     
         MVC   0(4,RE),0(R5)       COPY DEMO VALUE                              
*                                                                               
         CHI   R3,DUPLIQ*4         CHECK IF ITS THE DUPLICATE                   
         BNE   *+10                                                             
         MVC   OFFSETQ(4,RE),0(R5)                                              
*                                                                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,STH030                                                        
         DROP  R6                                                               
*                                                                               
STH100   DS    0H                                                               
         L     RE,DB.DBAQUART                                                   
         CLC   =X'17044C',2(RE)                                                 
         BNE   STAHX                                                            
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
STH110   DS    0H                                                               
         MVC   CTSTAC,0(R5)        COPY DEMO VALUE FOR STATION CUME             
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,STH110                                                        
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',DEMOS),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
STH120   DS    0H                                                               
         MVC   CTSTAR,0(R5)        COPY DEMO VALUE FOR STATION CUME             
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,STH120                                                        
         DROP  R6                                                               
*                                                                               
STAHX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SETLPMKT:                                                                     
*        LPM MARKETS THAT ACCESS 12 BOOKS / YEAR WILL HAVE TO BE                
*        INSERTED INTO THE FOLLOWING TABLE TO SET A PROCESSING FLAG.            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
SETLPMKT NTR1  LABEL=*,BASE=*                                                   
         MVI   LPMMKT,C'N'         SET 'NOT LPM MKT'                            
         MVI   LPMPASS,C'N'        SET 'NOT LPM MKT'                            
         LA    R3,LPMMKTAB         SET A(MARKET TABLE)                          
SLPM0020 EQU   *                                                                
****     CLC   0(2,R3),X'0000'     END OF TABLE REACHED?                        
         CLI   0(R3),X'FF'         END OF TABLE REACHED?                        
         BE    SLPM0200            YES - EXIT 'NOT LPM MKT'                     
         CLC   MARKET,0(R3)        RESOLVED MARKET NUMBER FOUND?                
         BE    SLPM0040            YES -                                        
         LA    R3,LPMMKTBL(R3)     NO  - BUMP TO NEXT TABLE POS                 
         B     SLPM0020            GO BACK FOR NEXT                             
SLPM0040 EQU   *                                                                
         MVI   LPMMKT,C'Y'         SET 'LPM MKT'                                
         MVI   LPMPASS,C'Q'        SET 'QTRS PASS'                              
SLPM0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
LPMMKTAB EQU   *                                                                
         DC    AL2(168)            ATLANTA                                      
LPMMKTBL EQU   *-LPMMKTAB          LENGTH ATTRIBUTE                             
         DC    AL2(106)            BOSTON                                       
         DC    AL2(202)            CHICAGO                                      
         DC    AL2(223)            DALLAS                                       
         DC    AL2(105)            DETROIT                                      
         DC    AL2(403)            LOS ANGELES                                  
         DC    AL2(101)            NEW YORK                                     
         DC    AL2(104)            PHILADELPHIA                                 
         DC    AL2(407)            SAN FRANCISCO                                
         DC    AL2(111)            WASHINGTON, DC                               
*                                                                               
*   'PARTIAL' MARKETS (NOT 12 MONTHS)                                           
*                                                                               
         DC    AL2(112)            BALTIMORE                                    
         DC    AL2(115)            CINCINNATI                                   
         DC    AL2(110)            CLEVELAND                                    
         DC    AL2(351)            DENVER                                       
         DC    AL2(133)            HARTFORD / NEW HAVEN                         
         DC    AL2(218)            HOUSTON                                      
         DC    AL2(127)            INDIANAPOLIS                                 
         DC    AL2(216)            KANSAS CITY                                  
         DC    AL2(128)            MIAMI                                        
         DC    AL2(217)            MILWAUKEE                                    
         DC    AL2(213)            MINNEAPOLIS                                  
         DC    AL2(108)            PITTSBURGH                                   
         DC    AL2(419)            SEATTLE                                      
         DC    AL2(209)            ST LOUIS                                     
*                                                                               
         DC    X'FFFF'             DELIMITER                                    
***      DC    X'0000'             DELIMITER                                    
         DS    0F                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DAYPART TABLE FOR HARRIS MODEL                                                
***********************************************************************         
EPTDPTS  DS    0D                          EASTERN/PACIFIC TIME                 
         DC    X'15000C'                                                        
LENDTABQ EQU   *-EPTDPTS                                                        
         DC    X'150C18'                                                        
         DC    X'151828'                                                        
         DC    X'152830'                                                        
         DC    X'153038'                                                        
         DC    X'173845'                                                        
         DC    X'174446'                                                        
         DC    X'15464C'                                                        
         DC    X'FFFFFF'           N/A                                          
DUPLIQ   EQU   (*-EPTDPTS)/LENDTABQ                                             
         DC    X'600418'                                                        
         DC    X'60182C'                                                        
OFFSETQ  EQU   (((*-EPTDPTS)/LENDTABQ)-DUPLIQ)*4                                
         DC    X'600418'                                                        
         DC    X'701C30'                                                        
         DC    X'00'                                                            
CMTDPTS  DS    0D                          CENTRAL/MOUNTAIN TIME                
         DC    X'15000C'                                                        
         DC    X'150C18'                                                        
         DC    X'151824'                                                        
         DC    X'15242C'                                                        
         DC    X'152C34'                                                        
         DC    X'173441'                                                        
         DC    X'174042'                                                        
         DC    X'154248'                                                        
         DC    X'FFFFFF'           N/A                                          
         DC    X'600418'                                                        
         DC    X'60182C'                                                        
         DC    X'600418'                                                        
         DC    X'701C30'                                                        
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
**<<<>>>                                                                        
*                                                                               
*   MONBOOKS:  CONVERT NUMBER OF QUARTERS TO NUMBER OF MONTHS,                  
*        SET UP BOOK LIST BASED ON THAT.                                        
*        THIS WILL BE CONVERTABLE FOR SPECIAL BOOKS                             
*                                                                               
MONBOOKS NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         ZIC   RF,STMONTH          SET START MONTH                              
         ZIC   RE,ENMONTH          SET END  MONTH                               
         CLC   STMONTH,ENMONTH     START MONTH > ENDMONTH?                      
         BNH   HLPM0020            NO  - NO YEAR ADJUSTMENT                     
         LA    RE,12(RE)           YES - ADD 12 MONTHS                          
HLPM0020 EQU   *                                                                
         LA    RE,1(RE)            MAKE # MONTHS INCLUSIVE                      
         SR    RE,RF               SUBTRACT START MONTH FROM END MONTH          
         STC   RE,NUMMONS                                                       
         LR    R3,RE               LOAD # MONTHS AS SET LOOP CONTROL            
*                                                                               
         ZIC   R2,STMONTH                                                       
         BCTR  R2,0                MAKE ZERO RELATIVE                           
         LA    RE,MONS(R2)         INDEX INTO MONTHS TABLE                      
         MVC   BYTE,0(RE)                                                       
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         B     HLPM0160                                                         
*                                                                               
HLPM0120 DS    0H                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
*                                                                               
         LA    RE,MONS(R2)         INDEX INTO MONTHS TABLE                      
         MVC   DB.DBSELBK(1),ENYEAR                                             
         CLC   BYTE,0(RE)          CURRENT MONTH BEFORE START?                  
         BNL   *+10                NO USE END YEAR                              
HLPM0160 MVC   DB.DBSELBK(1),STYEAR                                             
         MVC   DB.DBSELBK+1(1),0(RE)                                            
*                                                                               
*              SUPPORT CUME OVERRIDE RECORDS                                    
         GOTO1 =A(GETOVER),RR=Y          LOOK FOR OVERRIDE BOOK                 
         BE    HLPM0240                                                         
*                                                                               
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVI   DB.DBTPTT,C'T'                                                   
         MVI   DB.DBFUNCT,DBVLSTBK                                              
         MVC   DB.DBSELSRC,RTSRVC                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         MVC   DB.DBSELSTA,VHPSTA                                               
         DROP  RE                                                               
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         CLC   CABLESTA,SPACES     HAS AN ALIAS BEEN FOUND?                     
         BNH   HLPM0180            NO                                           
         MVC   DB.DBSELSTA,CABLESTA   YES - REPLACE STATION WITH ALIAS          
         MVC   DB.DBSELALF,CABLEMKT   REPLACE / INSERT ALPHA MKT                
         MVI   DB.DBBTYPE,C'C'        SET TO 'CABLE OUTLET'                     
HLPM0180 EQU   *                                                                
         MVC   DB.DBSELDAY,=X'40'               M                               
         MVC   DB.DBSELTIM,=X'07D007DF'         8-815P                          
*                                                                               
*        SEE IF THE BOOK IS LOADED                                              
*                                                                               
         MVI   WORK+4,1            TRY COUNT                                    
HLPM0200 DS    0H                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,0,0                                       
         CLI   DB.DBERROR,0        ERROR?                                       
         BE    HLPM0220            NO                                           
*                                                                               
         CLI   DB.DBERROR,16       NOT FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         CLI   WORK+4,5            ONLY ALLOW 5 TRIES                           
         BNL   HLPM0240            JUST USE THIS BOOK                           
*                                                                               
         ZIC   RE,WORK+4                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WORK+4                                                        
*                                                                               
         MVC   WORK(1),DB.DBSELBK                                               
         MVC   WORK+1(2),=X'0101'                                               
         GOTO1 VDATCON,DMCB,(3,WORK),(0,WORK+6)                                 
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),(0,WORK+6),-1                          
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,WORK)                                 
         MVC   DB.DBSELBK(1),WORK                                               
         B     HLPM0200                                                         
*                                                                               
HLPM0220 DS    0H                                                               
         LTR   RB,RB               'FOUND' DATA: PERMIT A DUMP HERE             
HLPM0240 DS    0H                                                               
         ZIC   RE,NUMMONS          INDEX INTO BOOKS LIST                        
         SR    RE,R3                                                            
         MHI   RE,2                                                             
         LA    RE,BOOKS(RE)                                                     
         MVC   0(2,RE),DB.DBSELBK                                               
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HLPM0120                                                      
         EJECT                                                                  
*------------------------------------------------------------                   
* GET MARKET CUMES                                                              
*------------------------------------------------------------                   
         ZIC   R3,NUMMONS                                                       
         SR    R2,R2                                                            
*                                                                               
HLPM0260 DS    0H                                                               
         LR    RE,R2                                                            
         MHI   RE,2                INDEX INTO BOOKS                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELBK,0(RE)                                                 
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'D'                                                 
         MVI   DB.DBFUNCT,DBGETDEM                                              
         MVC   DB.DBSELSRC,RTSRVC                                               
         EDIT  (B2,MARKET),(4,DB.DBSELSTA),FILL=0                               
         MVI   DB.DBSELSTA+4,C'T'                                               
         MVC   DB.DBSELDAY,=X'7F'               M-SU                            
         MVC   DB.DBSELTIM,=X'0258000C8'        6A-2A                           
*                                                                               
         STC   R2,BYTE2            STORE MON INDEX FOR THE HOOK                 
*                                                                               
*        GET CUMES FOR THE MARKET(BASIS CUMES)                                  
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,MKTHOOK,0                                 
         CLI   DB.DBERROR,0        ERROR?                                       
         B     *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HLPM0260                                                      
         EJECT                                                                  
*------------------------------------------------------------                   
* GET STATION CUMES                                                             
*------------------------------------------------------------                   
         MVC   REMSTAS,NUMSTAS                                                  
         MVC   CURSTA,FRSTSTA                                                   
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         LA    R4,VHPSTA                                                        
         DROP  RE                                                               
*                                                                               
HLPM0280 DS    0H                                                               
         ZIC   R3,NUMMONS                                                       
         SR    R2,R2                                                            
*                                                                               
HLPM0300 DS    0H                                                               
         LR    RE,R2                                                            
         MHI   RE,2                INDEX INTO BOOKS                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELBK,0(RE)                                                 
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'D'                 DPT?                            
         MVI   DB.DBFUNCT,DBGETDEM              GET DEMO?                       
         MVC   DB.DBSELSRC,RTSRVC                                               
         MVC   DB.DBSELSTA(5),0(R4)                                             
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
*                                                                               
         CLC   CABLESTA,SPACES     HAS AN ALIAS BEEN FOUND?                     
         BNH   HLPM0320            NO                                           
         MVC   DB.DBSELSTA,CABLESTA   YES - REPLACE STATION WITH ALIAS          
         MVC   DB.DBSELALF,CABLEMKT   REPLACE / INSERT ALPHA MKT                
         MVI   DB.DBBTYPE,C'C'        SET TO 'CABLE OUTLET'                     
HLPM0320 EQU   *                                                                
         STC   R2,BYTE2            STORE MON INDEX FOR THE HOOK                 
*                                                                               
*        GET CUMES FOR THE STATION FOR ALL DAYPARTS/DEMOS                       
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,STAHOOK2,0                                
         CLI   DB.DBERROR,0        ERROR?                                       
         B     *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HLPM0300                                                      
         ST    R4,DUB              SAVE OFF A(STATION)                          
         XIT1                                                                   
         EJECT                                                                  
**MON ROUTINE STATION HOOK                                                      
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING STATION CUME DATA FOR ALL DEMOS                    
*   SIGN ON TO SIGN OFF CUME (6A-2A)                                            
*   13 HARRIS DAYPARTS BASED ON TIME ZONE                                       
*                                                                               
***********************************************************************         
STAHOOK2 NTR1  BASE=*,LABEL=*                                                   
         CLI   ISCMT,0                                                          
         BNE   SHK20060                                                         
*                                                                               
         MVI   ISCMT,C'N'          SET TO EASTERN/PACIFIC                       
         L     RE,DB.DBAREC                                                     
         AH    RE,DB.DBDTADSP                                                   
SHK20020 CLI   0(RE),X'00'         FIND DIELEM                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'03'                                                      
         BE    SHK20040                                                         
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     SHK20020                                                         
*                                                                               
SHK20040 DS    0H                                                               
         USING DIELEM,RE                                                        
         CLI   DITZ,C'1'           EASTERN?                                     
         BE    SHK20060                                                         
         CLI   DITZ,C'4'           PACIFIC?                                     
         BE    SHK20060                                                         
         DROP  RE                                                               
*                                                                               
         MVI   ISCMT,C'Y'          NO                                           
*                                                                               
SHK20060 DS    0H                                                               
         LA    R1,EPTDPTS2                                                      
         CLI   ISCMT,C'Y'                                                       
         BNE   *+8                                                              
         LA    R1,CMTDPTS2                                                      
         L     RE,DB.DBAQUART                                                   
SHK20080 DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    SHK20160                                                         
         CLC   2(LENDTB2Q,RE),0(R1)                                             
         BE    SHK20100                                                         
         LA    R1,LENDTB2Q(R1)                                                  
         B     SHK20080                                                         
*                                                                               
SHK20100 DS    0H                                                               
         LA    R0,EPTDPTS2                                                      
         CLI   ISCMT,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,CMTDPTS2                                                      
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         LA    RF,LENDTB2Q                                                      
         DR    R0,RF                                                            
         LR    R3,R1               INDEX INTO DAYPARTS                          
         MHI   R3,4                                                             
*                                                                               
**TEST                                                                          
*        CLI   BYTE2,1                                                          
*        BNE   SHK20120                                                         
*        L     RF,DB.DBAQUART                                                   
*        LA    RF,2(RF)                                                         
*        GOTO1 VHEXOUT,DMCB,(RF),WORK,LENDTB2Q,0                                
*        GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
*        OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*        GOTO1 AADDDATA,DMCB,AFABLK,INVNUMEL,WORK,LENDTB2Q*2                    
*HK20120 DS    0H                                                               
**TEST                                                                          
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET MON INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
SHK20140 DS    0H                                                               
         LA    RE,CTDPTC(R3)       INDEX TO CORRECT DAYPART                     
         MVC   0(4,RE),0(R5)       COPY DEMO VALUE                              
*                                                                               
         CHI   R3,DUPLIQ2*4        CHECK IF ITS THE DUPLICATE                   
         BNE   *+10                                                             
         MVC   OFFSET2Q(4,RE),0(R5)                                             
*                                                                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,SHK20140                                                      
         DROP  R6                                                               
*                                                                               
SHK20160 DS    0H                                                               
         L     RE,DB.DBAQUART                                                   
         CLC   =X'17044C',2(RE)                                                 
         BNE   SHK20900                                                         
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET MON INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
SHK20180 DS    0H                                                               
         MVC   CTSTAC,0(R5)        COPY DEMO VALUE FOR STATION CUME             
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,SHK20180                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',DEMOS),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET MON INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
SHK20200 DS    0H                                                               
         MVC   CTSTAR,0(R5)        COPY DEMO VALUE FOR STATION CUME             
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,SHK20200                                                      
         DROP  R6                                                               
*                                                                               
SHK20900 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DAYPART TABLE FOR HARRIS MODEL : MONTH, NOT QUARTER, BASED                    
***********************************************************************         
EPTDPTS2 DS    0D                          EASTERN/PACIFIC TIME                 
         DC    X'15000C'                                                        
LENDTB2Q EQU   *-EPTDPTS2                                                       
         DC    X'150C18'                                                        
         DC    X'151828'                                                        
         DC    X'152830'                                                        
         DC    X'153038'                                                        
         DC    X'173845'                                                        
         DC    X'174446'                                                        
         DC    X'15464C'                                                        
         DC    X'FFFFFF'           N/A                                          
DUPLIQ2  EQU   (*-EPTDPTS2)/LENDTB2Q                                            
         DC    X'600418'                                                        
         DC    X'60182C'                                                        
OFFSET2Q EQU   (((*-EPTDPTS2)/LENDTB2Q)-DUPLIQ2)*4                              
         DC    X'600418'                                                        
         DC    X'701C30'                                                        
         DC    X'00'                                                            
CMTDPTS2 DS    0D                          CENTRAL/MOUNTAIN TIME                
         DC    X'15000C'                                                        
         DC    X'150C18'                                                        
         DC    X'151824'                                                        
         DC    X'15242C'                                                        
         DC    X'152C34'                                                        
         DC    X'173441'                                                        
         DC    X'174042'                                                        
         DC    X'154248'                                                        
         DC    X'FFFFFF'           N/A                                          
         DC    X'600418'                                                        
         DC    X'60182C'                                                        
         DC    X'600418'                                                        
         DC    X'701C30'                                                        
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
**MON ROUTINE STATION HOOK                                                      
**<<<>>>                                                                        
***********************************************************************         
* STATION ALIAS NORMALIZATION                                                   
*                                                                               
***********************************************************************         
SETALIAS NTR1  BASE=*,LABEL=*                                                   
         XC    CABLESTA(8),CABLESTA CLEAR ALIAS STORAGE                         
*                                                                               
         L     RE,FRSTSTA                                                       
         A     RE,OVPARMS+4                                                     
         LA    R0,13                                                            
*                                                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,2          TYPE                                         
         MVC   RSTAKREP,REPALPHA   REPCODE                                      
         MVC   RSTAKSTA,VHPSTA     STATION                                      
         DROP  R4                                                               
                                                                                
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'REPDIR',KEYSAVE,KEY,0               
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    SALI0020                                                         
         LA    R0,15                                                            
         DC    H'0'                                                             
SALI0020 EQU   *                                                                
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'GETREC'),=C'REPFILE',                  +        
               KEY+28,AIO4,DMWORK                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO4                                                          
         MVC   DATADISP,=H'34'           DISTANCE TO 1ST ELEMENT                
         MVI   MYELCODE,X'15'            ALIAS ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   SALI0900                  NO ALIAS ELEMENT                       
         USING RSTALIEL,R4                                                      
         BAS   RE,STARESET                                                      
         BAS   RE,ALFRESET                                                      
***      LA    R0,12                                                            
***      LA    RF,CABLESTA                                                      
***      DC    H'0'                                                             
SALI0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***STARESET                                                                     
*                                                                               
*   STRIP STATION FROM ALIAS                                                    
*   R4 -> STATION X'15' ELEMENT                                                 
*                                                                               
STARESET NTR1                                                                   
         LA    R4,2(R4)            A(START OF ALIAS)                            
         LA    R0,5                SET LOOP CONTROL MAX                         
*                                                                               
         MVC   CABLESTA,SPACES     CLEAR STATION TO SPACES                      
         LA    R2,CABLESTA                                                      
*                                                                               
SSET0020 EQU   *                                                                
         CLI   0(R4),C'/'          SEPARATOR FOUND?                             
         BE    SSET0040            YES - DON'T MOVE ANY MORE CHARACTERS         
         MVC   0(1,R2),0(R4)       NO  - MOVE CHAR TO DBLOCK                    
         LA    R2,1(R2)            BUMP TO NEXT OUTPUT POS                      
         LA    R4,1(R4)            BUMP TO NEXT INPUT  POS                      
         BCT   R0,SSET0020         MOVE 5 CHARS MAX                             
SSET0040 EQU   *                                                                
         CLI   CABLESTA+4,C' '     ANY LAST CHARACTER? (MEDIA)                  
         BH    SSET0060            YES                                          
         MVI   CABLESTA+4,C'T'     NO  - SET TO T                               
SSET0060 EQU   *                                                                
         XIT1                                                                   
                                                                                
***STARESET                                                                     
         EJECT                                                                  
***ALFRESET                                                                     
*                                                                               
*   STRIP MARKET(?) FROM ALIAS                                                  
*   R4 -> STATION X'15' ELEMENT                                                 
*                                                                               
ALFRESET NTR1                                                                   
         ZIC   R0,1(R4)            SET LOOP CONTROL MAX = ELT LEN               
         SH    R0,=H'2'            BACK OFF FOR CODE, LEN BYTES                 
*                                                                               
         LA    R4,2(R4)            A(START OF ALIAS)                            
*                                                                               
         MVC   CABLEMKT,SPACES     CLEAR ALPHA TO SPACES                        
         LA    R2,CABLEMKT                                                      
*                                                                               
ASET0020 EQU   *                                                                
         CLI   0(R4),C'/'          SEPARATOR FOUND?                             
         BE    ASET0040            YES - NOW MOVE TRAILING CHARACTERS           
         LA    R4,1(R4)            BUMP TO NEXT INPUT  POS                      
         BCT   R0,ASET0020         MOVE 5 CHARS MAX                             
         B     ASET0060            NO SEPARATOR FOUND                           
ASET0040 EQU   *                                                                
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         LR    R1,R0                                                            
         BCTR  R1,0                DECREASE LEN FOR SEPARATOR                   
         BCTR  R1,0                DECREASE LEN FOR EX                          
         EX    R1,ASET0050                                                      
         OC    0(3,R2),SPACES                                                   
         B     ASET0060                                                         
ASET0050 MVC   0(0,R2),0(R4)       INSERT MARKET(?)                             
ASET0060 EQU   *                                                                
         XIT1                                                                   
                                                                                
***ALFRESET                                                                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPETITIVE STATION ALIAS NORMALIZATION                                       
*                                                                               
***********************************************************************         
SETCALIA NTR1  BASE=*,LABEL=*                                                   
         LA    RF,COMPCABL                                                      
         XCEFL CCBLASTA,(RF)                                                    
*                                  CLEAR COMPETITIVE STATION ALIASES            
         L     R3,FRSTSTA                                                       
         A     R3,OVPARMS+4                                                     
         LA    R5,CCBLASTA         SET 1ST COMP ALIAS STATION                   
         ZIC   R0,NUMSTAS          SET NUMBER OF COMP STATIONS                  
         LTR   R0,R0               ANY STATIONS?                                
         BZ    SCAL0900            NO  - NOTHING TO SET UP                      
*                                                                               
SCAL0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,2          TYPE                                         
         MVC   RSTAKREP,REPALPHA   REPCODE                                      
         MVC   RSTAKSTA,0(R3)      STATION FROM COMPETITIVES                    
*                                                                               
         CLI   RSTAKSTA+4,C'1'     SATELLITE STATION?                           
         BE    SCAL0040            YES - TREAT AS 'NO ALIAS'                    
*                                                                               
         DROP  R4                                                               
                                                                                
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'REPDIR',KEYSAVE,KEY,0               
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BNE   SCAL0040            TREAT AS 'NO ALIAS'                          
*                                                                               
*   PROPOSER INPUT VALIDATION PERMITS THE ENTRY OF KXXX-1 IN THE                
*        COMPETITIVE FIELD.  THIS ISN'T APPRECIATED IN THIS                     
*        ROUTINE.  IF ENTERED IN THIS FASHION, IT WILL BE ASSUMED               
*        TO BE A NO-FIND.  THERE ARE OTHER APPROACHES THAT COULD BE             
*        TAKEN, BUT SO LONG AS PC CODE PERMITS STRANGE ENTRIES,                 
*        IT SHOULD NOT BE THE RESPONSIBILITY OF THIS MODULE TO FILTER           
*        EACH POSSIBILITY.   BILL UHR (FEB20/2007)                              
*                                                                               
SCAL0020 EQU   *                                                                
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'GETREC'),=C'REPFILE',                  +        
               KEY+28,AIO4,DMWORK                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO4                                                          
         MVC   DATADISP,=H'34'     DISTANCE TO 1ST ELEMENT                      
         MVI   MYELCODE,X'15'      ALIAS ELEMENT                                
         BRAS  RE,GETEL                                                         
         BNE   SCAL0040            NO ALIAS ELEMENT                             
         USING RSTALIEL,R4                                                      
         BAS   RE,CSTRESET                                                      
         BAS   RE,CALRESET                                                      
         LA    R5,LCCBLLEN(R5)     BUMP TO NEXT SLOT                            
SCAL0040 EQU   *                                                                
         LA    R3,5(R3)                                                         
         BCT   R0,SCAL0010         GO BACK FOR NEXT STATION                     
SCAL0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***CSTRESET                                                                     
*                                                                               
*   STRIP STATION FROM ALIAS                                                    
*   R4 -> STATION X'15' ELEMENT                                                 
*                                                                               
CSTRESET NTR1                                                                   
         LA    R4,2(R4)            A(START OF ALIAS IN ELEMENT)                 
         LA    R0,5                SET LOOP CONTROL MAX                         
*                                                                               
         MVC   0(LCCBLLEN,R5),SPACES     CLEAR STATION TO SPACES                
         MVC   0(5,R5),0(R3)       INSERT REQUESTED STATION                     
         LA    R2,5(R5)            SET A(ALIAS IN TABLE SLOT)                   
         LR    R6,R2               SET A(START OF ALIAS)                        
*                                                                               
CSET0020 EQU   *                                                                
         CLI   0(R4),C'/'          SEPARATOR FOUND?                             
         BE    CSET0040            YES - DON'T MOVE ANY MORE CHARACTERS         
         MVC   0(1,R2),0(R4)       NO  - MOVE CHAR TO DBLOCK                    
         LA    R2,1(R2)            BUMP TO NEXT OUTPUT POS                      
         LA    R4,1(R4)            BUMP TO NEXT INPUT  POS                      
         BCT   R0,CSET0020         MOVE 5 CHARS MAX                             
CSET0040 EQU   *                                                                
         CLI   4(R6),C' '          ANY LAST CHARACTER? (MEDIA)                  
         BH    CSET0060            YES                                          
         MVI   4(R6),C'T'          NO  - SET TO T                               
CSET0060 EQU   *                                                                
         XIT1                                                                   
                                                                                
***CSTRESET                                                                     
         EJECT                                                                  
***CALRESET                                                                     
*                                                                               
*   STRIP MARKET(?) FROM ALIAS                                                  
*   R4 -> STATION X'15' ELEMENT                                                 
*                                                                               
CALRESET NTR1                                                                   
         ZIC   R0,1(R4)            SET LOOP CONTROL MAX = ELT LEN               
         SH    R0,=H'2'            BACK OFF FOR CODE, LEN BYTES                 
*                                                                               
         LA    R4,2(R4)            A(START OF ALIAS)                            
*                                                                               
         LA    R2,10(R5)                                                        
*                                                                               
CASET020 EQU   *                                                                
         CLI   0(R4),C'/'          SEPARATOR FOUND?                             
         BE    CASET040            YES - NOW MOVE TRAILING CHARACTERS           
         LA    R4,1(R4)            BUMP TO NEXT INPUT  POS                      
         BCT   R0,CASET020         MOVE 5 CHARS MAX                             
         B     CASET060            NO SEPARATOR FOUND                           
CASET040 EQU   *                                                                
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         LR    R1,R0                                                            
         BCTR  R1,0                DECREASE LEN FOR SEPARATOR                   
         BCTR  R1,0                DECREASE LEN FOR EX                          
         EX    R1,CASET050                                                      
         OC    0(3,R2),SPACES                                                   
         B     CASET060                                                         
CASET050 MVC   0(0,R2),0(R4)       INSERT MARKET(?)                             
CASET060 EQU   *                                                                
         XIT1                                                                   
                                                                                
***CALRESET                                                                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETEL MACRO                                                                   
***********************************************************************         
         GETEL R4,DATADISP,MYELCODE  USED FOR THE GETEL OPERATIONS              
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
VDEMAND  DS    A                                                                
VDEMOUT  DS    A                                                                
*                                                                               
ACURPARM DS    F                                                                
*                                                                               
FRSTSTA  DS    F                                                                
FRSTSTA2 DS    F                                                                
CURSTA   DS    F                                                                
FRSTDEM  DS    F                                                                
CURDEM   DS    F                                                                
*                                                                               
TESTCTR2 DS    F                                                                
*                                                                               
DATADISP DS    H                                                                
MYELCODE DS    X                                                                
*                                                                               
OVDATE   DS    0XL2                                                             
OVYEAR   DS    X                                                                
OVQTR    DS    X                                                                
*                                                                               
NUMSTAS  DS    X                                                                
NUMDEMS  DS    X                                                                
NUMQTRS  DS    X                                                                
NUMMONS  DS    X                                                                
*                                                                               
REMSTAS  DS    X                                                                
REMDEMS  DS    X                                                                
*                                                                               
STYEAR   DS    X                                                                
ENYEAR   DS    X                                                                
STMONTH  DS    X                                                                
ENMONTH  DS    X                                                                
STQTR    DS    X                                                                
ENQTR    DS    X                                                                
LPMMKT   DC    C'Y'                DUMMY VALUE                                  
LPMPASS  DC    C'N'                NOT LPM MARKET                               
LPMSKIP  EQU   C'N'                LPM MKT: NO                                  
LPMQTRS  EQU   C'Q'                LPM MKT: PROCESS QUARTERS                    
LPMMONS  EQU   C'M'                LPM MKT: PROCESS MONTHS                      
*                                                                               
SKIP1E1F DS    CL1                 Y = SKIP GENERATING 1E+1F ITEMS              
*                                                                               
ISCMT    DS    CL1                 Y/N IS MKT CENTRAL/MOUNTAIN TIME?            
*                                                                               
BOOKS    DS    XL(2*MAXQTRS)       BOOKS FOR QUARTERS                           
BOOKSMON DS    XL(2*MAXQTRS)       BOOKS FOR MONTHS                             
DEMOS    DS    XL(3*(MAXDEMOS+1))                                               
CUMES    DS    XL(3*(MAXDEMOS+1))                                               
*                                                                               
MARKET   DS    XL(L'DBSELRMK)                                                   
LATESTBK DS    XL(L'DBSELBK)                                                    
*                                                                               
MKTBASIS DS    XL(MAXQTRS*4)       BASIS CUME FOR THE MARKET                    
*                                     (5 QTRS OR 13 MONTHS)                     
MYDBLOCK DS    CL(L'DBLOCK)                                                     
*                                                                               
         DS    CL20                DBLOCK IS BIGGER THAN ADVERTISED             
*                                                                               
VHPSTIND DS    CL1                 VHPSTA INDICATOR                             
*                                                                               
CABLESTA DS    CL5                                                              
CABLEMKT DS    CL3                                                              
*                                                                               
*   COMPETITIVE CABLE STATIONS                                                  
*        COMPETITIVE STATIONS REQUESTED MAY BE CABLE STATIONS THAT              
*        HAVE ASSOCIATED ALIASES THAT DEFINE THE STATION ITSELF.                
*        THIS TABLE PROVIDES UP TO FIVE (5) SLOTS FOR SUCH STATIONS.            
*        POS  1  -  5  =  STATION OF REQUEST.  WHEN AN ALIAS IS USED,           
*                         THE STATION OF REQUEST MAY NOT BE A REAL              
*                         STATION AT ALL.  IT COULD BE 'FRED', WITH             
*                         AND ALIAS OF ESPN / NY.                               
*        POS  6  - 10  =  STATION ALIAS.  THESE ARE THE CALL LETTERS            
*                         UNDER WHICH THE STATION IS IDENTIFIED IN THE          
*                         DEMO FILES                                            
*        POS  11 - 13  =  STATION MARKET. THIS IS THE MARKET # TO WHICH         
*                         THE STATION ALIAS + ITS EXTENSION RESOLVES.           
*                                                                               
MAXCABLE EQU   40                  NUMBER OF SLOTS IN LIST                      
DCABMED  EQU   5+4                 DISP(MEDIA IN ALIAS)                         
DCABALIA EQU   5                   DISP(ALIAS)                                  
*                                                                               
CCBLASTA DS    CL5                 REQUESTED STATION                            
CCBL1STA DS    CL5                 ALIAS                                        
CCBL1MKT DS    CL3                 ALIAS MARKET                                 
*                                                                               
LCCBLLEN EQU   *-CCBLASTA                                                       
*                                                                               
CCBLREST DS    CL(LCCBLLEN*40)     SPACE FOR 39 ADDL, PLUS 1 EXTRA              
*                                                                               
COMPCABL EQU   *-CCBLASTA          OVERALL CABLE SPACE FOR INIT                 
*&&DO                                                                           
CCBLBSTA DS    CL5                 REQUESTED STATION                            
CCBL2STA DS    CL5                                                              
CCBL2MKT DS    CL3                                                              
*                                                                               
CCBLCSTA DS    CL5                 REQUESTED STATION                            
CCBL3STA DS    CL5                                                              
CCBL3MKT DS    CL3                                                              
*                                                                               
CCBLDSTA DS    CL5                 REQUESTED STATION                            
CCBL4STA DS    CL5                                                              
CCBL4MKT DS    CL3                                                              
*                                                                               
CCBLESTA DS    CL5                 REQUESTED STATION                            
CCBL5STA DS    CL5                                                              
CCBL5MKT DS    CL3                                                              
*&&                                                                             
*                                                                               
FARADDR  DS    A                                                                
*                                                                               
*  AREA TO TABLE UP CUMES(R/F INFOS) FOR A STATION                              
*  5 QUARTERS                                                                   
*      24 DEMOS                                                                 
*          1  MARKET CUME                                                       
*          1  STATION CUME                                                      
*          1  STATION RATING                                                    
*          13 DAYPART CUMES                                                     
*                                                                               
MAXQTRS  EQU   13                                                               
MAXDEMOS EQU   24                                                               
*                                                                               
CUMETAB  DS    XL(MAXQTRS*MAXDEMOS*(3+13)*4)                                    
         ORG   CUMETAB                                                          
CTMKTC   DS    XL4                                                              
CTSTAC   DS    XL4                                                              
CTSTAR   DS    XL4                                                              
CTDPTC   DS    13XL4                                                            
CTDLENQ  EQU   *-CUMETAB           LENGTH OF DEMO ENTRY                         
CTSLENQ  EQU   *-CTSTAC            LENGTH OF STATION PORTION                    
CTQLENQ  EQU   MAXDEMOS*CTDLENQ    LENGTH OF QUARTER                            
         ORG                                                                    
*                                                                               
OVERWRKQ EQU   *-OVERWRKD          LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
       ++INCLUDE REPRPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGENCUMO                                                      
         EJECT                                                                  
MAPTABD  DSECT                                                                  
       ++INCLUDE REPRPMAP                                                       
*DEDEMFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060REPRP30   12/06/07'                                      
         END                                                                    
*                                                                               
