*          DATA SET RERES09    AT LEVEL 023 AS OF 11/14/07                      
*PHASE T81909A                                                                  
*INCLUDE CLPACK                                                                 
         TITLE 'T81909 - RERES09 - MARKET PROFILE REPORT'                       
*                                                                               
**********************************************************************          
*                                                                    *          
*        RERES09 (T81909) --- MARKET PROFILE REPORT                  *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* MAY02/90 (MRR) --- MAKE SUPPORTING RECORD I/O NON-LOCKING          *          
*                                                                    *          
* JUN18/91 (MRR) --- >SWITCH TO USING EXTENDED DBLOCK                *          
*                    >PASS SPOT DEMO LOOKER 1 DEC FLAG               *          
*                                                                    *          
* JUL08/91 (MRR) --- >PASS SPOT UPGRADE MODULE 1 DEC FLAG            *          
*                                                                    *          
* MAR22/93 (BU ) --- SOFT SE# RETRIEVAL FOR REP SYSTEM.  FIX.        *          
*                                                                    *          
* NOV07/94 (BU ) --- DMA CALCULATIONS ON IMPRESSIONS..               *          
*                                                                    *          
* MAR28/05 (BU ) --- ADD DEMFILV TO LIST                             *          
*                                                                    *          
* OCT25/07 (BU ) --- FIX PAGE0 ASSEMBLY ERROR                        *          
*                                                                    *          
* OCT26/07 (DE ) --- USE SOFT DEMOFILE OPEN LISTS                    *          
*                                                                    *          
*                                                                    *          
*                    ***  END TOMBSTONE  ***                         *          
**********************************************************************          
*                                                                    *          
*   REQUIRED INPUT TO REQUEST SCREEN GIVES YOU                       *          
* MARKET - USE IT TO GET STATION LIST IN ORDER                       *          
* SCHEME                                                             *          
* PERIOD                                                             *          
* YEAR (OR DEFAULT TO SCHEME)                                        *          
* SOURCE                                                             *          
* BOOK(S)                                                            *          
* DEMO(S)                                                            *          
* ------------------------------------------------------------------ *          
*   OPTIONAL INPUT TO REQUEST SCREEN GIVES YOU                       *          
* DAYPART - IF INPUT, VALIDATE AGAINST SCHEME RECORD                 *          
*         - IF BLANK, USE LIST IN SCHEME RECORD                      *          
* ------------------------------------------------------------------ *          
* IF THERE IS ONE, USE PJ2 FOR SCHEME/PERIOD FOR HISTORICAL BOOKS    *          
* ELSE, USE PJ1 FOR SCHEME/PERIOD                                    *          
* IF THERE IS AN UPGRADE OVERRIDE, USE IT INSTEAD OF THE UPGRADE FROM*          
* THE SCHEME/PERIOD                                                  *          
* ------------------------------------------------------------------ *          
*  READ REP MARKET RECORD                                            *          
*       GET DAYPART                                                  *          
*           GET STATION                                              *          
*               GOTO RANSID WITH STATION AND DAYPART                 *          
*                    GET DAY/TIME                                    *          
*                         GET BOOK                                   *          
*                              GET DEMOS - (SPGETDEMF)               *          
*                         NEXT BOOK                                  *          
*                    NEXT DAY/TIME                                   *          
*               DONE RANSID                                          *          
*           NEXT STATION                                             *          
*       NEXT DAYPART                                                 *          
*                                                                    *          
*********REGISTER USAGE******                                        *          
*                                                                    *          
*  R3=STATION LIST                                                   *          
*  R4=BOOK LIST                                                      *          
*  R5=RANSID BLOCK                                                   *          
*  R6=READING,GETEL                                                  *          
*  R7=2ND BASE REGISTER                                              *          
*  R8=SPOOLD                                                         *          
*  R9=SYSD                                                           *          
*  RA=TWA                                                            *          
*  RB= BASE REGISTER                                                 *          
*  RC= GEND                                                          *          
**********************************************************************          
*                                                                               
         EJECT                                                                  
T81909   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1909**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         MVI   CTLSWTCH,C'N'       TURN OFF SE# FLAG                            
*                                                                               
         LH    RF,=Y(BUFF-SYSD)                                                 
         LA    RF,SYSD(RF)                                                      
         ST    RF,SBUFF                                                         
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
*                                                                               
         B     MPXIT                                                            
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              PRINT REPORT                                    *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
PREP     DS    0H                                                               
         OC    ABOX,ABOX                                                        
         BZ    MP3                                                              
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         CLI   COP2,C'Y'           WIDE PRINTING                                
         BE    MP5                                                              
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
MP3      MVC   MPPWIDTH,=F'132'                                                 
         MVC   MPHWIDTH,=F'132'                                                 
         LA    RE,H1                                                            
         ST    RE,MPAH1                                                         
         LA    RE,H8                                                            
         ST    RE,MPAH8                                                         
         LA    RE,P                                                             
         ST    RE,MPAP1                                                         
         LA    RE,REGSPECS                                                      
         ST    RE,SPECS                                                         
         B     MP7                                                              
         SPACE 1                                                                
MP5      MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         MVC   MPPWIDTH,=F'165'                                                 
         MVC   MPHWIDTH,=F'198'                                                 
         L     R1,BOXAWIDE                                                      
         USING WIDED,R1                                                         
         LA    RE,XHEAD1                                                        
         ST    RE,MPAH1                                                         
         LA    RE,XHEAD8                                                        
         ST    RE,MPAH8                                                         
         LA    RE,XP                                                            
         ST    RE,MPAP1                                                         
         LA    RE,WIDSPECS                                                      
         ST    RE,SPECS                                                         
         SPACE 1                                                                
         DROP  R1                                                               
         SPACE 2                                                                
MP7      LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=V(CLPACK)                                                    
         A     R1,RELO                                                          
         ST    R1,ACLPACK                                                       
         SPACE 1                                                                
         MVI   RCSUBPRG,0                                                       
         CLI   COP1,C'Y'           SUPPRESS CERTAIN HEADLINES                   
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         SPACE 1                                                                
         BAS   RE,DEMOHDS          BUILD DEMO HEADLINES                         
         OC    CINDEX,CINDEX       IF INDEX BOOK                                
         BZ    MP10                                                             
         BAS   RE,BLDPCL           BUILD PERCENT CHANGE LINE                    
         L     RE,SBUFF            AND SAVE ADDRESS OF AREA FOR INDEX           
         LA    RE,2048(RE)                                                      
         LA    RE,2048(RE)                                                      
         LA    RE,904(RE)                                                       
         ST    RE,AINDEX1                                                       
         LA    RE,200(RE)                                                       
         ST    RE,AINDEX2                                                       
         SPACE 1                                                                
***  READ REP MARKET RECORD TO GET STATIONS IN CORRECT ORDER                    
         SPACE 1                                                                
MP10     XC    KEY,KEY                                                          
         MVI   FORCEHED,C'Y'                                                    
         LA    R6,KEY                                                           
         USING RMKTKEY,R6                                                       
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,CMARKET                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         CLC   KEY(27),KEYSAVE     MARKET RECORD                                
         BNE   MPXIT                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
*  GET STATIONS FROM MARKET RECORD AND CREATE LIST IN MKTSTA                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RMKTELEM,R6                                                      
         MVC   CMKTNAM,RMKTNAME                                                 
         SPACE 1                                                                
         LA    R3,MKTSTA                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    MP50                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
MP40     BAS   RE,NEXTEL                                                        
         BNE   MP60                                                             
         USING RMKTSTEL,R6                                                      
MP50     MVC   WORK(5),RMKTSTAT                                                 
         CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
         GOTO1 MSPACK,DMCB,CMARKET,WORK,0(R3)                                   
         LA    R3,5(R3)                                                         
         B     MP40                                                             
         SPACE 1                                                                
MP60     MVI   0(R3),X'FF'         END OF MARKET/STATION LIST MARKER            
         SPACE 1                                                                
         LA    R1,CDAYPART         DAYPART LIST                                 
         ST    R1,ADAYPART                                                      
         SPACE 1                                                                
         BAS   RE,SWISPOT          SWITCH TO SPOT SYSTEM                        
*                                                                               
MP70     LA    R3,MKTSTA           MARKET/STATION LIST                          
         MVI   NEWSTN,C'Y'                                                      
         SPACE 1                                                                
MP80     EQU   *                                                                
         L     R5,SBUFF                                                         
         XCEFL 0(R5),5000          CLEAR THE BUFFER                             
         L     R5,SBUFF            PUT LINES FOR A DAY/TIME TOGETHER            
         ST    R5,ABUFF                                                         
         L     R5,AIO2             SET UP AND GO TO RANSID                      
         USING SRBLKD,R5                                                        
         SPACE 1                                                                
         LA    RE,SRBLK            CLEAR BLOCK                                  
         LA    RF,SRBLKLN                                                       
         XCEF                                                                   
         SPACE 1                                                                
         MVC   SRASIR,AIO1         USE IO1 FOR PJ1 NSID RECORDS                 
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,ACLPACK                                                 
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SRAMASTC,TWAMASTC                                                
         SPACE 1                                                                
         MVC   SRSELSCH,CSCHEME                                                 
         MVC   SRSELAM,CAGYMED                                                  
         MVC   SRSELAGY,AGENCY                                                  
         MVI   SRSELMED,C'T'                                                    
         MVC   SRSELPER,CPERIOD                                                 
*        MVI   SRNSID,SRNREP                                                    
         MVC   SRSELMKT(5),0(R3)     MARKET AND STATION                         
         L     RF,ADAYPART                                                      
         MVC   SRSELDPT(1),0(RF)                                                
         MVC   SRSELYR,CYEAR                                                    
         DROP  R5                                                               
         SPACE 1                                                                
         OC    CSCHEME2,CSCHEME2                                                
         BZ    MP100                                                            
         SPACE 1                                                                
         L     R5,AIO3                                                          
         USING SRBLKD,R5                                                        
         SPACE 1                                                                
         LA    RE,SRBLK            CLEAR BLOCK                                  
         LA    RF,SRBLKLN                                                       
         XCEF                                                                   
         SPACE 1                                                                
         L     RE,AIO1                                                          
         ST    RE,SRASIR                                                        
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,ACLPACK                                                 
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SRAMASTC,TWAMASTC                                                
         SPACE 1                                                                
         MVC   SRSELSCH,CSCHEME2                                                
         MVC   SRSELAM,CAGYMED                                                  
         MVC   SRSELAGY,AGENCY                                                  
         MVI   SRSELMED,C'T'                                                    
         MVC   SRSELPER,CPERIOD2                                                
*        MVI   SRNSID,SRNREP                                                    
         MVC   SRSELMKT(5),0(R3)     MARKET AND STATION                         
         L     RF,ADAYPART                                                      
         MVC   SRSELDPT(1),0(RF)                                                
         MVC   SRSELYR,CYEAR2                                                   
         SPACE 1                                                                
MP90     DS    0H'0'                                                            
         OC    CSCHEME2,CSCHEME2      IF SCHEME 2                               
         BZ    MP100                                                            
         L     R5,AIO3             USE IT FOR HISTORICAL BOOKS                  
         LA    R2,MPRPJ2H                                                       
         B     MP110                                                            
MP100    L     R5,AIO2             ELSE USE SCHEME 1                            
         LA    R2,MPRPJ1H                                                       
         SPACE 1                                                                
MP110    L     RE,AINDEX2          RESET INDEX AREA                             
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         XC    0(200,RE),0(RE)                                                  
         SPACE 1                                                                
         BAS   RE,RSID             GO TO RANSID AND CHECK FOR ERRORS            
         CLI   SRMODE,SRONEREC     WAS A SID RECORD RETURNED                    
         BE    MP120               YES                                          
         OI    CSTATUS,X'40'       NO MORE SID RECORDS FOR HISTORICAL           
         B     MP160                                                            
         SPACE 1                                                                
MP120    OI    CSTATUS,X'01'       INDICATE SOMETHING TO PRINT                  
         L     RE,ABUFF            YES, FILL IN SOME PRINT LINE VALUES          
         USING PRTD,RE                                                          
         MVC   PDAY,SRERDAY                                                     
         MVC   PTIM,SRERTIM                                                     
         MVC   PSTAT,SRERSTA                                                    
         MVI   STAPRNT,C'Y'                                                     
         DROP  R5,RE                                                            
         LA    R4,BOOKS                                                         
MP130    MVC   THISBOOK,0(R4)                                                   
         SPACE 1                                                                
         BAS   RE,GETDEMS          NOW LOOK UP DEMOS                            
         SPACE 1                                                                
MP140    OC    CINDEX,CINDEX       IS THERE AN INDEX BOOK                       
         BZ    MP150                                                            
         CLC   0(4,R4),CINDEX      DOES IT MATCH BOOK WE JUST DID               
         BNE   MP150               NO                                           
         SPACE 1                                                                
* YES, SO BLOCK HAS DEMOS FOR THIS BOOK.  SAVE THEM AT AINDEX2                  
         SPACE 1                                                                
         L     RE,AINDEX2                                                       
         ZIC   R1,NUMDEMS                                                       
         MH    R1,=H'8'            DEMO IS 4 BYTES, + SVI DEMO                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),BLOCK                                                    
         SPACE 1                                                                
MP150    LA    R4,4(R4)            NEXT BOOK                                    
         CLI   0(R4),X'FF'                                                      
         BNE   MP130                                                            
         SPACE 1                                                                
         OC    CINDEX,CINDEX       IS THERE AN INDEX BOOK                       
         BZ    MP160                                                            
         SPACE 1                                                                
* BLOCK HAS DEMOS FOR LAST BOOK.  SAVE THEM AT AINDEX1                          
         SPACE 1                                                                
         L     RE,AINDEX1                                                       
         ZIC   R1,NUMDEMS          NUMBER OF DEMOS                              
         MH    R1,=H'8'            DEMO IS 4 BYTES, + SVI DEMO                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),BLOCK                                                    
         SPACE 1                                                                
         OC    200(200,RE),200(RE)                                              
         BNZ   MP160               ALREADY HAVE INDEX BOOK DEMOS                
         MVC   THISBOOK,CINDEX                                                  
         OI    CSTATUS,X'10'       DOING INDEX BOOK                             
         BAS   RE,GETDEMS                                                       
* NOW BLOCK HAS DEMOS FOR INDEX BOOK.  SAVE THEM AT AINDEX2                     
         SPACE 1                                                                
         L     RE,AINDEX2                                                       
         ZIC   R1,NUMDEMS                                                       
         MH    R1,=H'8'            DEMO IS 4 BYTES, + SVI DEMO                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),BLOCK                                                    
         NI    CSTATUS,X'EF'       RESET INDEX BOOK INDICATOR                   
         SPACE 1                                                                
* DO PERCENT CHANGE VS LAST YEAR NOW                                            
         SPACE 1                                                                
MP160    OC    CINDEX,CINDEX       INDEX BOOK IS OPTIONAL                       
         BZ    MP170                                                            
         TM    CSTATUS,X'40'       NO MORE HISTORICAL SID RECS                  
         BO    MP170               SO SKIP INDEX (MAY BE MORE XTRA PJS)         
         BAS   RE,INDEX            CALCULATE INDEX AND PRINT IT                 
         SPACE 1                                                                
* DO PROJECTIONS NOW                                                            
MP170    OC    CSCHEME2,CSCHEME2                                                
         BNZ   MP180                                                            
*  ONLY 1 SCHEME TO WORRY ABOUT                                                 
         TM    CSTATUS,X'40'       NO MORE HISTORICAL SID RECS                  
         BO    MP240               DONE                                         
         L     R5,AIO2                                                          
         OC    CUPTYP1,CUPTYP1     IS THERE AN OVERRIDE FOR PJ1                 
         BZ    *+8                 NO                                           
         OI    CSTATUS,X'08'       INDICATE UPGRADE OVERRIDE 1                  
         BAS   RE,GETPJ            ELSE, DO THE PROJECTION                      
         NI    CSTATUS,X'F7'       RESET STATUS                                 
         SPACE 1                                                                
         BAS   RE,SPLAT            PRINT THE ENTIRE CHUNK                       
         B     MP90       THEN GET NEXT SID DETAIL(DAY/TIME) RECORD             
         SPACE 1                                                                
*  2 SCHEMES TO WORRY ABOUT                                                     
         SPACE 1                                                                
MP180    TM    CSTATUS,X'20'       NO MORE SID RECS FOR XTRA SCHEME             
         BO    MP220                                                            
         LA    R2,MPRPJ1H                                                       
         L     R5,AIO2                                                          
         USING SRBLKD,R5                                                        
MP190    BAS   RE,RSID             GO TO RANSID AND CHECK FOR ERRORS            
         CLI   SRMODE,SRONEREC     WAS A SID RECORD RETURNED                    
         BE    MP200                                                            
         OI    CSTATUS,X'20'       NO MORE SID RECORDS FOR XTRA SCHEME          
         B     MP220                                                            
MP200    OI    CSTATUS,X'01'                                                    
         L     RE,ABUFF                                                         
         USING PRTD,RE                                                          
         CLI   STAPRNT,C'Y'        HAS STATION BEEN PRINTED                     
         BE    MP210                                                            
         MVC   PSTAT,SRERSTA       NO - PRINT IT                                
         MVI   STAPRNT,C'Y'                                                     
MP210    MVC   PDAY,SRERDAY                                                     
         MVC   PTIM,SRERTIM                                                     
         DROP  RE,R5                                                            
         L     R5,AIO2                                                          
         OC    CUPTYP1,CUPTYP1     IS THERE AN OVERRIDE FOR PJ1                 
         BZ    *+8                 NO                                           
         OI    CSTATUS,X'08'       INDICATE  UPGRADE OVERRIDE 1                 
         BAS   RE,GETPJ            DO PJ ON EXTRA SCHEME                        
         NI    CSTATUS,X'F7'       RESET STATUS                                 
         TM    CSTATUS,X'40'       NO MORE HISTORICAL SID RECORDS               
         BNO   MP230                                                            
         BAS   RE,SPLAT            PRINT THE DAY/TIME CHUNK                     
         MVI   STAPRNT,C'N'        NEED TO PRINT STATION FOR EACH CHUNK         
         B     MP190               AND GET REST OF XTRA SCHEME RECORDS          
         SPACE 1                                                                
MP220    TM    CSTATUS,X'40'       NO MORE HISTORICAL SID RECORDS               
         BO    MP240               DONE                                         
MP230    L     R5,AIO3                                                          
         USING SRBLKD,R5                                                        
         L     RE,ABUFF                                                         
         USING PRTD,RE                                                          
         CLI   STAPRNT,C'Y'        DID WE PRINT STATION FOR THIS                
         BE    *+14                DAY/TIME CHUNK YET?                          
         MVC   PSTAT,SRERSTA                                                    
         MVI   STAPRNT,C'Y'                                                     
         MVC   PDAY,SRERDAY                                                     
         MVC   PTIM,SRERTIM                                                     
         DROP  RE,R5                                                            
         OC    CUPTYP2,CUPTYP2     IS THERE AN OVERRIDE                         
         BZ    *+8                 NO                                           
         OI    CSTATUS,X'04'       INDICATE UPGRADE OVERRIDE 2                  
         BAS   RE,GETPJ            DO PJ ON HISTORICAL SCHEME                   
         NI    CSTATUS,X'FB'       RESET STATUS                                 
         BAS   RE,SPLAT            PRINT THE DAY/TIME CHUNK                     
         B     MP90                                                             
         SPACE 2                                                                
MP240    NI    CSTATUS,X'9F'       RESET NO MORE SID RECORDS INDICATOR          
         MVI   STAPRNT,C'N'                                                     
         LA    R3,5(R3)            NEXT STATION                                 
         CLI   0(R3),X'FF'                                                      
         BE    MP250                                                            
         TM    CSTATUS,X'01'       IF SOMETHING PRINTED, THEN PUT               
         BZ    MP80                                                             
         MVI   NEWSTN,C'Y'                                                      
         B     MP80                                                             
         SPACE 1                                                                
MP250    L     RF,ADAYPART         NEXT DAYPART                                 
         LA    RF,1(RF)                                                         
         ST    RF,ADAYPART                                                      
         CLI   0(RF),0             END OF DAYPART LIST                          
         BE    MP260                                                            
         TM    CSTATUS,X'01'       ANY LINES PRINTED                            
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'       NEW PAGE PER DAYPART                         
         NI    CSTATUS,X'FE'       RESET PRINT INDICATOR                        
         B     MP70                                                             
         SPACE 1                                                                
MP260    BAS   RE,SWIREP           SWITCH TO REP                                
         SPACE 1                                                                
         B     MPXIT               DONE                                         
MPXIT    XIT1                                                                   
         EJECT                                                                  
RSID     DS    0H'0'                                                            
         ST    RE,FULL                                                          
         PRINT GEN                                                              
         GOTO1 RANSID,DMCB,(R5)                                                 
         PRINT NOGEN                                                            
         SPACE 1                                                                
         USING SRBLKD,R5                                                        
         CLI   SRERROR,SRNOERR     TEST FOR ERROR                               
         BE    RS40                                                             
         CLI   SRERROR,SRNOSTA     STATION NOT FOUND                            
         BNE   RS10                                                             
         MVC   CONHEAD+10(L'NOSTA),NOSTA                                        
         B     MYEND                                                            
         SPACE 1                                                                
RS10     CLI   SRERROR,SRNOMKT     MARKET NOT FOUND                             
         BNE   RS20                                                             
         MVC   CONHEAD+10(L'NOMKT),NOMKT                                        
         B     MYEND                                                            
         SPACE 1                                                                
RS20     CLI   SRERROR,SRNOSCH     SCHEME NOT FOUND                             
         BNE   RS30                                                             
         MVC   CONHEAD+10(L'NOSCH),NOSCH                                        
         B     MYEND                                                            
         SPACE 1                                                                
RS30     CLI   SRERROR,SRNOPER     PERIOD NOT FOUND                             
         BE    *+6                                                              
         DC    H'0'                SOMETHING ELSE IS WRONG                      
         MVC   CONHEAD+10(L'NOPERD),NOPERD                                      
         B     MYEND                                                            
RS40     L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
CHKDOT   NTR1                                                                   
         L     R2,ABUFF                                                         
         USING PRTD,R2                                                          
         CLI   PTIM+5,X'0B'                                                     
         BNE   CDOT0010                                                         
         DC    H'0'                                                             
CDOT0010 EQU   *                                                                
         XIT                                                                    
         DROP  R2                                                               
DUMPOUT  DS    CL10                                                             
         EJECT                                                                  
GETDEMS  NTR1                                                                   
         USING SRBLKD,R5                                                        
         SPACE 1                                                                
         XC    SPDEMLK,SPDEMLK                                                  
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         MVC   SPLKAREC,AIO1                                                    
         MVC   SPLKAFAC,ACOMFACS                                                
         LA    R1,DEMOS                                                         
         ST    R1,SPLKALST         DEMO LIST                                    
         LA    R1,BLOCK                                                         
         ST    R1,SPLKAVAL         DEMO VALUES OUTPUT AREA                      
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSRC,SVSOURCE                                                 
         MVC   SPLKAGY,AGENCY                                                   
         MVI   SPLKMED,C'T'                                                     
         MVI   SPLKSVI,X'FF'                                                    
         MVC   SPLKSTA,SRERSTA                                                  
         MVC   SPLKDAY,SRACTDAY                                                 
         MVC   SPLKTIM,SRACTTIM                                                 
         MVC   SPLKDBK,THISBOOK+1  DEMO BOOK MONTH AND YEAR                     
         MVC   SPLKBTYP,THISBOOK+3 SPECIAL BOOK TYPE                            
         OI    SPLKOPT,SPLKOPR1    SET 1 DEC FLAG                               
         DROP  R5                                                               
         CLI   COP3,C'I'           DEMOS ON IMPRESSIONS?                        
         BNE   GDEM0020            NO                                           
         XC    DMAICALC,DMAICALC   CLEAR DMA IMP CALC WORKAREA                  
         MVI   DMAICALC+5,C'I'     INSERT 'IMPRESSIONS' IND                     
         LA    R5,DMAICALC         SET A(DMA IMP CALC WORKAREA)                 
         ST    R5,SPLKA1W                                                       
GDEM0020 EQU   *                                                                
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLK)    CALL SPGETDEM                   
         SPACE 2                                                                
         TM    CSTATUS,X'10'       DOING INDEX BOOK                             
         BO    MPXIT               YES, SO SKIP PRINTING STUFF                  
         L     R4,ABUFF                                                         
         USING PRTD,R4                                                          
         MVC   PPRG,SPLKPRG        PROGRAM NAME                                 
         SPACE 1                                                                
         LA    R3,THISBOOK                                                      
         ZIC   R1,2(R3)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   PBOOK(3),0(R1)                                                   
         EDIT  (1,1(R3)),(2,PBOOK+3)                                            
         CLI   3(R3),0             SPECIAL BOOK TYPE                            
         BE    GDEM0040                                                         
         MVC   PBOOK+5(3),=C'( )'                                               
         MVC   PBOOK+6(1),3(R3)                                                 
         SPACE 1                                                                
GDEM0040 EQU   *                                                                
         BAS   RE,PRNTDEMS                                                      
         B     MPXIT                                                            
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
PRNTDEMS NTR1                                                                   
         USING PRTD,R4                                                          
         LA    R3,PDEM             PRINT LINE                                   
         LA    R2,BLOCK            DEMO VALUES                                  
         LA    R6,DEMOS            DEMOS                                        
         ZIC   R1,NUMDEMS          NUMBER OF DEMOS                              
         SPACE 1                                                                
PD10     EQU   *                                                                
         L     RF,0(R2)                                                         
         C     RF,=F'9999'         IF MORE THAN 4 DIGITS AND A DECIMAL          
         BNH   PD20                                                             
         SR    RE,RE               THEN ROUND,                                  
         LA    RF,5(RF)                                                         
         D     RE,=F'10'                                                        
         EDIT  (RF),(5,0(R3))      AND PRINT WITH NO DECIMAL                    
         B     PD30                (BUT LEAVE VALUE AS IS FOR INDEX)            
         SPACE 1                                                                
PD20     EDIT  (RF),(5,0(R3)),1     1 DECIMAL POINT EVERYTHING                  
         SPACE 1                                                                
PD30     LA    R6,3(R6)            NEXT DEMO                                    
         LA    R3,6(R3)            NEXT PRINT AREA                              
         LA    R2,4(R2)            NEXT DEMO VALUE                              
         CLC   PBOOK+3(2),=C'PJ'                                                
         BE    *+8                                                              
         LA    R2,4(R2)            FOR HISTORICAL BOOKS, SKIP SVI VALUE         
         BCT   R1,PD10                                                          
         SPACE 1                                                                
         A     R4,MPPWIDTH      POINT TO NEXT LINE                              
         ST    R4,ABUFF                                                         
         PRINT GEN                                                              
         XCEFL 0(R4),MPPWIDTH      CLEAR THE NEXT LINE                          
         PRINT NOGEN                                                            
         B     MPXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE DEALS WITH THE PROJECTION LINES                                  
         SPACE 1                                                                
GETPJ    NTR1                                                                   
         USING SRBLKD,R5                                                        
         TM    CSTATUS,X'0C'       IS THERE UPGRADE OVERRIDE (1 OR 2)           
         BNZ   GP10                                                             
         CLI   SRUPFILE,0          IS THERE AN UPGRADE                          
         BE    GETPJX                                                           
GP10     EQU   *                                                                
         LA    R4,BLOCK1                                                        
         USING SPDEMUPD,R4         R4=A(UPGRADE BLOCK)                          
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         MVC   SPUPAREC,AIO1                                                    
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGENCY                                                   
         MVI   SPUPMED,C'T'                                                     
         MVC   SPUPSTA,SRERSTA                                                  
         MVC   SPUPDAY,SRACTDAY                                                 
         MVC   SPUPTIM,SRACTTIM                                                 
         MVC   SPUPSRC,SVSOURCE                                                 
         XC    SPUPFBK,SPUPFBK     USE LATEST AS SHARE BOOK UNLESS              
*                                  OVERRIDDEN BY A BK=MMM/YY IN UPGRADE         
         MVI   SPUPFIL,C'T'                                                     
         MVI   SPUPOPTS,SPOP1DEC   SET 1 DEC FLAG                               
         SPACE 1                                                                
         TM    CSTATUS,X'08'       IS THERE AN UPGRADE FROM PJ1                 
         BZ    GP20                                                             
         LA    R3,CUPTYP1                                                       
         B     GP30                                                             
GP20     TM    CSTATUS,X'04'       OR FROM PJ2                                  
         BZ    GP40                                                             
         LA    R3,CUPTYP2                                                       
         SPACE 1                                                                
         USING UPGD,R3                                                          
*  YES, SO USE SOME VALUES FROM UPGRADE DETAILS INPUT ON SCREEN                 
GP30     OC    CUPFBK,CUPFBK                                                    
         BZ    *+10                                                             
         MVC   SPUPFBK,CUPFBK                                                   
         OC    CUPSTA,CUPSTA                                                    
         BZ    *+10                                                             
         MVC   SPUPSTA,CUPSTA                                                   
         MVC   SPUPUDAY,CUPUDAY                                                 
         MVC   SPUPUTIM,CUPUTIM                                                 
         MVC   SPUPTYPE(8),CUPTYPE                                              
         MVC   SPUPBTYP,CUPBTYPE                                                
         B     GP50                                                             
         SPACE 1                                                                
*  ELSE, USE SOME VALUES FROM UPGRADE DETAILS IN SID RECORD                     
GP40     OC    SRUPBOOK,SRUPBOOK                                                
         BZ    *+10                                                             
         MVC   SPUPFBK,SRUPBOOK                                                 
         MVC   SPUPUDAY,SRUPDAY                                                 
         MVC   SPUPUTIM,SRUPTIM                                                 
         MVC   SPUPTYPE(8),SRUPEXP                                              
         MVC   SPUPAOVR,SROVELEM   A(DEMO OVERRIDE LIST)                        
         OC    SRUPSTA,SRUPSTA                                                  
         BZ    *+10                                                             
         MVC   SPUPSTA,SRUPSTA                                                  
         SPACE 1                                                                
GP50     EQU   *                                                                
         CLI   COP3,C'I'           DMA CALCS BASED ON IMPS?                     
         BNE   GP55                NO                                           
         OI    SPUPOPTS,X'08'      YES - TURN ON FLAG                           
         OI    SPUPOPTS,SPOPNORM   YES - TURN ON FLAG                           
GP55     EQU   *                                                                
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 SPDEMUP,DMCB,SPDEMUPD,DEMOS,BLOCK                                
         MVC   SVUPPRG,SPUPPRG     SAVE PROGRAM NAME FROM SHARE BOOK            
         SPACE 1                                                                
         DROP  R4                                                               
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   GETPJX                                                           
         TM    CSTATUS,X'0C'       OVERRIDE UPGRADE                             
         BNZ   *+8                 YES, SKIP GETOVRD                            
         BAS   RE,GETOVRD                                                       
         L     R4,ABUFF                                                         
         USING PRTD,R4                                                          
         MVC   PPRG,SRACTPRO       USE PROGRAM NAME FROM SID RECORD             
         OC    SRACTPRO,SRACTPRO   IF NONE, THEN USE                            
         BNZ   *+10                                                             
         MVC   PPRG,SVUPPRG        PROGRAM NAME FROM SHARE BOOK                 
         SPACE 1                                                                
         ZIC   RE,SRUPEXP+3        FOR BOOK, USE PUT BOOK MONTH                 
         TM    CSTATUS,X'0C'       BUT IF OVERRIDE UPGRADE                      
         BZ    GP60                GET PUT BOOK MONTH FROM CUPFLD1              
         ZIC   RE,CUPFLD1+1                                                     
         DROP  R3                                                               
         SPACE 1                                                                
GP60     BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         LA    RE,MONTHS(RE)                                                    
         MVC   PBOOK(3),0(RE)                                                   
         MVC   PBOOK+3(2),=C'PJ'     PLUS PJ                                    
         BAS   RE,PRNTDEMS                                                      
GETPJX   B     MPXIT                                                            
         DROP  R4,R5                                                            
         SPACE 3                                                                
         EJECT                                                                  
GETOVRD  NTR1                                                                   
         USING SRBLKD,R5                                                        
         OC    SROVELEM,SROVELEM                                                
         BZ    GETOVRDX                                                         
         LA    R2,CDMOVFLG         OVERRIDE FLAG                                
         LA    R1,DEMOS                                                         
         ZIC   R0,NUMDEMS                                                       
         SPACE 1                                                                
         BAS   RE,GETVAL                                                        
         LA    R2,1(R2)                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,*-12                                                          
GETOVRDX B     MPXIT                                                            
         DROP  R5                                                               
         SPACE 4                                                                
GETVAL   NTR1                                                                   
         USING SRBLKD,R5                                                        
         L     R6,AIO1                                                          
         LA    R6,24(R6)                                                        
         L     R6,SROVELEM                                                      
GV10     CLI   0(R6),0             TEST END OF OVRD ELEMS                       
         BE    GETVALX                                                          
         CLI   0(R6),X'DE'         TEST OVRD ELEM                               
         BE    GV30                                                             
GV20     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GV10                                                             
         SPACE 1                                                                
GV30     CLC   2(2,R6),1(R1)       MATCH DEMO MOD/NUM                           
         BNE   GV20                                                             
         MVI   0(R2),1             SET FLAG                                     
*NOW FIND FIELD IN BLOCK AND SET OVRD VALUE                                     
         SPACE 1                                                                
         LA    R4,DEMOS                                                         
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
GV40     CLI   0(R4),0             MAKE SURE NOT CPP, ETC.                      
         BNE   GV50                                                             
         CLC   1(2,R4),2(R6)       MATCH MOD/NUM                                
         BNE   GV50                                                             
         XC    0(4,R3),0(R3)       CLEAR OLD VALUE                              
         MVC   2(2,R3),4(R6)       SET NEW VALUE                                
         B     GETVALX                                                          
         SPACE 1                                                                
GV50     LA    R4,3(R4)                                                         
         LA    R3,4(R3)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   GV40                                                             
GETVALX  B     MPXIT                                                            
         DROP  R5                                                               
         EJECT                                                                  
* THIS ROUTINE CALCULATES THE INDEX AND PRINTS THEM                             
*    AINDEX1 (BUFF+5000) HAS DEMOS TO BE INDEXED                                
*    AINDEX2 (BUFF+5200) HAS DEMOS OF INDEX                                     
*    BLOCK WILL HOLD THE NEW INDEXED VALUES                                     
         SPACE 1                                                                
INDEX    NTR1                                                                   
         L     R4,ABUFF            PRINT A SPACING LINE                         
         A     R4,MPPWIDTH                                                      
         ST    R4,ABUFF                                                         
         L     R6,AINDEX1                                                       
         L     R3,AINDEX2                                                       
         LA    R2,BLOCK                                                         
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         ZIC   R4,NUMDEMS                                                       
         SPACE 1                                                                
IND10    SR    R0,R0                                                            
         L     R1,0(R6)                                                         
         XC    0(8,R6),0(R6)                                                    
         C     R1,0(R3)            IF VALUES ARE THE SAME                       
         BE    IND20               PRINT NOTHING                                
         C     R1,=F'0'            IF INDEX1 IS ZERO                            
         BNE   IND15               PRINT A -                                    
         MVI   0(R2),X'FD'                                                      
         B     IND20                                                            
IND15    M     R0,=F'200'                                                       
         SPACE 1                                                                
         OC    0(4,R3),0(R3)       IF INDEX 2 IS ZERO                           
         BNZ   IND17               PRINT A +                                    
         MVI   0(R2),X'FE'                                                      
         B     IND20                                                            
         SPACE 1                                                                
IND17    D     R0,0(R3)                                                         
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
         SH    R1,=H'100'          WANT PERCENT + OR -                          
         ST    R1,0(R2)                                                         
         SPACE 1                                                                
IND20    LA    R6,8(R6)                                                         
         LA    R3,8(R3)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,IND10                                                         
         SPACE 1                                                                
         L     R4,ABUFF                                                         
         USING PRTD,R4                                                          
         MVC   PDAY+4(29),PLIN     PERCENT CHANGE COMMENT                       
         SPACE 1                                                                
         LA    R3,PDEM+1           PRINT LINE                                   
         LA    R2,BLOCK            INDEX VALUES                                 
         ZIC   R5,NUMDEMS          NUMBER OF DEMOS                              
IND30    CLI   0(R2),X'FD'                                                      
         BNE   IND33                                                            
         MVC   2(2,R3),=C'--'                                                   
         B     IND40                                                            
IND33    CLI   0(R2),X'FE'                                                      
         BNE   IND35                                                            
         MVC   2(2,R3),=C'++'                                                   
         B     IND40                                                            
IND35    EDIT  (4,0(R2)),(4,0(R3)),TRAIL=C'%',FLOAT=-                           
IND40    LA    R3,6(R3)            NEXT PRINT POSITION                          
         LA    R2,4(R2)            NEXT INDEX VALUE                             
         BCT   R5,IND30                                                         
         SPACE 1                                                                
         A     R4,MPPWIDTH         DONE WITH THE LINE                           
         A     R4,MPPWIDTH         AND DO A SPACING LINE                        
         ST    R4,ABUFF                                                         
         SPACE 1                                                                
         B     MPXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
*** ROUTINE TO SWITCH TO SPOT SYSTEM THROUGH FASWITCH                           
*** AND SET UP INTERNAL VALUES FOR SPOT                                         
         SPACE 1                                                                
SWISPOT  NTR1                                                                   
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   SS10                NO  - ON-LINE                                
         CLI   CTLSWTCH,C'Y'       SE # ALREADY GOTTEN?                         
         BE    SS05                YES                                          
         MVI   CTLSWTCH,C'Y'       NO  - GO GET IT                              
         BAS   RE,CTRLSET                                                       
SS05     EQU   *                                                                
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         MVC   4(1,R5),CSPOTSE#    INSERT SPOT SE#                              
*                                                                               
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         L     RE,TSPFUSER                                                      
         CLC   =C'SPOT',0(RE)                                                   
         BE    SS40                                                             
         MVC   0(4,RE),=C'SPOT'                                                 
         DROP  RE                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',SPTFLIST,AIO2                   
         B     SS40                                                             
         SPACE                                                                  
SS10     L     RF,SWITCH                                                        
         XC    DMCB(8),DMCB                                                     
         CLC   AGENCY,=C'SJ'       TEST REP?                                    
         BE    SS20                YES                                          
*                                  NO  - CALL SPOT SYSTEM                       
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         B     SS30                                                             
*                                                                               
*   TEST                                                                        
         MVC   CONHEAD+10(12),=C'S/B IN SPOTS'                                  
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         LA    R2,CONHEAD+24                                                    
         PRINT GEN                                                              
         GOTO1 HEXOUT,DMCB,(R5),(R2),8,=C'TOG'                                  
         PRINT NOGEN                                                            
         B     MYEND                                                            
*   TEST END                                                                    
*                                                                               
         B     SS30                                                             
SS20     EQU   *                                                                
         MVI   DMCB,X'02'          USE SPOT 1 (SYSTEM NUMBER IS 2)              
         GOTO1 (RF),DMCB                                                        
         SPACE 1                                                                
SS30     EQU   *                                                                
         CLI   4(R1),2             SYSTEM OPERATIONAL?                          
         BNE   *+14                YES                                          
         MVC   CONHEAD+10(27),=C'SPOT SYSTEM NOT OPERATIONAL'                   
         B     MYEND                                                            
         SPACE 1                                                                
         CLI   4(R1),1             NOT AUTHORIZED FOR SYSTEM ACCESS?            
         BNE   *+14                YES                                          
         MVC   CONHEAD+10(27),=C'NOT AUTHORIZED FOR SPOT SYS'                   
         B     MYEND                                                            
         SPACE 1                                                                
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*  SET UP INTERNAL VALUES FOR SPOT                                              
SS40     MVI   SYSTEM,C'S'                                                      
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   REQFILE,=C'SPTREQ '                                              
         B     MPXIT                                                            
         EJECT                                                                  
*** ROUTINE TO SWITCH BACK TO REP AND RESET INTERNAL VALUES                     
         SPACE 1                                                                
SWIREP   NTR1                                                                   
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   SR20                                                             
*******  GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
         CLI   CTLSWTCH,C'Y'       SE # ALREADY GOTTEN?                         
         BE    SR05                YES                                          
         MVI   CTLSWTCH,C'Y'       NO  - GO GET IT                              
         BAS   RE,CTRLSET                                                       
SR05     EQU   *                                                                
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         MVC   4(1,R5),CREPSE#     INSERT REP SE#                               
*                                                                               
*   OPEN REP FILES *AND* CONTROL FILE UNDER REP SE #                            
*                                                                               
SR10     EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO2                    
         B     SR40                                                             
*                                                                               
SR20     L     RF,SWITCH                                                        
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SR40     MVI   SYSTEM,C'R'                                                      
         MVC   LKEY,=H'27'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         B     MPXIT                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*   CTLSET:  GETS SE # FROM CONTROL FILE FOR THE RUN.  ONLY DONE ONE            
*     TIME, BASED ON VALUE OF 'CTLSWTCH'.                                       
*             R5  =  A(UTL)                                                     
*                                                                               
CTRLSET  NTR1                                                                   
         MVI   4(R5),X'0A'         SET UTL SE TO CTFILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AIO2,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'5'           FIND CONTROL FILE ACCESS RECORD              
         MVC   WORK+23,AGENCY      INSERT POWER CODE                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO2                     
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIO2                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTH ELT FOR 'REP'                  
         ST    R1,FULL             SAVE A(1ST ELT IN RECORD)                    
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         MVC   CREPSE#,3(R1)       SAVE REP UTL CODE                            
*                                  FIND SYS AUTH ELT FOR 'SPOT'                 
         L     R1,FULL             RESET A(1ST ELT IN RECORD)                   
CTRL0040 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0050            NO                                           
         CLI   2(R1),X'02'         IS IT 'SPOT' SYSTEM?                         
         BE    CTRL0060            YES                                          
CTRL0050 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0040            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0060 EQU   *                                                                
         MVC   CSPOTSE#,3(R1)      SAVE SPOT UTL CODE                           
         B     MPXIT                                                            
         EJECT                                                                  
SPLAT    NTR1                                                                   
         SR    R2,R2                                                            
         L     R5,SBUFF                                                         
         L     R3,ABUFF                                                         
         CR    R3,R5                                                            
         BE    SPLATX              NOTHING TO PRINT                             
         SR    R3,R5                                                            
         CLI   COP2,C'Y'                                                        
         BE    *+12                                                             
         D     R2,=F'132'                                                       
         B     *+8                                                              
         D     R2,=F'165'                                                       
         CH    R3,=H'29'           MAXIMUM OF 29 LINES IN A BUFFER              
         BL    *+6                 ELSE WILL CREAM INDEX STUFF                  
         DC    H'0'                                                             
         STC   R3,ALLOWLIN         MAKE SURE CHUNK WILL FIT ON 1 PAGE           
         SPACE 1                                                                
         OC    ABOX,ABOX                                                        
         BZ    SPLAT8                                                           
         CLI   FORCEHED,C'Y'         AM I AT TOP OF PAGE                        
         BE    SPLAT4                                                           
         ZIC   R2,MAXLINES                                                      
         ZIC   RE,LINE                                                          
         LA    RE,0(R3,RE)                                                      
         CR    RE,R2               WILL THIS CHUNK FIT ON PAGE                  
         BL    SPLAT3                                                           
         MVI   FORCEHED,C'Y'       NO, FORCE NEW PAGE AND                       
         B     SPLAT4              BOX BOTTOM IS DONE AUTOMATICALLY             
SPLAT3   CLI   NEWSTN,C'Y'         NEW STATION GETS                             
         BNE   SPLAT5                                                           
         BAS   RE,BOXBTTP          BOX BOTTOM FOR OLD AND TOP FOR NEW           
SPLAT4   MVI   NEWSTN,C'N'                                                      
         B     SPLAT8                                                           
SPLAT5   BAS   RE,BOXMID           SAME STATION JUST GETS MIDLINE               
SPLAT8   L     R5,SBUFF                                                         
         LA    R2,131              WIDTH OF PRINT LINE-1 FOR EXECUTE            
         CLI   COP2,C'Y'                                                        
         BNE   *+8                                                              
         LA    R2,164                                                           
         SPACE 1                                                                
SPLAT10  EX    R2,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),SPACEW                                                   
         L     R4,MPAP1                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         XC    0(0,R5),0(R5)                                                    
         A     R5,MPPWIDTH         NEXT LINE                                    
         BCT   R3,SPLAT10                                                       
         SPACE 1                                                                
         L     R5,SBUFF                                                         
         ST    R5,ABUFF                                                         
SPLATX   B     MPXIT                                                            
         EJECT                                                                  
BOXBTTP  NTR1                                                                   
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     MPXIT                                                            
         SPACE 3                                                                
BOXMID   NTR1                                                                   
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     MPXIT                                                            
         SPACE 3                                                                
         DROP  R5                                                               
         EJECT                                                                  
HOOK     NTR1                                                                   
         L     R1,MPAH1            POINT TO HEADLINE1                           
         SPACE 1                                                                
         MVC   11(4,R1),CMARKET                                                 
         MVC   21(20,R1),CMKTNAM                                                
         L     RF,ADAYPART                                                      
         LA    R2,DPTBL                                                         
HK10     CLI   0(R2),X'FF'                                                      
         BE    HK20                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    HK20                                                             
         LA    R2,L'DPTBL(R2)                                                   
         B     HK10                                                             
HK20     A     R1,MPHWIDTH         POINT TO HEADLINE 2                          
         MVC   11(1,R1),0(RF)                                                   
         MVC   21(20,R1),1(R2)                                                  
         A     R1,MPHWIDTH         POINT TO HEADLINE 3                          
         MVC   11(3,R1),=C'ARB'                                                 
         CLI   SVSOURCE,C'A'                                                    
         BE    HK25                                                             
         CLI   SVSOURCE,C'N'                                                    
         BNE   *+14                                                             
         MVC   11(3,R1),=C'NSI'                                                 
         B     HK25                                                             
         MVC   11(3,R1),=C'SRC'                                                 
         SPACE 1                                                                
HK25     CLI   COP1,C'Y'           SUPPRESS CERTAIN HEADLINES                   
         BE    HK40                                                             
         A     R1,MPHWIDTH         POINT TO HEADLINE 4                          
         CLI   COP2,C'Y'           WIDE PRINTING                                
         BE    HK33                                                             
         LA    R2,MPRPJ1H          MOVE IN PJ1                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   92(0,R1),8(R2)                                                   
         LA    R2,MPROV1H          AND OVERRIDE 1, IF THERE IS ONE              
         CLI   5(R2),0                                                          
         BE    HK30                                                             
         MVC   104(3,R1),=C'OV1'                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   108(0,R1),8(R2)                                                  
         SPACE 1                                                                
HK30     LA    R2,MPRPJ2H                                                       
         CLI   5(R2),0                                                          
         BE    HK40                                                             
         A     R1,MPHWIDTH         POINT TO HEADLINE 5                          
         MVC   88(3,R1),=C'PJ2'    AND PJ2, IF THERE IS ONE                     
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   92(0,R1),8(R2)                                                   
         LA    R2,MPROV2H          AND OVERRIDE 2, IF THERE IS ONE              
         CLI   5(R2),0                                                          
         BE    HK40                                                             
         MVC   104(3,R1),=C'OV2'                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   108(0,R1),8(R2)                                                  
         B     HK40                                                             
         SPACE 1                                                                
*   WIDE PRINTING HEADLINES                                                     
         SPACE 1                                                                
HK33     LA    R2,MPRPJ1H          MOVE IN PJ1                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   125(0,R1),8(R2)                                                  
         LA    R2,MPROV1H          AND OVERRIDE 1, IF THERE IS ONE              
         CLI   5(R2),0                                                          
         BE    HK35                                                             
         MVC   137(3,R1),=C'OV1'                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   141(0,R1),8(R2)                                                  
         SPACE 1                                                                
HK35     LA    R2,MPRPJ2H                                                       
         CLI   5(R2),0                                                          
         BE    HK40                                                             
         A     R1,MPHWIDTH          POINT TO HEADLINE 5                         
         MVC   121(3,R1),=C'PJ2'    AND PJ2, IF THERE IS ONE                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   125(0,R1),8(R2)                                                  
         LA    R2,MPROV2H          AND OVERRIDE 2, IF THERE IS ONE              
         CLI   5(R2),0                                                          
         BE    HK40                                                             
         MVC   137(3,R1),=C'OV2'                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   141(0,R1),8(R2)                                                  
         SPACE 1                                                                
HK40     ZIC   RE,LDHD                                                          
         L     R1,MPAH8                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   52(0,R1),DHD1       DEMO HEADLINES                               
         A     R1,MPHWIDTH         POINT TO HEADLINE 9                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   52(0,R1),DHD2                                                    
         SPACE 1                                                                
         OC    ABOX,ABOX           SET UP BOXES IF NEEDED                       
         BZ    MPXIT                                                            
         SPACE 1                                                                
* PUT SOME NON-PRINTING CHARACTER AT END OF H10 TO GET BOXES CORRECTLY          
         SPACE 1                                                                
         A     R1,MPHWIDTH         POINT TO HEADLINE 10                         
         LA    R1,54(R1)           + 54                                         
         LA    R1,0(RE,R1)         + LENGTH OF DEMO HEADLINE                    
         MVI   0(R1),X'FE'         = END OF LINE                                
         SPACE 1                                                                
         L     R1,ABOX             A(BOX DSECT)                                 
         USING BOXD,R1                                                          
         MVC   BOXROWS,SPACEW                                                   
         MVI   BOXROWS+6,C'T'      ENCLOSE COL. HEADINGS IN A BOX               
         MVI   BOXROWS+9,C'B'                                                   
         MVI   BOXROWS+10,C'T'                                                  
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACEW                                                   
         MVC   BOXCOLSR,SPACEW                                                  
         MVI   BOXCOLS,C'L'                                                     
         LA    RE,BOXCOLS+51                                                    
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=H'6'                                                         
         LA    RE,0(RF,RE)                                                      
         MVI   0(RE),C'R'                                                       
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R1                                                               
         SPACE 2                                                                
HKX      B     MPXIT                                                            
         SPACE 2                                                                
DPTBL    DS    0CL21                                                            
         DC    C'APRIME ACCESS        '                                         
         DC    C'DDAYTIME             '                                         
         DC    C'EEARLY FRINGE        '                                         
         DC    C'FFRINGE              '                                         
         DC    C'JSPORTS              '                                         
         DC    C'KKIDS                '                                         
         DC    C'LLATE FRINGE         '                                         
         DC    C'MMORNING             '                                         
         DC    C'NNEWS                '                                         
         DC    C'OSOAPS               '                                         
         DC    C'PPRIME               '                                         
         DC    C'REARLY NEWS          '                                         
         DC    C'SSPECIALS            '                                         
         DC    C'TLATE NEWS           '                                         
         DC    C'VMOVIES              '                                         
         DC    C'WWEEKEND             '                                         
         DC    C'XCOMPETITIVE         '                                         
         DC    C'YLOCAL               '                                         
         DC    C'ZDAYPART SUMMARY     '                                         
         DC    X'FF'                                                            
         DC    C'UNKNOWN             '                                          
         EJECT                                                                  
***THIS ROUTINE SETS UP THE DEMO HEADLINES                                      
DEMOHDS  ST    RE,FULL                                                          
         XC    DHD1,DHD1                                                        
         XC    DHD2,DHD2                                                        
         ZIC   R6,NUMDEMS                                                       
         SPACE 1                                                                
         LR    R3,R6               NUMBER OF DEMOS                              
         MH    R3,=H'6'            X LENGTH OF DEMO EXPRESSION                  
         SH    R3,=H'2'            - 1 FOR LAST SPACE, - 1 FOR EXECUTE          
         STC   R3,LDHD             = LENGTH OF DEMO HEADLINES                   
         SPACE 1                                                                
         LA    R3,DHD1                                                          
         LA    R5,DHD2                                                          
         LA    R2,DEMOS                                                         
         LA    R4,BLOCK                                                         
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'       1 DECIMAL RTG                                
         MVI   DBXTTSP,X'01'       1 DECIMAL SHARES                             
         MVI   DBXTTIP,X'02'       IMP = 00'S                                   
         SPACE 1                                                                
DH50     CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,DMCB,(0,(R2)),(5,WORK),(0,DBLOCKD)                       
         DROP  R4                                                               
         MVC   0(5,R3),WORK                                                     
         MVC   0(5,R5),WORK+5                                                   
         LA    R2,3(R2)                                                         
         LA    R3,6(R3)                                                         
         LA    R5,6(R5)                                                         
         BCT   R6,DH50                                                          
         SPACE 1                                                                
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* THIS ROUTINE BUILDS THE PRINT LINE FOR THE INDEX BOOK                         
         SPACE 2                                                                
BLDPCL   ST    RE,FULL                                                          
         MVC   PLIN(29),=C'PERCENT CHANGE MMMYY VS MMMYY'                       
         LA    R3,BOOKS                                                         
BP10     CLI   4(R3),X'FF'                                                      
         BE    BP20                                                             
         LA    R3,4(R3)                                                         
         B     BP10                                                             
         SPACE 1                                                                
BP20     ZIC   R1,2(R3)            LAST BOOK REQUESTED                          
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   PLIN+15(3),0(R1)                                                 
         EDIT  (1,1(R3)),(2,PLIN+18)                                            
         SPACE 1                                                                
         LA    R3,CINDEX           INDEX BOOK                                   
         ZIC   R1,2(R3)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   PLIN+24(3),0(R1)                                                 
         EDIT  (1,1(R3)),(2,PLIN+27)                                            
         SPACE 1                                                                
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         B     MPXIT                                                            
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         B     MPXIT                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* MY OWN ERROR MESSAGES                                                         
         SPACE 2                                                                
INVSTN   DC    C'INVALID STATION'                                               
NOSTA    DC    C'STATION NOT FOUND'                                             
NOMKT    DC    C'MARKET NOT FOUND'                                              
NOSCH    DC    C'SCHEME NOT FOUND'                                              
NOPERD   DC    C'PERIOD NOT FOUND'                                              
         SPACE 4                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    C'X'                                                             
*                                                                               
CTLSWTCH DC    C'N'                CONTROL FILE SWITCH                          
*                                                                               
SPACEW   DC    198C' '                                                          
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
         SPACE 2                                                                
REGSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,89,AGYNAME                                                    
         PSPEC H2,89,RUN                                                        
         PSPEC H2,114,PAGE                                                      
         PSPEC H1,51,C'MARKET PROFILE REPORT'                                   
         PSPEC H2,51,C'------ ------- ------'                                   
         PSPEC H1,2,C'MARKET'                                                   
         PSPEC H2,2,C'DAYPART'                                                  
         PSPEC H3,2,C'SERVICE'                                                  
         PSPEC H8,2,C'STA  DAY(S)  TIME        PROGRAM          BOOK'           
         SPACE 1                                                                
         SPROG 1                                                                
         PSPEC H3,89,REPORT                                                     
         PSPEC H3,105,REQUESTOR                                                 
         PSPEC H4,89,C'PJ1'                                                     
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 3                                                                
WIDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         WSPEC H1,122,AGYNAME                                                   
         WSPEC H2,122,RUN                                                       
         WSPEC H2,147,PAGE                                                      
         WSPEC H1,72,C'MARKET PROFILE REPORT'                                   
         WSPEC H2,72,C'------ ------- ------'                                   
         WSPEC H1,2,C'MARKET'                                                   
         WSPEC H2,2,C'DAYPART'                                                  
         WSPEC H3,2,C'SERVICE'                                                  
         WSPEC H8,2,C'STA  DAY(S)  TIME        PROGRAM          BOOK'           
         SPACE 1                                                                
         SPROG 1                                                                
         WSPEC H3,122,REPORT                                                    
         WSPEC H3,138,REQUESTOR                                                 
         WSPEC H4,122,C'PJ1'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
PRTD     DSECT                                                                  
         DS    CL1                                                              
PSTAT    DS    CL4                                                              
         DS    CL1                                                              
PDAY     DS    CL7                                                              
         DS    CL1                                                              
PTIM     DS    CL11                                                             
         DS    CL1                                                              
PPRG     DS    CL16                                                             
         DS    CL1                                                              
PBOOK    DS    CL8                                                              
         DS    CL1                                                              
PDEM     DS    CL111                                                            
         SPACE 4                                                                
UPGD     DSECT                                                                  
CUPTYPE  DS    XL1                 UPGRADE TYPE                                 
*CUPTRTG EQU   2                   RATING UPGRADE                               
*CUPTHUT EQU   3                   HUT UPGRADE (CUPSTYP NE P)                   
*CUPTPUT EQU   3                   PUT UPGRADE (CUPSTYP EQ P)                   
*                                  CUPFLD1 = OLDHPT SOURCE BOOK                 
*CUPTNDX EQU   4                   INDEX UPGRADE                                
*CUPTHPT EQU   6                   H/P/T UPGRADE                                
CUPSTYP  DS    XL1                 UPGRADE SUB-TYPE (P=PUT UPGRADE)             
*                                  UGRADE BOOK/INDEX VALUES                     
CUPFLD1  DS    XL2                                                              
CUPFLD2  DS    XL2                                                              
CUPFLD3  DS    XL2                                                              
CUPFBK   DS    XL2                 FROM BOOK (SHARES)                           
CUPUDAY  DS    XL1                 DAY CODE                                     
CUPUTIM  DS    XL4                 START AND END TIMES (BINARY)                 
CUPSTA   DS    CL5                 STATION CALL LETTERS                         
CUPBTYPE DS    CL1                 UPGRADE BOOK TYPE                            
         DS    CL9                 *** SPARE ***                                
         EJECT                                                                  
* DDWIDED                                                                       
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERESFFD                                                                      
* DDGENTWA                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
         EJECT                                                                  
RMKTRECD DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
         EJECT                                                                  
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         EJECT                                                                  
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF8D                                                       
         EJECT                                                                  
*                                                                               
*               WORK AREA                                                       
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
         DS    0F                                                               
MYWORK   DS    0CL600                                                           
SAVEKEY  DS    CL27                                                             
SV2BKEY  DS    CL27                SAVE MARKET RECORD KEY                       
MKTSTA   DS    CL60                12 PACKED MARKET/STATION @ 5 BYTES           
TRACEOPT DS    C                                                                
SAVBOK   DS    CL2                                                              
THISBOOK DS    CL4                                                              
ADAYPART DS    A                                                                
ACLPACK  DS    A                                                                
ABUFF    DS    A                   POINTER TO NEXT PRINT LINE IN BUFF           
AINDEX1  DS    A                   POINTER TO INDEX AREA 1 (BUFF+5000)          
AINDEX2  DS    A                   POINTER TO INDEX AREA 2 (BUFF+5200)          
DHD1     DS    CL111                                                            
DHD2     DS    CL111                                                            
LDHD     DS    CL1                 LENGTH OF DEMO HEADLINES                     
SVUPPRG  DS    CL16                                                             
PLIN     DS    CL29                PERCENT CHANGE LINE FOR INDEX BOOK           
STAPRNT  DS    CL1                 Y=CALL LETTERS HAVE BEEN PRINTED             
NEWSTN   DS    CL1                 Y=NEW STATION                                
MPAH1    DS    A                   A(FIRST HEADLINE) - H1 OR XHEAD1             
MPAH8    DS    A                   A(8TH HEADLINE) - H8 OR XHEAD8               
MPAP1    DS    A                   A(FIRST PRINTLINE) - P OR XP                 
MPPWIDTH DS    F                   WIDTH OF PRINT LINES IN BUFF-132/165         
MPHWIDTH DS    F                   WIDTH OF HEADLINES (132 OR 198)              
         SPACE 3                                                                
       ++INCLUDE SPDEMLK                                                        
SBUFF    DS    A                   A(START OF PRINT BUFFER)                     
LOCLEN   EQU   *-MYWORK                                                         
         EJECT                                                                  
*  DDREPMASTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
*  SPDEMUPD                                                                     
*        PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
*  DDTWADCONS                                                                   
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023RERES09   11/14/07'                                      
         END                                                                    
