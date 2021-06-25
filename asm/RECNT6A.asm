*          DATA SET RECNT6A    AT LEVEL 202 AS OF 06/19/15                      
*PHASE T8026AA                                                                  
*INCLUDE REDARTKO                                                               
*INCLUDE REREPDIF                                                               
*INCLUDE REGENDHT                                                               
         TITLE 'T8026A - REP SEND'                                              
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT6A (T8026A) --- REP SEND                            *             
*                                                                 *             
* --------------------------------------------------------------- *             
* REFER TO RECNTHIST FOR PAST HISTORY                             *             
*                                                                 *             
* 16JUN15 KUI BYPASS STATION PROFILE 4 CFX CHECK FOR KATZ REPS    *             
*         COMMENTED OUT RADIO BAND CHECKS FOR ADDRESSIBILITY      *             
* 20DEC13 KUI REMOVE OBSELETE TAKEOVER ROUTING LOGIC (DSREPTK-26) *             
* 31OCT13 KUI SUPPORT OFFER RIS KEYS                              *             
* 04SEP12 KWA ADD STATION USER NAME TO AUDIT RECORD               *             
* 06DEC11 BOB CREATE AUDIT TEXT RECORDS ON SEND/CONFIRM           *             
* 29MAR11 SKU UNVISION ROLLBACK: CHECK STAD ADD OPT 3 CFX ACTION  *             
* 04FEB11 SKU UNVISION ROLLBACK: CFX FIX                          *             
* 27JAN11 SKU UNVISION ROLLBACK: CHECK STAD ADD OPT 2 SEND ACTION *             
* 08JUL09 SKU ESPERANTO SUPPORT                                   *             
* 01OCT07 SKU PUT BACK RCONSSID CODE FOR TEL                      *             
* 07APR06 BU  TRAPMODS: SENSITIZE TO CONTRACT VER #               *             
* 18MAR05 HQ  SKIP NON-HEADER PLAN RECORD ON BUYCODE VALIDATION   *             
* 23SEP04 HQ  FIX SEND= 3 DIGIT VERSION NUMBER                    *             
* 20AUG04 HQ  MERGE TV AND RADIO CODE                             *             
* 13APR04 BU  MODIFY MISSING BUYCODE MESSAGE                      *             
* 09MAR04 BU  IGNORE ALL MANDATORY CHECKS FOR 'LAST'              *             
* 08MAR04 BU  SKIP CANCELLED BUYS IN MANDATORY SCAN               *             
* 01MAR04 BU  BUYCODES NOT MANDATORY FROM STATION SIDE            *             
* 24FEB04 BU  SCAN FOR BUYCODES IF MANDATORY PROFILE SET          *             
* 02JAN04 SKU HAPPY NEW YEAR! REMOVE 5 MIN RULE FOR ROM           *             
* 07MAR03 SKU MAKE CONTRACT ROM AWARE                             *             
* 07DEC02 BU  PRINTER DESIGNATION                                 *             
* 12NOV02 HQ  HANDLE CONFIRM CONTRACT FROM DARE                   *             
* 23OCT02 SKU REDI SUPPORT                                        *             
* 22APR02 BU  SET CONFIRM DATE SWITCH TO TODAY,NOT MONDAY         *             
* 10OCT01 SKU FOR DARE KEYS, SKIP MATCHING ON AGENCY OFFICE       *             
* 22AUG01 SKU ADD BLAIR TO GETREPID LIST                          *             
* 20JUN01 SKU ADD FSS FOR FOX STATION SALES MOVE                  *             
* 29MAY01 SKU REMOVE OUTDATED MISSING RCONSSID CHECKS             *             
* 11DEC00 BU  TRADE AGENCY CODE PROCESSING                        *             
* 07JUL00 RHV NAT'L-->LOCAL ORDER CONVERSION                      *             
* 30NOV99 SKU MORE SPECIAL ID MAPPING FOR EAGLE TV                *             
* 04NOV99 SKU SPECIAL ID MAPPING FOR EAGLE TV                     *             
* 08OCT99 SKU INCLUDE SZ AND CQ IN GETREPID                       *             
* 02JUN99 RHV SONNET 9F KEY FOR NBC HOME MKT                      *             
* 29JAN99 RHV SAVE FIRST SEND DATES                               *             
* 14DEC98 SKU FIX DARE/CFX CONFIRM BUG                            *             
* 10SEP98 JRD UPDATE SONNET CODE FOR NEW ELEMENTS                 *             
* 06JUL98 RHV CFX STAMP REP SIDE, NOT STA                         *             
* 17JUN98 RHV CHECK FLIGHT END DATE ON 1ST SEND                   *             
* 26MAR98 SKU ALLOW CONFIRMATION IF REVISION CANCELLED (REVCAN)   *             
*             ALLOW SEND IF REVISION HAS NOT BEEN PROCESSED       *             
* 03MAR98 SKU RESET DARE REVISION MANUAL FLAG FOR SEND ACTION     *             
* 25NOV97 SKU TKO DARE CONFIRM BUG FIX                            *             
* 27OCT97 SKU DARE CONFIRM RESETS REVISION COUNTER                *             
* 26AUG97 SKU DARE CONFIRM RESETS MANUAL CHANGE FLAG              *             
* 24JUL97 SKU 4K CONTRACT SUPPORT                                 *             
* 04APR97 RHV TEMPORARY FORCE GETREPID FOR FOX CONVERTED ORDERS   *             
* 28MAR97 SKU SUPPORT VARIOUS/BRAND                               *             
* 18MAR97 RHV NEW CF ACTION LOGIC                                 *             
* 26FEB97 SKU CFX BUG FIX                                         *             
* 14FEB97 SKU CHECK IF CONTRACT TYPE SPECIFIED FOR AUTOHEADER     *             
* 27DEC96 SKU ALLOW 5 MINUTES BEFORE NEXT DARE TRANSACTION (CONF) *             
* 06DEC96 SKU BUG FIX FOR SONNET IF MAKEGOOD OFFER EXISTS         *             
* 01NOV96 SKU CLEAR WIP LOCK BITS FOR ACTION MGS                  *             
* 29OCT96 SKU CANNOT DO MGS FOR GRAPHNET STATIONS                 *             
* 03OCT96 SKU SUPPORT LOW POWER STATION                           *             
* 19SKU96 SKU FIX SEND=XX BUG TO SHOW 3 DIGIT VERSION NUMBERS     *             
* 30JUL96 RHV REQUIRE STD CMT IF STATION OPTION ON                *             
* 25JUN96 SKU EDI AGENCY LEO ORDER MOD STARTS AT ZERO             *             
* 14JUN96 PXZ 8F KEY CHANGES                                                    
* 10JUN96 SKU ALLOW EOP CODES FOR K AND C FORMATS                 *             
* 04JUN96 SKU RE-MAP KATZ OFFICE IN GETREPID                      *             
* 30MAY96 RHV DATE STAMP & ZERO SPL ELEMENT ON CONFIRM            *             
* 29MAY96 SKU ADD KATZ TV TO GETREPID                             *             
* 08APR96 SKU SKIP EDI AGENCY '1342' DMBB 'E1' KEY GEN            *             
*         PXZ ADD PASSIVE KEYS FOR RIS                            *             
* 29MAR96 RHV WRITE CONFIRM DATE&TIME TO NEW X'22' ELEM           *             
* 22MAR96 SKU EXPAND MG SEND ELEMENT X'21' TO INCLUDE SEPARATE    *             
*              DATE/TIME STAMP FOR REP AND STA                    *             
*             ADD PETRY TO GETREPID                               *             
* 15MAR96 RHV SET K BUCKET ACTIVITY DATE TO MONDAY OF CONFIRM     *             
*              WITH CONFIRM ACTION & PROFILE 27                   *             
* 20FEB96 SKU FOR KATZ CONVERTED CONTRACTS, LOOK UP THE REP       *             
*              SENDING ID IF STATION IS FIRST TO SEND OR CONFIRM  *             
* 05FEB96 RHV ADD NEW 'CFX' ACTION FOR REPS                       *             
* 01FEB96 SKU KATZ EDI SUPPORT                                    *             
* 14DEC95 SKU ADJUST FOR NEW DDS START TIME                       *             
* 11DEC95 SKU 2K CONTRACT SUPPORT                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8026A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T8026A,R9,RR=R3,CLEAR=YES                        
         LR    R8,RC                                                            
         USING MYWORKD,R8          LOCAL WORK AREA                              
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         ST    R3,RELO                                                          
         EJECT                                                                  
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    CONCNUMH+4,X'20'      SAME K?                                    
         BO    *+8                   YES                                        
         NI    TWASNDFL,X'FF'-X'80'  NO -TURN OFF THIS FLAG                     
         OI    CONCNUMH+4,X'20'    BUT CALL FIELD VALID NOW                     
         DROP  RF                                                               
         EJECT                                                                  
*&&DO                                                                           
         CLI   RCONKSTA+4,C'F'     FM RADIO STATION?                            
         BE    MAIN03              YES - SKIP BUY CODE PROCESSING               
         CLI   RCONKSTA+4,C'A'     AM RADIO STATION?                            
         BE    MAIN03              YES - SKIP BUY CODE PROCESSING               
*&&                                                                             
*                                                                               
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    MAIN03              YES - NO RESTRICTION                         
*                                                                               
         CLC   =C'SEND',CONACT     ACTION = SEND?                               
         BNE   MAIN01              CHECK STAD ADD OPT 2 IF PERMITTED            
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWASTADF,X'40'                                                   
         BZ    MAIN01                                                           
         DROP  RF                                                               
*                                                                               
         LA    R3,1026             SET ERROR MESSAGE                            
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
MAIN01   DS    0H                                                               
         CLC   =C'CFX',CONACT      ACTION = CFX?                                
         BNE   MAIN02              CHECK STAD ADD OPT 4 IF PERMITTED            
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWASTADF,X'10'                                                   
         BZ    MAIN02                                                           
         DROP  RF                                                               
*                                                                               
* FOR KATZ MIGRATION TO STRATA, ALLOW CFX ACTION GOING FORWARD                  
* REGARDLESS OF STATION ADDITIONAL OPTION 4 SETTING                             
* (6/16/15 SKUI)                                                                
*                                                                               
         CLC   =C'AM',RCONKREP                                                  
         BE    MAIN02                                                           
         CLC   =C'CQ',RCONKREP                                                  
         BE    MAIN02                                                           
         CLC   =C'SZ',RCONKREP                                                  
         BE    MAIN02                                                           
         CLC   =C'TV',RCONKREP                                                  
         BE    MAIN02                                                           
*                                                                               
         LA    R3,1027             SET ERROR MESSAGE                            
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
*   BUYCODE PROFILE SET ON:                                                     
MAIN02   DS    0H                                                               
         TM    PROFILES+CNTBYCDB,CNTBYCDA                                       
*                                  BUYCODES MANDATORY?                          
         BNO   MAIN03              NO                                           
*                                                                               
         CLC   =C'LAST',CONACT     YES - ACTION = LAST?                         
         BE    MAIN03              YES - SKIP MANDATORY CHECK                   
*                                                                               
         GOTO1 =A(BUYCODES),RR=Y                                                
*                                  YES - CHECK ALL BUYS                         
         BZ    MAIN03              ALL BUYS HAVE CODES                          
*                                                                               
         LA    R3,990              SET ERROR MESSAGE                            
*                                                                               
         MVC   CONCACT,MYSPACES                                                 
         MVC   CONCACT(8),=C'DIS,BCD,'                                          
         EDIT  RBUYKMLN,(3,CONCACT+8),ALIGN=LEFT                                
         FOUT  CONCACTH                                                         
         OI    CONCACTH+6,X'40'    FORCE CURSOR TO ACTION                       
***      L     RD,BASERD                                                        
         B     ERROR                                                            
*                                                                               
MAIN03   EQU   *                                                                
         CLI   RCONKSTA+4,C'F'     FM RADIO STATION?                            
         BE    MAIN05              YES                                          
         CLI   RCONKSTA+4,C'A'     AM RADIO STATION?                            
         BNE   MAIN30              NO - SKIP                                    
*                                                                               
MAIN05   DS    0H                                                               
         CLC   =C'SEND',CONACT     DO ONLY FOR ACTION SEND                      
         BNE   MAIN30                                                           
         TM    TWAGENFG,TWQGOOPN                                                
         BZ    MAIN10                                                           
         GOTOR SENDOPEN                                                         
         B     EXXMOD                                                           
*                                                                               
MAIN10   DS    0H                                                               
         TM    TWAGENFG,TWQGOSND                                                
         BO    MAIN20                                                           
         GOTOR PROCREDI            PROCOESS FOR RADIO EDI                       
         BNZ   EXXMOD                                                           
*                                                                               
MAIN20   DS    0H                                                               
*        NI    TWAGENFG,X'FF'-TWQGOSND                                          
*                                                                               
***********************************************************************         
* CHECK IF ANY MAKEGOODS ARE PRESENT FOR THIS ORDER AND FLAG IF THEY            
* EXIST                                                                         
***********************************************************************         
MAIN30   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'        MAKEGOODS?                                   
         BAS   RE,GETEL                                                         
         BNE   SEND2                                                            
         USING RCONMGEL,R6                                                      
         NI    RCONMGCT,X'FF'-X'80' SET NO MG OFFERS EXISTS                     
*                                                                               
KEYD     USING RMKGKEY,KEY         YES!                                         
         XC    KEY,KEY                                                          
         MVI   KEYD.RMKGKTYP,X'11'                                              
         MVC   KEYD.RMKGKREP,TWAAGY   INSERT POWER CODE                         
         MVC   KEYD.RMKGKOFF,RCONKOFF INSERT OFFICE CODE                        
         MVC   KEYD.RMKGKSTA,RCONKSTA INSERT STATION CALL LETTERS               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEYD.RMKGKCON,TWACNUM                                            
         DROP  RF                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         B     CHKMG20                                                          
*                                                                               
CHKMG10  DS    0H                                                               
         GOTO1 VSEQ                                                             
*                                                                               
CHKMG20  DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   SEND2               ANY MAKEGOOD OFFERS FOUND?                   
         OC    KEYD.RMKGKPLN(6),KEYD.RMKGKPLN                                   
         BNZ   CHKMG10             WANT HEADERS ONLY                            
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         TM    RMKGSCST,RMKGSAPQ   SKIP OFFER IF APPLIED                        
         BO    CHKMG10                                                          
         OI    RCONMGCT,X'80'      MG OFFERS ATTACHED                           
         DROP  KEYD,R6                                                          
         EJECT                                                                  
***********************************************************************         
*  THIS ROUTINE IS FOR ACTION SEND AND CONFIRM FOR AN ACE STATION               
***********************************************************************         
SEND2    DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    SEND2A              YES - SKIP CHECK                             
*                                                                               
* CHECK IF MISSING CONTRACT TYPE IN CASE OF AUTOHEADER                          
*                                                                               
         TM    PROFILES+CNTVTYPB,CNTVTYPA   INPUT REQUIRED?                     
         BZ    SEND2AA             NO                                           
         LA    R3,571                                                           
         LA    R2,CONCACTH                                                      
         CLI   RCONTYPE,0                                                       
         BE    ERROR                                                            
*                                                                               
SEND2AA  DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         TM    TWASTAOP,X'80'      STD K CMT REQUIRED BY STATION PROF?          
         BZ    SEND2A              NO - SKIP CHECK                              
         LA    R2,CONCOM1H         YES                                          
         CLC   =C'C=',8(R2)        STD CMT IN 1ST K CMT FIELD?                  
         BE    SEND2A              YES - OK                                     
         LA    R3,600              NO - NOT OK                                  
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
         DROP  RF                                                               
*                                                                               
SEND2A   DS    0H                                                               
         LA    R2,CONCACTH                                                      
         CLC   =C'MGS',CONACT      FOR ACTION 'MGS'                             
         BNE   SEND3                                                            
*                                                                               
         LA    R3,626              GRAPHNET NOT SUPPORTED FOR MGS               
         TM    RCONMODR+1,X'40'                                                 
         BO    ERROR                                                            
*                                                                               
         LA    R3,498              NO MAKEGOOD OFFER TO SEND                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'        ALL K W/MGO WILL HAVE THIS                   
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                                                               
SEND2AC  OC    RCONSSID,RCONSSID                                                
         BNZ   SEND35                                                           
*                                                                               
SEND2B   EQU   *                                                                
         CLI   RCONKSTA+4,C'F'     FM RADIO STATION?                            
         BE    SEND2B02            YES                                          
         CLI   RCONKSTA+4,C'A'     AM RADIO STATION?                            
         BNE   SEND2B80            NO  - NOT RADIO: SKIP                        
SEND2B02 EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAGENFG,TWQMASTR   MASTER PROCESS FROM DARE?                    
         DROP  RF                                                               
         BNO   SEND2B80            NO                                           
         LA    R6,PIDTABLE         SET A(PRINTER ID TABLE)                      
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         CLI   FASYS,X'38'         TEST SYSTEM?                                 
         BNE   SEND2B10            NO  - USE PIDTABLE                           
         LA    R6,PIDTABL2         YES - USE PIDTABL2                           
         DROP  RF                                                               
SEND2B10 EQU   *                                                                
         CLI   0(R6),0             END OF TABLE                                 
         BE    SEND2B70            NOT IN TABLE: USE GETREPID                   
         CLC   TWAUSRID,DSIGNON(R6)                                             
*                                  SIGNON TWAUSRID IN TABLE?                    
         BNE   SEND2B30            NO                                           
         CLC   RCONKREP,DPOWERCD(R6)                                            
*                                  YES - FOR THIS REP OF ORDER?                 
         BNE   SEND2B30            NO                                           
         MVC   WORK(2),DSSID(R6)   YES - USE THIS PRINTER SYS ID                
         B     SEND2B90                                                         
SEND2B30 EQU   *                                                                
         LA    R6,LPIDTABL(R6)     BUMP TO NEXT TABLE ENTRY                     
         B     SEND2B10            GO BACK FOR NEXT                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE REDARPID                                                       
         EJECT                                                                  
SEND2B70 EQU   *                                                                
         GOTOR MASTERID            MISSING. WE NEED TO GET THE REP ID           
         B     SEND2B90                                                         
*                                                                               
SEND2B80 EQU   *                                                                
         GOTOR GETREPID            MISSING. WE NEED TO GET THE REP ID           
*                                                                               
SEND2B90 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         MVC   RCONSSID,WORK                                                    
         B     SEND35                                                           
         DROP  R6                                                               
                                                                                
SEND3    DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   SEND40                                                           
                                                                                
         CLC   CONACT,=C'LAST'     FOR ACTION 'LAST'                            
         BE    SEND35              ONLY NEED TO DO GETTIME                      
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    SEND5                                                            
         DC    H'0'                                                             
                                                                                
         USING RCONSEND,R6                                                      
SEND5    LA    R3,185         ERROR, REP MUST SEND BEFORE MORE ACTION           
*                                                                               
*&&DO                                                                           
         CLC   RCONKREP,=C'FN'     FOX?                                         
         BNE   SEND5B              NO                                           
         CLC   RCONKSTA(4),=C'KTTV'                                             
         BE    SEND5A              FORCE REP ID LOOKUP (FOR NOW)                
         CLC   RCONKSTA(4),=C'WFLD'                                             
         BE    SEND5A              FORCE REP ID LOOKUP (FOR NOW)                
         CLC   RCONKSTA(4),=C'WTXF'                                             
         BE    SEND5A              FORCE REP ID LOOKUP (FOR NOW)                
         CLC   RCONKSTA(4),=C'WFXT'                                             
         BE    SEND5A              FORCE REP ID LOOKUP (FOR NOW)                
         CLC   RCONKSTA(4),=C'WNYW'                                             
         BNE   SEND5B              NOT ONE OF THESE STATIONS                    
*                                                                               
SEND5A   CLC   RCONKCON,=X'02512334'                                            
         BNH   SEND5C              FORCE GET ID                                 
*                                                                               
SEND5B   OC    RCONSSID,RCONSSID   STATION CAN'T BE FIRST TO SEND               
         BZ    SEND5BA                                                          
         CLC   =C'AM',REPALPHA     FOR KATZ/NOW EAGLE                           
         BNE   SEND8                                                            
         CLC   RCONSSID,=AL2(8162) 8162=FIRST EAGLE ID (ETVNY)                  
         BL    SEND5C              ANYTHING BELOW IS STILL KAMXX                
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        CHECK IF TAKEOVER                            
         BAS   RE,GETEL                                                         
         BE    SEND5C                                                           
         B     SEND8                                                            
*****                                                                           
***** SPECIAL FOR KATZ AMERICA, CONTINENTAL AND SELTEL                          
*****                                                                           
SEND5BA  DS    0H                                                               
         CLC   =C'AM',REPALPHA                                                  
         BE    SEND5C                                                           
         CLC   =C'CQ',REPALPHA                                                  
         BE    SEND5C                                                           
         CLC   =C'SZ',REPALPHA                                                  
         BE    SEND5C                                                           
*****                                                                           
         TM    RCONMODR+1,X'10'    CHECK IF KATZ CONVERTED CONTRACT             
         BZ    ERROR               YES, SINCE THE REP SENDING IS                
*&&                                                                             
         OC    RCONSSID,RCONSSID   MUST BE A TAKEOVER CONVERTED                 
         BNZ   SEND8               CONTRACT                                     
SEND5C   GOTOR GETREPID            MISSING. WE NEED TO GET THE REP ID           
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         MVC   RCONSSID,WORK                                                    
         DROP  R6                                                               
*                                                                               
SEND8    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                                                               
         CLC   =C'RSND',CONACT     RESEND OR MAKEGOOD SEND?                     
         BNE   SEND10              IF NOT, ACTION MUST BE SEND/CONF             
                                                                                
         LA    R3,260              STA NOT LAST TO SEND                         
         TM    RCONSENF,X'40'                                                   
         BZ    ERROR                                                            
         B     SEND35                                                           
                                                                                
SEND10   CLC   CONACT,=C'CF  '                                                  
         BNE   SEND20                                                           
*                                                                               
* FOR ACTION (ACE) CONFIRM ONLY                                                 
*                                                                               
         LA    R3,185                                                           
         TM    RCONSENF,X'40'      STATION CAN'T CONF IF LAST SENT BY           
         BO    ERROR               STATION                                      
         LA    R3,167              ERROR, LATEST VERSION NOT YET SENT           
         TM    RCONSENF,X'10'      ONLY CONFIRM IF LATEST STATION               
         BZ    ERROR                                                            
         LA    R3,168              ERROR, LATEST VERSION NOT YET SENT           
         TM    RCONSENF,X'20'      AND REP VERSION SENT                         
         BZ    ERROR                                                            
         B     SEND30                                                           
*                                                                               
* FOR ACTION SEND ONLY, ONLY ALLOW SEND IF CHANGES MADE SINCE LAST SEND         
*                                                                               
SEND20   LA    R3,183              NO CHANGES SINCE LAST SEND                   
         TM    RCONSENF,X'10'      STATION VERSION NOT ADVANCED                 
         BO    ERROR                                                            
*                                                                               
* ONLY ALLOW SEND IF CONTRACT ORDER COMMENT EXISTS                              
*                                                                               
         LA    R3,128              ERROR, NO ORDER COMMENT                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
*                                                                               
* R6 NOT AT SEND ELEM ANYMORE - GET IT BACK                                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    SEND30                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
* FOR SEND AND CONFIRM, VERIFY THAT STA IS USING CORRECT VER                    
*                                                                               
SEND30   DS    0H                                                               
         CLC   =C'SEND',CONACT                                                  
         BNE   SEND33                                                           
         OC    STLNUM,STLNUM       IF SEND=X SPECIFIED                          
         BZ    SEND31                                                           
         CLC   RCONSRV,STLNUM      X MUST BE THE LASTEST REP VER                
         BNE   SEND32                                                           
         B     SEND35                                                           
SEND31   TM    RCONSENF,X'40'      IF STA WAS LAST TO SEND                      
         BO    SEND35              NO NEED TO DO SEND=X                         
*                                                                               
* DISPLAY SOFT ERROR MESSAGE WITH ACTUAL REP VERSION # NEEDED                   
*                                                                               
SEND32   DS    0H                                                               
         LA    R3,462              NEED SEND=X, X=LATEST REP VER                
         EDIT  RCONSRV,(3,WORK2),ALIGN=LEFT                                     
         LA    RF,WORK2            BUFFER FOR VERSION NUMBER                    
         ST    RF,DMCB+12                                                       
         STC   R0,DMCB+12          INDICATE SIZE OF VERSION NUMBER              
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    CONCACTH+6,X'40'    FORCE CURSOR TO ACTION                       
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
                                                                                
SEND33   DS    0H                                                               
         CLC   =C'CF ',CONACT     FOR STATION CONFIRMATION                      
         BNE   SEND35                                                           
         LA    R3,186              ERROR, MUST CONF LATEST VERSION              
         CLC   RCONSRV,STLNUM      LATEST REP VER. TO INPUT                     
         BNE   ERROR                                                            
*                                                                               
* ADDITION CHECK FOR STATION CONFIRMATION                                       
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   SEND34                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         LA    R3,63               ERROR, CONTRACT ALREADY CONFIRMED            
         TM    RCONCONF,X'40'      CONFIRMED NOW??                              
         BO    ERROR               THIS ONLY HAPPENS IF REP JUST CFX'ED         
         DROP  R6                  STATION SHOULDN'T BE ABLE TO CONFIRM         
*                                                                               
* DARE CHECK FOR EXISTENCE OF AGENCY EQUIVALENCY CODE                           
*                                                                               
SEND34   DS    0H                                                               
         GOTOR VALIDARE,DMCB,(RC)                                               
*        GOTOR CFRMDARE,DMCB,(RC)                                               
                                                                                
SEND35   BAS   RE,GETTIME       TIME OF DAY FOR REPORT & SEND ELEMENT           
         B     RS2                                                              
         EJECT                                                                  
***********************************************************************         
* VERIFY REP CONTRACT ACTION                                                    
*  TAKEOVER CONTRACT - REP CAN 'DONE'                                           
*  ACE - REP CAN 'SEND' OR 'LAST' OR 'RSND' OR 'MGS' OR 'CFX'                   
*  GRAPHNET - REP CAN 'SEND' OR 'LAST' OR 'CNF' OR 'RSND' OR 'MGS'              
*             OR 'CFX'                                                          
***********************************************************************         
SEND40   DC    0H'0'                                                            
         LA    R3,75               INVALID CONTRACT ACTION                      
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ERROR                                                            
         SPACE 1                                                                
         CLC   =C'DONE',CONACT     ACTION 'DONE' FOR TAKEOVER CONTRACTS         
         BE    TAKEOVER                                                         
                                                                                
         CLC   CONACT,=C'LAST'     FOR ACTION 'LAST'                            
         BE    SEND70              ONLY NEED TO DO GETTIME                      
         SPACE 1                                                                
         CLC   CONACT,=C'SEND'                                                  
         BE    SEND51                                                           
         SPACE 1                                                                
         CLC   CONACT,=C'RSND'                                                  
         BE    SEND51                                                           
*                                                                               
         CLC   =C'MGS',CONACT                                                   
         BE    SEND70                                                           
*                                                                               
         CLC   =C'CFX',CONACT                                                   
         BE    SEND100                                                          
*                                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BZ    ERROR                                                            
         CLC   CONACT,=C'CF  '                                                  
         BE    SEND100                                                          
         B     ERROR                                                            
         EJECT                                                                  
*   DEAL WITH ACTION 'SEND' FOR ACE OR GRAPHNET                                 
         SPACE 1                                                                
SEND51   DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         LA    R3,281              SEND INVALID, IF RISK=6                      
         CLI   TWAARISK,6          PROHIBITS REP FROM SENDING ORDER             
         BE    ERROR                                                            
         DROP  RF                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    SEND51A                                                          
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
*  ONLY ALLOW SEND IF CHANGES MADE SINCE LAST SEND                              
*  ONLY ALLOW RSND IF NO CHANGES MADE SINCE LAST SEND                           
         SPACE 1                                                                
SEND51A  CLC   =C'SEND',CONACT                                                  
         BNE   SEND52                                                           
         LA    R3,183              NO CHANGES SINCE LAST SEND                   
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BNZ   ERROR                                                            
         B     SEND53                                                           
         SPACE 1                                                                
SEND52   DS    0H                                                               
         LA    R3,258              CHANGES SINCE LAST SEND, MUST SEND           
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    ERROR                                                            
         LA    R3,259              CONTRACT NOT LAST SENT BY REP                
         TM    RCONSENF,X'80'                                                   
         BNZ   SEND70                                                           
         B     ERROR                                                            
*                                                                               
* REP MUST DO SEND=X FOR NEW STATION VERSIONS                                   
* IF REP DID 'SEND=X', MAKE SURE IT'S LATEST STATION VERSION                    
*                                                                               
SEND53   DS    0H                                                               
         OC    STLNUM,STLNUM       IF SEND=X SPECIFIED                          
         BZ    SEND54                                                           
         CLC   RCONSSV,STLNUM      X MUST BE THE LASTEST STA VER                
         BNE   SEND55                                                           
         B     SEND59                                                           
SEND54   TM    RCONSENF,X'80'      IF REP WAS LAST TO SEND                      
         BO    SEND59              NO NEED TO DO SEND=X                         
         CLI   RCONSSV,0           SEND OK WITHOUT =X FOR FIRST TIME            
         BE    SEND59                                                           
         TM    RCONSENF,X'02'      IF STATION LAST SENT IS ACTUALLY             
         BO    SEND59              CONF ACTION, NO NEED TO DO SEND=X            
*                                                                               
* DISPLAY SOFT ERROR MESSAGE WITH ACTUAL STATION VERSION # NEEDED               
*                                                                               
SEND55   DS    0H                                                               
         LA    R3,461              NEED SEND=X, X=LATEST STA VER                
         EDIT  RCONSSV,(3,WORK2),ALIGN=LEFT                                     
         LA    RF,WORK2            BUFFER FOR VERSION NUMBER                    
         ST    RF,DMCB+12                                                       
         STC   R0,DMCB+12          INDICATE SIZE OF VERSION NUMBER              
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    CONCACTH+6,X'40'    FORCE CURSOR TO ACTION                       
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
*                                                                               
*  FOR REP, VERIFY THAT BUY TOTALS = ORDER TOTAL                                
*  ALSO CHECK BUYS VS. FLIGHT END DATE (CON PROFILE #37)                        
*                                                                               
SEND59   DS    0H                                                               
         XC    FULL,FULL           WILL HOLD BUCKET TOTAL                       
         XC    TEMP,TEMP           LATEST EFF END DATE & HAVE BUY FLAG          
         TM    RCONMODR+1,X'20'    IS THIS A MONTHLY?                           
         BNZ   SEND66              IF SO, ALLOW SEND                            
*                                                                               
*        CHECK BUY RECORDS TO SEE IF BUY WAS ADDED                              
*        AND CANCELLED WITHIN SAME WEEK, AND THEREFORE, NO BUCKET.              
*        IF ONLY DELETED BUYS EXIST, DON'T ALLOW SEND                           
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RBUYKEY,R4                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM                                                 
         DROP  RF                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         DROP  R4                                                               
         GOTO1 VHIGH                                                            
         B     SEND64                                                           
SEND62   OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
SEND64   CLC   KEY(22),KEYSAVE                                                  
         BNE   SEND65D             END OF BUYS                                  
         DROP  R6                                                               
SEND65   OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         TM    RBUYCNTL,X'C0'      VOID POINTER                                 
         BO    SEND62                                                           
         TM    RBUYCNTL,X'80'      DELETED                                      
         BZ    *+12                                                             
         CLI   RBUYCHGI,C'X'       DELETED (NOT CANCELLED)                      
         BE    SEND62                                                           
*                                                                               
         MVI   TEMP,X'FF'          WE HAVE A VALID BUY                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BNE   SEND62                                                           
         USING RBUYDTEL,R6                                                      
SEND65C  CLC   RBUYDTED,TEMP+1     BUY DATE VS. LATEST BUY DATE                 
         BNH   *+10                NOT HIGH -OK                                 
         MVC   TEMP+1(3),RBUYDTED  HIGHER -REPLACE                              
         BAS   RE,NEXTEL           ANOTHER 03 ELEM?                             
         BE    SEND65C                                                          
         B     SEND62              NEXT BUY                                     
         DROP  R6                                                               
                                                                                
SEND65D  DS    0H                  RESTORE SEND ELEM                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCONSEND,R6                                                      
         CLI   RCONSRV,1           VERSION 1?                                   
         BNE   SEND66              NO -SKIP THESE CHECKS                        
         DROP  R6                                                               
*                                                                               
         LA    R3,179              ERROR, NO BUYS                               
         CLI   TEMP,X'FF'          MUST HAVE BUYS IN ORDER TO SEND              
         BNE   ERROR                                                            
*                                                                               
         TM    PROFILES+CNTCKDTB,CNTCKDTA   K PROFILE #37?                      
         BZ    SEND66              NO - DONT NEED TO CHECK FLIGHT END           
*                                                                               
         CLC   =C'SEND',CONACT     SEND ACTION?                                 
         BNE   SEND66              NO -SKIP CHECK                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL            DARE K?                                      
         BE    SEND66              YES - SKIP CHECK                             
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWASNDFL,X'80'      WARNING ALREADY GIVEN?                       
         BO    SEND66              YES - BYPASS CHECK THIS TIME THRU            
         DROP  RF                                                               
*                                                                               
         OC    TEMP+1(3),TEMP+1    HAVE LATEST DATE?                            
         BZ    SEND66              NO -SKIP CHECK                               
*                                                                               
***>>>   GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,TEMP+16) FLIGHT END MM/DD          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
         MVC   TEMP+16(6),WORK+6   FLIGHT END YYMMDD                            
         GOTO1 GETDAY,DMCB,TEMP+16,DMCB+4                                       
         ZIC   R2,DMCB+0           FLIGHT END DAY                               
         GOTO1 DATCON,DMCB,(3,TEMP+1),(0,TEMP+4)   YY/MM/DD                     
         GOTO1 GETDAY,DMCB,TEMP+4,DMCB+4                                        
         ZIC   R1,DMCB+0           LATEST BUY DAY                               
         MVC   TEMP+10(6),TEMP+4   DEFAULT                                      
         CR    R2,R1               SAME DAY?                                    
         BE    SEND65E             YES                                          
         BH    *+8                 OUT OF WK ROTATOR?                           
         LA    R2,7(R2)            YES - ADD 1 WEEK                             
         SR    R2,R1                                                            
         GOTO1 ADDAY,DMCB,TEMP+4,TEMP+10,(R2) CORRECT END DATE                  
SEND65E  DS    0H                                                               
         CLC   TEMP+16(6),TEMP+10  FLIGHT END VS. CORRECT END                   
         BNH   SEND66              NOT HIGH - OK                                
*                                                                               
         LR    RF,RA               FLIGHT END BEYOND LAST SUNDAY                
         AHI   RF,TWAWORKQ         GIVE WARNING                                 
         USING TWAWORK,RF                                                       
         OI    TWASNDFL,X'80'      WARNING GIVEN                                
         DROP  RF                                                               
         LA    R3,809                                                           
         GOTO1 DATCON,DMCB,(0,TEMP+10),(11,TEMP+22)                             
         LA    RF,TEMP+22          BUFFER FOR DATE                              
         ST    RF,DMCB+12                                                       
         MVI   DMCB+12,8           INDICATE SIZE OF DATE                        
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         MVC   CONCACT(4),=C'SEND'                                              
         MVI   CONCACTH+5,4                                                     
         OI    CONCACTH+1,X'01'                                                 
         NI    CONCACTH+4,X'FF'-X'20'                                           
         OI    CONCACTH+6,X'40'+X'80'                                           
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
                                                                                
*                                                                               
SEND66   DS    0H                                                               
         LA    R2,CONCACTH                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   SEND68                                                           
         SPACE 1                                                                
         USING RCONBKEL,R6                                                      
SEND67   ICM   R5,15,RCONBKAM                                                   
         A     R5,FULL                                                          
         ST    R5,FULL                                                          
         DROP  R6                                                               
         SPACE 1                                                                
         BAS   RE,NEXTEL                                                        
         BE    SEND67                                                           
         SPACE 1                                                                
SEND68   LA    R3,187              ERROR, NO ORDER TOTAL                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         LA    R3,187              ERROR, ORDER TOTAL NOT = BUY TOTAL           
         USING RCONXEL,R6                                                       
         L     R5,FULL                                                          
         CLC   RCONTOT,FULL                                                     
         BNE   ERROR                                                            
         SPACE 1                                                                
*  ONLY ALLOW SEND IF CONTRACT ORDER COMMENT EXISTS                             
         SPACE 1                                                                
         LA    R3,128              ERROR, NO ORDER COMMENT                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         SPACE 1                                                                
SEND70   DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         LA    R3,281              SEND INVALID, IF RISK=6                      
         CLI   TWAARISK,6          PROHIBITS REP FROM SENDING ORDER             
         BE    ERROR                                                            
         DROP  RF                                                               
*                                                                               
         BAS   RE,GETTIME       TIME OF DAY FOR REPORTS & SEND ELEMENT          
                                                                                
*        BAS   RE,CKDARERC         CHECK IF DARE RECALLED                       
         GOTOR CKDARERC                                                         
                                                                                
         B     RS2                                                              
         DROP  R6                                                               
         EJECT                                                                  
*  DEAL WITH ACTION 'CF ' FOR GRAPHNET AND 'CFX' FOR ACE & GRAPHNET             
         SPACE 1                                                                
SEND100  DS    0H                  CHECK CONFIRMED NOW?                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         SPACE 1                                                                
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    SEND110                                                          
         LA    R3,63         ERROR, CONTRACT ALREADY CONFIRMED                  
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
SEND110  DS    0H                                                               
         CLC   =C'CFX',CONACT                                                   
         BNE   SEND120                                                          
*                                                                               
         GOTOR VALIDARE,DMCB,(RC)                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'10'                                                   
         BO    RS2                                                              
         LA    R3,167                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
SEND120  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    SEND125                                                          
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
SEND125  TM    RCONSENF,X'20'      LASTEST REP VERSION SENT                     
         BO    *+12                                                             
         LA    R3,168              LATEST VER NOT YET SENT                      
         B     ERROR                                                            
         DROP  R6                                                               
* SPECIAL CHECK FOR DARE CONTRACTS                                              
         GOTOR VALIDARE,DMCB,(RC)                                               
         GOTOR CFRMDARE,DMCB,(RC)                                               
         BAS   RE,GETTIME      TIME OF DAY FOR REPORTS & SEND ELEMENT           
         B     RS2                                                              
         EJECT                                                                  
*********************************************************************           
* FOR TAKEOVER CONTRACTS, ALLOW CONFIRMATION W/O SENDING TO STATION             
*********************************************************************           
TAKEOVER DS    0H                                                               
         LA    R3,75               INVALID CONTRACT ACTION                      
         XC    WORK2,WORK2                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         USING RCONCMEL,R6                                                      
         ZIC   R1,RCONCMLN                                                      
         SH    R1,=H'3'                                                         
         BM    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONCMNT                                                
         DROP  R6                                                               
                                                                                
         CLC   =C'C=TO',WORK2      COMMET MUST BE C=TOXXXX-X                    
         BNE   ERROR               WHERE XXXX-X IS THE STATION                  
         MVC   WORK(6),WORK2+4                                                  
                                                                                
         CLC   =C'-TV',CONSTA+4                                                 
         BE    TKOV20                                                           
         CLC   =C'-TV',CONSTA+3    CHECK INCASE OF 3 LETTER CALLS               
         BE    TKOV30                                                           
         CLC   =C'-L',CONSTA+4                                                  
         BE    TKOV20                                                           
         CLC   =C'-L',CONSTA+3     CHECK INCASE OF 3 LETTER CALLS               
         BE    TKOV30                                                           
         CLC   =C'-C',WORK2+7      CHECK IF COMBO                               
         BE    TKOV35                                                           
         CLC   =C'-C',WORK2+8                                                   
         BE    TKOV35                                                           
                                                                                
* COMPARE FOR RADIO                                                             
         CLI   WORK+5,C' '         CHECK INCASE OF 3 LETTER CALLS               
         BNE   TKOV10                                                           
         MVI   WORK+5,C'M'                                                      
                                                                                
TKOV10   DS    0H                                                               
         CLC   WORK(6),CONSTA                                                   
         BNE   ERROR                                                            
         B     TKOV40                                                           
                                                                                
* COMPARE FOR TV                                                                
TKOV20   DS    0H                  TV CAN BE SPECIFIED AS                       
         CLC   WORK(4),CONSTA      XXXX OR XXXX-T                               
         BNE   ERROR                                                            
         CLC   WORK+4(2),MYSPACES                                               
         BE    TKOV40                                                           
         CLC   =C'-T',WORK+4                                                    
         BE    TKOV40                                                           
         CLC   =C'-L',WORK+4                                                    
         BE    TKOV40                                                           
         B     ERROR                                                            
                                                                                
* AND 3 LETTER CALLS                                                            
TKOV30   DS    0H                                                               
         CLC   WORK(3),CONSTA                                                   
         BNE   ERROR                                                            
         CLC   WORK+3(3),MYSPACES                                               
         BE    TKOV40                                                           
         CLC   =C'-T',WORK+3                                                    
         BE    TKOV40                                                           
         CLC   =C'-L',WORK+3                                                    
         BE    TKOV40                                                           
         B     ERROR                                                            
                                                                                
* CHECK FOR COMBO TAKEOVER                                                      
TKOV35   DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
                                                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         CLI   WORK+3,C'-'         TAKEOVER COMMENT STATION MUST MATCH          
         BE    TKOV38              PARENT STATION                               
         CLC   RSTACS(4),WORK                                                   
         BNE   ERROR                                                            
                                                                                
TKOV38   DS    0H                  INCASE PARENT'S CALL IS 3 LETTERS            
         CLC   RSTACS(3),WORK                                                   
         BNE   ERROR                                                            
         DROP  R6                                                               
                                                                                
TKOV40   DS    0H                                                               
         LA    R6,RCONREC                                                       
         USING RCONXEL,R6                                                       
         MVI   ELCODE,X'1F'        CHECK IF CONFIRMED BEFORE                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONCONF,X'80'      MUST NEVER BE CONFIRMED BEFORE               
         BZ    ERROR                                                            
                                                                                
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         OC    RCONSSID,RCONSSID   MUST NEVER BE SENT BEFORE                    
         BNZ   ERROR                                                            
         CLI   RCONSRV,1           AND REP VERSION MUST BE 1                    
         BNE   ERROR                                                            
* MIMICK ACTION CONFIRM                                                         
* SET MOD NUMBER TO 0                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         MVI   RCONMOD,0                                                        
         MVC   RCONMODD,TODAY                                                   
*                                                                               
* REPLACE VERSION NUMBER WITH MOD NUMBER ON SCREEN                              
*                                                                               
         MVC   CONMOD,MYSPACES                                                  
                                                                                
         FOUT  CONMODH                                                          
*                                                                               
*  MARK CONTRACT CONFIRMED                                                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         NI    RCONCONF,X'7F'      TURN OFF NOT CONFIRMED                       
         OI    RCONCONF,X'40'      TURN ON CONFIRMED NOW                        
         NI    RCONSTAT,X'FF'-X'01' TURN OFF BATCH CONFIRM FLAG                 
         SPACE 1                                                                
*  MARK CONTRACT WITH DATE/TIME/SENT FLAG                                       
         SPACE 1                                                                
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),X'20'         NEXT ELEMENT S/B SEND ELEMENT                
         BE    TKOV50                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONSEND,R6                                                      
TKOV50   DS    0H                                                               
         MVC   RCONSSID,TWAUSRID   REP SENDING ID                               
         OI    RCONSENF,X'10'      STA VERS. NOT ADVANCED                       
         OI    RCONSENF,X'20'      REP VERS. NOT ADVANCED                       
         NI    RCONSENF,X'7F'      TURN OFF SENT BY REP                         
         OI    RCONSENF,X'40'      TURN ON SENT BY STATION                      
         OI    RCONSENF,X'02'      TURN ON CONF BY STATION                      
         OI    RCONSENF,X'01'      TURN ON TAKEOVER BY REP                      
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSSDT) COMPRESSED DATE                   
                                                                                
         BAS   RE,GETTIME                                                       
                                                                                
         UNPK  DUB,SENDTIME                                                     
         MVC   RCONSSTI,DUB+1      TIME                                         
         SPACE 1                                                                
*  PUT CONTRACT                                                                 
         SPACE 1                                                                
         MVI   UPDATE,C'Y'                                                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28,TWAKADDR                                                  
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,AIO4                                                
                                                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         BAS   RE,PASSIV8F                                                      
*                                                                               
         BRAS  RE,AUDTXT           PUT OUT AUDIT TEXT RECORD                    
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
RS2      DS    0H                   THIS WAS FORMERLY REP SEND 2                
         CLC   =C'LAST',CONACT                                                  
         BE    EXXMOD              'LAST' DIDN'T DO REP SEND 2                  
         CLC   =C'RSND',CONACT     FOR RESEND, CHECK IF EC/UPDATE X'9F'         
         BE    SND70                BEFORE EXITING                              
         CLC   =C'MGS',CONACT      FOR MGS, UPDATE MAKEGOOD SEND ELEMT          
         BE    SND100               BEFORE EXITING                              
                                                                                
*          DATA SET RECNT6B    AT LEVEL 029 AS OF 07/23/90                      
* DELETE BUY COMMENT ELEMENTS THAT DON'T BELONG TO SENDER                       
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         NI    TWASNDFL,X'FF'-X'80' TURN OFF WARNING GIVEN FLAG                 
         DROP  RF                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RBUYKEY,R4                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM                                                 
         DROP  R4,RF                                                            
         SPACE 1                                                                
         GOTO1 VHIGH                                                            
         B     READ15                                                           
         SPACE 1                                                                
READ10   GOTO1 VSEQ                                                             
         SPACE 1                                                                
READ15   CLC   KEY(22),KEYSAVE                                                  
         BNE   READ50                                                           
         LA    RE,RBUYREC          CLEAR I/O AREA                               
         LA    RF,1000             EACH TIME THROUGH                            
         XCEF                                                                   
*                                                                               
         L     R6,AIO2             CLEAR I/O AREA                               
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R6),(RF)          EACH TIME THROUGH                            
*                                                                               
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         GOTOR TRAPMODS            CONTROL MOD CODE ELEMENTS                    
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'84'                                                     
         BAS   RE,GETEL                                                         
         BNE   READ32              ALL BUYS SUBJECT TO REWRITE                  
*                                     FOR MOD CODE ELEMENT                      
*                                                                               
         CLC   CONACT,=C'CF  '     CF / CFX?                                    
         BE    READ30              YES - DELETE MOD CODE ELTS                   
         CLC   =C'CFX',CONACT            AND COMMENT ELTS                       
         BE    READ30                                                           
         SPACE 1                                                                
         CLI   TWAACCS,C'$'        STATION                                      
         BE    READ20                                                           
         TM    2(R6),X'80'         REP COMMENT                                  
         BO    READ32              REWRITE BUY FOR MOD ELEMENT                  
         B     READ30                                                           
         SPACE 1                                                                
READ20   TM    2(R6),X'80'         REP COMMENT                                  
         BZ    READ10                                                           
READ30   EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)    DELETE COMMENT                  
*                                                                               
* FOR COMBO, CHECK IF NO UPDATE FLAG SET.  FOR CHECKING TOTALS ONLY             
*                                                                               
READ32   EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    READ35                                                           
         TM    TWASPREF,SNDNOUPD                                                
         BO    READ40                                                           
         DROP  RF                                                               
*                                                                               
READ35   GOTO1 VPUTREC,DMCB,RBUYREC             PUT CHANGED RECORD              
*                                                                               
READ40   DS    0H                                                               
         B     READ10              GET NEXT BUY                                 
         SPACE 2                                                                
READ50   CLC   CONACT,=C'SEND'     ACE OR GRAPHNET                              
         BE    SND5                                                             
         SPACE 1                                                                
         CLC   CONACT,=C'CF  '                                                  
         BE    CNF10                                                            
         CLC   =C'CFX',CONACT                                                   
         BE    CNF10                                                            
         SPACE 2                                                                
SND5     DC    0H'0'                                                            
         SPACE 1                                                                
*  MARK CONTRACT WITH DATE/TIME/SENT FLAG/(ID)                                  
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    SND10                                                            
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
SND10    MVC   SVSENF(1),RCONSENF  SAVE SAR/SPL UDATE INDICATORS                
         NI    RCONSENF,X'F3'      AND TURN THEM OFF IN CONTRACT                
         CLI   TWAACCS,C'$'        STATION                                      
         BE    SND30                                                            
         SPACE 1                                                                
         CLI   RCONKSTA+4,C'F'     FM RADIO STATION?                            
         BE    SND10005            YES                                          
         CLI   RCONKSTA+4,C'A'     AM RADIO STATION?                            
         BNE   SND10090            NO  - NOT RADIO: SKIP                        
SND10005 EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAGENFG,TWQMASTR   MASTER PROCESS FROM DARE?                    
         DROP  RF                                                               
         BNO   SND10090            NO                                           
*                                                                               
         LA    R5,PIDTABLE         SET A(PRINTER ID TABLE)                      
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         CLI   FASYS,X'38'         TEST SYSTEM?                                 
         BNE   SND10010            NO  - USE PIDTABLE                           
         LA    R5,PIDTABL2         YES - USE PIDTABL2                           
         DROP  RF                                                               
SND10010 EQU   *                                                                
         CLI   0(R5),0             END OF TABLE                                 
         BE    SND10080            NOT IN TABLE: USE GETREPID                   
         CLC   TWAUSRID,DSIGNON(R5)                                             
*                                  SIGNON TWAUSRID IN TABLE?                    
         BNE   SND10020            NO                                           
         CLC   RCONKREP,DPOWERCD(R5)                                            
*                                  YES - FOR THIS REP OF ORDER?                 
         BNE   SND10020            NO                                           
         MVC   RCONSSID,DSSID(R5)  YES - INSERT REP SENDING ID                  
         B     SND10100                                                         
SND10020 EQU   *                                                                
         LA    R5,LPIDTABL(R5)     BUMP TO NEXT TABLE ENTRY                     
         B     SND10010            GO BACK FOR NEXT                             
*                                                                               
SND10080 EQU   *                                                                
         GOTOR MASTERID            MISSING. WE NEED TO GET THE REP ID           
         MVC   RCONSSID,WORK       INSERT REP SENDING ID                        
         B     SND10100                                                         
SND10090 EQU   *                                                                
         MVC   RCONSSID,TWAUSRID   REP SENDING ID                               
SND10100 EQU   *                                                                
         OI    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         NI    RCONSENF,X'BF'      TURN OFF SENT BY STATION                     
         OI    RCONSENF,X'80'      SENT BY REP                                  
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSRDT)     COMPRESSED DATE               
         UNPK  DUB,SENDTIME                                                     
         MVC   RCONSRTI,DUB+1      TIME                                         
*                                                                               
         CLI   RCONSNLN,RCONSN3Q   ELEM HOLDS 1ST SEND INFO?                    
         BL    SND20               NO                                           
         OC    RCONSRVF(3),RCONSRVF 1ST SEND ALREADY SET?                       
         BNZ   SND20               YES                                          
         MVC   RCONSRVF,RCONSRV    1ST VERSION SENT                             
         MVC   RCONSRDF,RCONSRDT   1ST DATE SENT                                
*                                                                               
SND20    DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'92',RCONREC)   DELETE STA ORD CMT               
         B     SND70               WHEN REP IS SENDING                          
         SPACE 1                                                                
SND30    OI    RCONSENF,X'10'      STATION VERSION NOT ADVANCED                 
         NI    RCONSENF,X'7F'      TURN OFF SENT BY REP                         
         OI    RCONSENF,X'40'      SENT BY STATION                              
         NI    RCONSENF,X'FF'-X'02' TURN OFF CONF BY STATION                    
         NI    RCONSENF,X'FF'-X'01' TURN OFF TAKEOVER FLAG, IF ON               
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSSDT)    COMPRESSED DATE                
         UNPK  DUB,SENDTIME                                                     
         MVC   RCONSSTI,DUB+1                                                   
*                                                                               
         CLI   RCONSNLN,RCONSN3Q   ELEM HOLDS 1ST SEND INFO?                    
         BL    SND40               NO                                           
         OC    RCONSSVF(3),RCONSSVF 1ST SEND ALREADY SET?                       
         BNZ   SND40               YES                                          
         MVC   RCONSSVF,RCONSSV    1ST VERSION SENT                             
         MVC   RCONSSDF,RCONSSDT   1ST DATE SENT                                
*                                                                               
SND40    DS    0H                                                               
         SPACE 1                                                                
         GOTO1 VDELELEM,DMCB,(X'82',RCONREC)   DELETE REP CON ORD CMT           
*                                  WHEN STATION IS SENDING                      
                                                                                
SND70    DS    0H                                                               
         CLC   =C'RSND',CONACT     FOR RESEND, EXIT IF NOT EOP USER             
         BNE   SND73                                                            
                                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLI   TWATRFMT,C'B'                                                    
         BE    SND73                                                            
         CLI   TWATRFMT,C'W'                                                    
         BE    SND73                                                            
         CLI   TWATRFMT,C'C'                                                    
         BE    SND73                                                            
         CLI   TWATRFMT,C'K'                                                    
         BE    SND73                                                            
         CLI   TWATRFMT,C'J'                                                    
         BNE   EXXMOD                                                           
         DROP  RF                                                               
                                                                                
SND73    DS    0H                  IF ELECTRONIC CONTRACT BIAS/JDS              
         GOTOR ECCHECK             UPDATE X'9F' AGY/ADV CODES                   
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        DARE ORDER?                                  
         BAS   RE,GETEL                                                         
         BNE   SND74                                                            
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q   NEW ELEMENT LENGTH IN USE??                  
         BL    SND74                                                            
         NI    RCONDRF2,X'FF'-X'04' RESET MANUAL CHANGES ALL THE TIME           
         DROP  R6                                                               
*                                                                               
SND74    DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
* FOR COMBO, CHECK IF NO UPDATE FLAG SET.  FOR CHECKING TOTALS ONLY             
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    SND75                                                            
         TM    TWASPREF,SNDNOUPD                                                
         BO    SND80                                                            
         DROP  RF                                                               
*                                                                               
SND75    DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         BAS   RE,PASSIV8F                                                      
*                                                                               
         BRAS  RE,AUDTXT           PUT OUT AUDIT TEXT RECORD                    
*                                                                               
         GOTOR FLAGDARE                                                         
*                                                                               
SND80    DS    0H                                                               
         CLC   =C'SEND',CONACT                                                  
         BNE   SNDX                                                             
*&&DO                                                                           
         CLI   RCONKSTA+4,C'F'     FM RADIO STATION?                            
         BE    SND85               YES                                          
         CLI   RCONKSTA+4,C'A'     AM RADIO STATION?                            
         BE    SND85               NO  - NOT RADIO: SKIP                        
*&&                                                                             
*                                                                               
********                                                                        
* TV CODE                                                                       
********                                                                        
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BO    SNDX                                                             
         MVC   CONMOD(3),MYSPACES  NO LONGER WIP ANYMORE                        
         OI    CONMODH+6,X'80'     XMIT                                         
         B     SNDX                                                             
*                                                                               
********                                                                        
* RADIO CODE                                                                    
********                                                                        
SND85    DS    0H                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BZ    SND90                                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   SNDX                                                             
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      LINKED TO EDI ORDER?                         
         BZ    SNDX                                                             
         DROP  R6                                                               
*                                                                               
SND90    DS    0H                                                               
         MVC   CONMOD(3),MYSPACES  NO LONGER WIP ANYMORE                        
         OI    CONMODH+6,X'80'     XMIT                                         
         B     SNDX                                                             
         EJECT                                                                  
*                                                                               
* UPDATE MAKEGOOD OFFER SEND ELEMENT                                            
*                                                                               
SND100   DS    0H                                                               
         LA    R6,RCONREC          UPDATE OR ADD MAKEGOOD OFFER ELMT            
         USING RCONMGEL,R6                                                      
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
                                                                                
         XC    WORK2,WORK2                                                      
         ZIC   R1,RCONMGLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONMGEL                                                
         GOTO1 VDELELEM,DMCB,(X'21',RCONREC)                                    
         DROP  R6                                                               
                                                                                
         LA    R6,WORK2                                                         
         USING RCONMGEL,R6                                                      
         CLI   RCONMGLN,RCONXMGQ                                                
         BE    SND105                                                           
*                                                                               
         MVI   RCONMGLN,RCONXMGQ   UPDATE TO NEW LENGTH                         
         TM    RCONMGFG,X'20'      LAST SENT BY STATION?                        
         BO    SND105                                                           
         MVC   RCONRMDT(5),RCONMGDT   UPDATE REP LAST SENT DATE/TIME            
         XC    RCONMGDT(5),RCONMGDT                                             
                                                                                
* RESET MAKEGOOD OFFER WIP FLAG, ALWAYS                                         
SND105   DS    0H                                                               
         NI    RCONMGFG,X'FF'-X'80'-X'40'-X'20'                                 
         CLI   TWAACCS,C'$'                                                     
         BNE   SND115                                                           
                                                                                
* SAVE DATE/TIME FOR STATION                                                    
SND110   DS    0H                                                               
         OI    RCONMGFG,X'20'      FLAG SENT BY STATION                         
         GOTO1 DATCON,DMCB,(5,0),(2,RCONMGDT)                                   
         MVC   RCONMGTM,SENDTIME                                                
         B     SND120                                                           
                                                                                
* SAVE DATE/TIME FOR REP                                                        
SND115   DS    0H                                                               
         OI    RCONMGFG,X'40'      FLAG SENT BY REP                             
         GOTO1 DATCON,DMCB,(5,0),(2,RCONRMDT)                                   
         MVC   RCONRMTM,SENDTIME                                                
                                                                                
*  ADD MG X'21' SEND ELEMENT                                                    
SND120   DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
                                                                                
*  PUT CONTRACT                                                                 
         MVI   UPDATE,C'Y'                                                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEY+28,TWAKADDR                                                  
         DROP  RF                                                               
                                                                                
         GOTO1 VGETREC,DMCB,AIO4                                                
                                                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         BAS   RE,PASSIV8F                                                      
*                                                                               
*******  BRAS  RE,AUDTXT           PUT OUT AUDIT TEXT RECORD                    
*                                                                               
SNDX     DS    0H                                                               
         BAS   RE,RESETMGO                                                      
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* BEFORE EXITING, RESET ALL MAKEGOOD OFFERS SO ANYONE CAN UPDATE                
* FOR ACTION MGS                                                                
*                                                                               
RESETMGO NTR1                                                                   
KEYD     USING RMKGKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   KEYD.RMKGKTYP,X'11'                                              
         MVC   KEYD.RMKGKREP,TWAAGY     INSERT POWER CODE                       
         MVC   KEYD.RMKGKOFF,RCONKOFF   INSERT OFFICE CODE                      
         MVC   KEYD.RMKGKSTA,RCONKSTA   INSERT STATION CALL LETTERS             
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEYD.RMKGKCON,TWACNUM                                            
         DROP  RF                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         B     RSMGO20                                                          
*                                                                               
RSMGO10  DS    0H                                                               
         GOTO1 VSEQ                                                             
*                                                                               
RSMGO20  DS    0H                  MAKGOOD OFFER FOR THIS CONTRACT??            
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   RSMGOX                                                           
*                                                                               
         OC    KEYD.RMKGKPLN(6),KEYD.RMKGKPLN                                   
         BNZ   RSMGO10             WANT HEADER RECORD ONLY                      
         DROP  KEYD                                                             
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA           RESET WORK-IN-PROGRESS FLAG                  
         USING RMKGREC,R6                                                       
*                                                                               
         MVC   SAVWIP,RMKGSFG2     SAVE WIP FLAG                                
*                                                                               
         NI    RMKGSFG2,X'FF'-RMGF2WPQ                                          
*                                                                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                                                               
         GOTOR PASSREWR            UPDATE MAKEGOOD PASSIVES                     
*                                                                               
*****    BRAS  RE,AUDTXT           PUT OUT AUDIT TEXT RECORD                    
*                                                                               
         B     RSMGO10                                                          
*                                                                               
RSMGOX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* ACTION CONFIRM                                                                
* INCREMENT MOD NUMBER                                                          
*                                                                               
CNF10    DC    0H'0'                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CNF15                                                            
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'20'           CONFIRMED PREVIOUSLY?                   
         BO    CNF15                                                            
         DROP  R6                                                               
*                                                                               
* DATE STAMP EPL/SPL ELEM ON ORIGINAL CONFIRM IF CONTYPE OPTION SET             
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAPRFK,X'20'            CONTYPE K OPTION #3?                    
         BZ    CNF11                                                            
         DROP  RF                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'             EPL/SPL ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   CNF10A                                                           
         USING RCONSPEL,R6                                                      
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSPYR)   TODAY'S DATE                    
         OI    RCONSPES,X'08'           FLAG NEW DATE STYLE                     
         B     CNF11                                                            
CNF10A   DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         MVI   RCONSPCO,X'06'                                                   
         MVI   RCONSPLN,X'12'                                                   
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSPYR)   TODAY'S DATE                    
         MVI   RCONSPES,X'0C'                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RCONSPDT)   TODAY'S DATE                    
         MVI   RCONSPNU,1                                                       
         MVC   RCONSPST,RCONKSTA                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BE    CNF11                                                            
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RCONACEL,R6                                                      
         MVI   RCONACCO,X'08'                                                   
         MVI   RCONACLN,X'0C'                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RCONACTA)   TODAY'S DATE                    
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         DROP  R6                                                               
*                                                                               
CNF11    DS    0H                                                               
         CLI   RCONKSTA+4,C' '                                                  
         BE    CNF11A                                                           
         CLI   RCONKSTA+4,C'L'                                                  
         BNE   CNF15                                                            
CNF11A   DS    0H                                                               
         TM    PROFILES+CNTSETBB,CNTSETBA  CHECK PROFILE #27                    
         BZ    CNF15                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
CNF12    BNE   CNF15                                                            
         USING RCONBKEL,R6                                                      
         MVC   RCONBKWK,MONDATE         SET WEEK'S MONDAY DATE                  
         TM    TWAFLAGS,X'08'           DAILY PACING USER?                      
         BNO   CNF14                    NO                                      
*                                       YES - RESET ACTIVITY DATE TO            
         GOTO1 DATCON,DMCB,(5,0),(2,RCONBKWK)   TODAY'S DATE                    
*                                                                               
CNF14    EQU   *                                                                
         DROP  R6                                                               
         BAS   RE,NEXTEL                                                        
         B     CNF12                                                            
*                                                                               
CNF15    DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAPRFK,X'20'            CONTYPE K OPTION #3?                    
         BZ    CNF18                                                            
         DROP  RF                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'             SPL/EPL ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   CNF18                    NO ELEM? - SKIP                         
         USING RCONSPEL,R6                                                      
         CLI   RCONSPNU,0               IF NO STATIONS, SKIP THIS PART          
         BE    CNF17                                                            
         ZIC   R1,RCONSPNU              # OF STATIONS                           
         LA    RE,RCONSPAM              FIRST MINI ELEM AMOUNT                  
CNF16    XC    0(4,RE),0(RE)            CLEAR MINI ELEM AMOUNT                  
         LA    RE,9(RE)                 NEXT MINI ELEM AMOUNT                   
         BCT   R1,CNF16                                                         
CNF17    NI    RCONSPES,X'FF'-X'40'     CLEAR SOME FLAGS                        
         NI    RCONSPES,X'FF'-X'80'                                             
         TM    RCONSPES,X'20'           OVERIDE VALUE IN 08 ELEM?               
         BZ    CNF18                    NO - SKIP                               
         NI    RCONSPES,X'FF'-X'20'     YES - CLEAR FLAG                        
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'             GET 08 ELEM                             
         BAS   RE,GETEL                                                         
         BNE   CNF18                                                            
         USING RCONACEL,R6                                                      
         XC    RCONAC$$,RCONAC$$        ZERO OVERIDE AMOUNT                     
         DROP  R6                                                               
*                                                                               
CNF18    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         ZIC   R1,RCONMOD                                                       
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
         MVC   RCONMODD,TODAY                                                   
         SPACE 1                                                                
* REPLACE VERSION NUMBER WITH MOD NUMBER ON SCREEN                              
         SPACE 1                                                                
         MVC   CONMOD,MYSPACES                                                  
         CLI   RCONMOD,0           IF MOD 0, SHOW NOTHING                       
         BE    CNF20                                                            
         MVC   CONMOD(7),=C'MOD NUM'                                            
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD                                                
         CLI   HALF+1,250                                                       
         BL    *+8                                                              
         MVI   HALF,255                                                         
         EDIT  (2,HALF),(3,CONMOD+8),ALIGN=LEFT                                 
CNF20    FOUT  CONMODH                                                          
         SPACE 1                                                                
*  MARK CONTRACT CONFIRMED                                                      
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         NI    RCONCONF,X'7F'      TURN OFF NOT CONFIRMED                       
         OI    RCONCONF,X'40'      TURN ON CONFIRMED NOW                        
         NI    RCONSTAT,X'FF'-X'01'  TURN OFF BATCH CONFIRM FLAG                
*                                                                               
         CLC   =C'CFX',CONACT                                                   
         BNE   CNF30                                                            
*                                                                               
         MVC   RCONCFX#,RCONMOD    FOR 'CFX' ACTION, SAVE MOD#                  
         CLI   RCONMOD,0                                                        
         BNE   *+8                                                              
         MVI   RCONCFX#,X'FF'      IF MOD# = 0, USE X'FF'                       
         DROP  R6                                                               
*                                                                               
         LR    RF,RA               CHECK STAD ADD OPT 2                         
         AHI   RF,TWAWORKQ         SKIP CREATION OF X'22' ELM                   
         USING TWAWORK,RF          AND SKIP SETTING X'20' ELM                   
         TM    TWASTADF,X'40'                                                   
         BNZ   CNF45                                                            
         DROP  RF                                                               
*                                                                               
*  UPDATE X'22' MOD DATE/TIME ELEMENT, CREATE IF NECESSARY                      
CNF30    LA    R6,RCONREC                                                       
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BE    CNF35                IF IT DOESN'T EXIST, MAKE IT                
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         MVI   0(R6),X'22'                                                      
         MVI   1(R6),RMODELLQ                                                   
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         B     CNF30                                                            
CNF35    LR    R5,R6                                                            
         USING RMODELEM,R5                                                      
         MVC   WORK2(RMODEL3M-RMODEL1M),RMODEL1M                                
         MVC   RMODEL2M(RMODEL3M-RMODEL1M),WORK2                                
         MVC   RMODEL1M,RCONMOD                                                 
         GOTO1 DATCON,DMCB,(5,0),(2,RMODEL1D)                                   
         BAS   RE,GETTIME                                                       
         UNPK  DUB,SENDTIME                                                     
         MVC   RMODEL1T,DUB+1      TIME                                         
*                                                                               
*  MARK CONTRACT WITH DATE/TIME/SENT FLAG                                       
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                   PUT HIGHER OF REP OR STA VERSION#           
         MVC   RMODEL1V,RCONSRV     IN X'22' ELEMENT                            
         CLC   RCONSSV,RCONSRV                                                  
         BNH   *+10                                                             
         MVC   RMODEL1V,RCONSRV                                                 
         DROP  R5                                                               
*                                                                               
         CLC   =C'CFX',CONACT      CFX ACTION?                                  
         BNE   CNF40               NO                                           
*                                                                               
         LR    RF,RA               CHECK STAD ADD OPT 2                         
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWASTADF,X'40'                                                   
         BNZ   CNF45                                                            
         DROP  RF                                                               
*                                                                               
         OI    RCONSENF,X'10'      STA VERS. NOT ADVANCED                       
         NI    RCONSENF,X'FF'-X'40' TURN OFF SENT BY STA                        
         OI    RCONSENF,X'80'      TURN ON SENT BY REP                          
         OI    RCONSENF,X'02'      TURN ON CONF BY STATION                      
         NI    RCONSENF,X'FF'-X'01' TURN OFF TAKEOVER FLAG, IF ON               
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSRDT) COMPRESSED DATE                   
         UNPK  DUB,SENDTIME                                                     
         MVC   RCONSRTI,DUB+1      TIME                                         
         B     CNF45                                                            
*                                                                               
CNF40    DS    0H                                                               
         OI    RCONSENF,X'10'      STA VERS. NOT ADVANCED                       
         NI    RCONSENF,X'7F'      TURN OFF SENT BY REP                         
         OI    RCONSENF,X'40'      TURN ON SENT BY STATION                      
         OI    RCONSENF,X'02'      TURN ON CONF BY STATION                      
         NI    RCONSENF,X'FF'-X'01' TURN OFF TAKEOVER FLAG, IF ON               
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSSDT) COMPRESSED DATE                   
         UNPK  DUB,SENDTIME                                                     
         MVC   RCONSSTI,DUB+1      TIME                                         
*                                                                               
*  DELETE ORDER COMMENTS                                                        
CNF45    DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'82',RCONREC)   REP COMMENT                      
         GOTO1 VDELELEM,DMCB,(X'92',RCONREC)   STATION COMMENT                  
*                                                                               
* FOR EDI/DARE ORDERS, UPDATE EDI ELEMENT FOR MOD NUMBERS                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        FOR SPECIAL EDI/DARE ORDERS                  
         BAS   RE,GETEL            ADD A PASSIVE KEY X'E1'                      
         BNE   CNF27                                                            
         USING RCONDREL,R6                                                      
         CLI   RCONDRFG,X'02'                                                   
         BE    CNF25B              SPECIAL EDI CONVERTED ORDER                  
         TM    RCONDRFG,X'80'+X'40'+X'04'                                       
         BNO   CNF27               MUST BE LINKED, APPROVED AND EDI             
         DROP  R6                                                               
*                                                                               
CNF25B   DS    0H                                                               
         LA    R6,RCONREC          IF ELEMENT EXISTS, UPDATE THIS               
         MVI   ELCODE,X'ED'        ELEMENT ONLY ONCE PER DAY                    
         BAS   RE,GETEL                                                         
         BNE   CNF26                                                            
         USING RCONEDEL,R6                                                      
         CLC   RCONEDDT,TODAY                                                   
         BE    CNF27                                                            
         MVC   RCONEDDT,TODAY                                                   
         ZIC   RF,RCONEDMD                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RCONEDMD                                                      
         B     CNF27                                                            
         DROP  R6                                                               
*                                                                               
CNF26    DS    0H                  ELEMENT NOT FOUND, ADD ONE                   
         LA    R6,WORK2                                                         
         USING RCONEDEL,R6                                                      
         XC    WORK2(80),WORK2                                                  
         MVI   RCONEDCD,X'ED'                                                   
         MVI   RCONEDLN,6                                                       
         MVI   RCONEDMD,1          EDI/KATZ ORDER STARTS AT MOD 1               
         MVC   RCONEDDT,TODAY                                                   
*                                                                               
         CLC   =C'1224',RCONKAGY   EXCEPT FOR LEO,                              
         BE    CNF26A               WHICH IS TO START AT MOD 0                  
         CLC   =C'LEOB',RCONKAGY                                                
         BNE   CNF26B                                                           
*                                                                               
CNF26A   DS    0H                                                               
         MVI   RCONEDMD,0                                                       
*                                                                               
CNF26B   DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         DROP  R6                                                               
*                                                                               
*  PUT CONTRACT                                                                 
*                                                                               
CNF27    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        DARE ORDER?                                  
         BAS   RE,GETEL                                                         
         BNE   CNF28                                                            
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q   NEW ELEMENT LENGTH IN USE??                  
         BL    CNF28                                                            
*                                                                               
         NI    RCONDRF2,X'FF'-X'04' RESET MANUAL CHANGES ALL THE TIME           
*                                                                               
         TM    RCONDRF2,X'80'      VARIOUS ORDER?                               
         BZ    CNF28                                                            
         OI    RCONDRF2,X'20'      MARK VARIOUS CONFIRMED                       
         DROP  R6                                                               
*                                                                               
*  PUT CONTRACT                                                                 
*                                                                               
CNF28    DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28,TWAKADDR                                                  
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
* FOR COMBO, CHECK IF NO UPDATE FLAG SET.  FOR CHECKING TOTALS ONLY             
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    CNF60                                                            
         TM    TWASPREF,SNDNOUPD                                                
         BO    CNF70                                                            
         DROP  RF                                                               
*                                                                               
CNF60    DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         BAS   RE,PASSIV8F                                                      
*                                                                               
         BRAS  RE,AUDTXT           PUT OUT AUDIT TEXT RECORD                    
*                                                                               
*                                                                               
CNF70    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        FOR SPECIAL EDI/DARE ORDERS                  
         BAS   RE,GETEL            ADD A PASSIVE KEY X'E1'                      
         BNE   CNFX                                                             
         USING RCONDREL,R6                                                      
         CLI   RCONDRFG,X'02'                                                   
         BE    CNF80               SPECIAL FOR EDI CONVERTED ORDERS             
         TM    RCONDRFG,X'80'+X'40'+X'04'                                       
         BNO   CNFX                MUST BE LINKED, APPROVED AND EDI             
         DROP  R6                                                               
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS CONFIRM, ADD A PASSIVE KEY          
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
CNF80    DS    0H                                                               
         CLC   =C'1342',RCONKAGY   SKIP FOR DMBB                                
         BE    CNFX                                                             
         CLC   =C'2905',RCONKAGY   SKIP FOR DMBB                                
         BE    CNFX                                                             
         CLC   =C'DMBB',RCONKAGY   SKIP FOR DMBB                                
         BE    CNFX                                                             
         XC    IOAREA,IOAREA                                                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REDIKEY,R6                                                       
         MVI   REDIKTYP,REDIKTYQ                                                
         MVC   REDIKREP,REPALPHA                                                
         MVI   REDIKACT,C'C'       ACTION IF CONFIRM                            
*                                                                               
         MVC   REDIKCON,RCONKCON   CONTRACT NUMBER                              
*                                  DATE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,REDIKDTE)                                
*                                  FETCH TODAY'S DATE                           
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,REDIKTIM       TIME IN HHMMSS                               
         DROP  R6                                                               
*                                                                               
         XC    IOAREA(256),IOAREA                                               
         MVC   IOAREA(27),KEY                                                   
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  CONTRACT RECORD ADDRESS                      
         DROP  RF                                                               
*                                                                               
         GOTO1 VADD                ADD THE KEY                                  
*                                                                               
         MVI   REDBKTYP,REDBKTYQ   ADD X'0E' RECORD AS A BACKUP                 
         MVI   REDBLEN+1,34+REDBELLQ                                            
         MVI   REDBCODE,1          TO THE X'E1' KEYS                            
         MVI   REDBELLN,REDBELLQ                                                
         MVC   REDBMODN,RCONMOD                                                 
*                                                                               
         GOTO1 VADDREC,DMCB,IOAREA                                              
*                                                                               
CNFX     DS    0H                                                               
         BAS   RE,GETTIME       TIME OF DAY FOR REPORTS & SEND ELEMENT          
         B     EXXMOD                                                           
         EJECT                                                                  
GETTIME  DC    0H'0'                                                            
         ST    RE,FULL             SAVE RE                                      
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         DROP  R7                                                               
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         AP    FATIME,=P'60000'                                                 
         MVC   SENDTIME,FATIME   SAVE TIME FOR REPORTS & SEND ELEMENT           
         DROP  RF                                                               
         L     RF,SENDTIME                                                      
         SLL   RF,4                                                             
         ST    RF,SENDTIME                                                      
         L     RE,FULL             RESTORE RE                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*  UPDATE 8F PASSIVE KEY - 8F KEY FOR INCOMPLETE CONTRACTS ONLY                 
*                                                                               
PASSIV8F NTR1                                                                   
***********************************************************                     
* GET CONTRACT RECORD                                                           
         MVI   UPDATE,C'Y'                                                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28,TWAKADDR                                                  
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
                                                                                
************************************************************                    
*                                                                               
* PUT CONTRACT STATUS IN BYTE / PUT REP VERSION NUMBER IN DUB                   
*                                                                               
         MVI   DUB,0               CLEAR ROUTINE WORK AREAS                     
         MVI   BYTE,0                                                           
         XC    WORK(20),WORK                                                    
*                                                                               
*******************************************************************             
* - IF THERE ARE MKGD OFFERS - WHERE THEY LAST SENT BY REP OR STATION           
*   SET BYTE=X'02' SENT BY REP    BYTE=X'03' SENT BY STATION                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'        MAKEGOODS?                                   
         BAS   RE,GETEL                                                         
         BNE   PASF20                                                           
         USING RCONMGEL,R6                                                      
         TM    RCONMGCT,X'80'      ..ARE THERE MAKEGOOD OFFERS?                 
         BZ    PASF20                                                           
         TM    RCONMGFG,X'40'      ..AND LAST SENT BY REP?                      
         BNO   PASF10                                                           
         MVC   WORK+10(2),RCONMGDT     ,,DO OLD ELEMENT DATA                    
         MVC   WORK+12(3),RCONMGTM                                              
         CLI   RCONMGLN,RCONMGLQ       ,,UNLESS WE HAVE A NEW ELEMENT           
         BE    *+16                    NO                                       
         MVC   WORK+10(2),RCONRMDT     YES,NEW ELEM                             
         MVC   WORK+12(3),RCONRMTM                                              
         MVI   BYTE,X'02'          .. YES - X'02' = MAKEGOOD OFFERS             
         B     PASF20                                                           
*                                                                               
PASF10   DS    0H                                                               
**       TM    RCONMGFG,X'20'         LAST SENT BY STATION?                     
**       BNO   PASF20                                                           
**       MVC   WORK+10(2),RCONMGDT                                              
**       MVC   WORK+12(3),RCONMGTM                                              
**       MVI   BYTE,X'03'             STATION SEND                              
*                                                                               
         EJECT                                                                  
                                                                                
***************************************************************                 
         DROP  R6                                                               
PASF20   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'        EXTENDED DESCRIPTION ELEM                    
         BAS   RE,GETEL                                                         
         BNE   PASF30                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONTRACT CONFIRMED NOW?                      
         BO    PASF40              YES                                          
                                                                                
*****************************************************************               
         DROP  R6                                                               
PASF30   LA    R6,RCONREC          NO                                           
         MVI   ELCODE,X'20'        SENT ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BNO   PASF40                           NO                              
*                                                                               
         MVC   WORK(2),RCONSRDT                 YES - DATE                      
         GOTO1 HEXIN,DMCB,RCONSRTI,WORK+2,6           TIME                      
                                                                                
         CLC   WORK(2),WORK+10     COMPARE ORDER DATE TO MKGD SENT DATE         
         BH    PASF38                                                           
         BL    PASF37                                                           
* IF SAME DATE/ COMPARE TIME SENT                                               
         CLC   WORK+2(3),WORK+12   COMPARE SENT TIMES                           
         BH    PASF38                                                           
* MKGD ELEM GETS PREFERENCE                                                     
PASF37   CLI   BYTE,X'03'      MKGD OFFER LAST SENT BY STATION?                 
         BNE   PASF37C         NO                                               
         MVI   BYTE,0          YES - BYTE=0 = DON'T BUILD 8F KEY                
         B     PASF40                                                           
*                                                                               
PASF37C  MVI   BYTE,X'02'          MKGD OFFER LAST SENT BY REP                  
         B     *+14                                                             
PASF38   MVI   BYTE,X'01'          REP ORDER GETS PREFERENCE                    
         MVC   DUB(1),RCONSRV      SENT REP VERSION NUMBER                      
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
****************************************************************                
* UPDATE PASSIVE '8F' KEY                                                       
* BYTE=X'01' ORDER SENT BY REP     X'02'=MKGD OFFER SENT BY REP                 
* BYTE=0 DELETE CURRENT 8F KEY AND DON'T CREATE NEW ONE                         
****************************************************************                
PASF40   DS    0H                                                               
         LA    R6,RCONREC          SCANS ALL 8F'S FOR CONTRACT                  
         XC    KEY,KEY                                                          
K        USING RCON8FTP,KEY                                                     
         MVI   K.RCON8FTP,X'8F'                                                 
         MVC   K.RCON8FRP,RCONKREP                                              
         MVC   K.RCON8FSA,RCONKSTA                                              
*                                                                               
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   PASF41                                                           
         TM    RCONRF1-RCONRFEL(R6),X'20'     HOME MKT K?                       
         BZ    PASF40A                        NO                                
         MVI   K.RCON8FTP,X'9F'               YES - USE 9F KEY                  
         B     PASF41                                                           
*                                                                               
PASF40A  DS    0H    ** CHECK IF WE NEED TO MAKE A HOME MKT ORDER               
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET IN PROGRESS?                     
         BZ    PASF41                                                           
         TM    PROFILES+CNTRPECB,CNTRPECA   EXCEPT BY PROF 38?                  
         BO    PASF41                                                           
         CLI   RCONKOFF,C'0'       OFC CODE 00-99                               
         BL    PASF41              NOT LOCAL OFC - DON'T CONVERT                
         CLI   RCONKOFF,C'9'                                                    
         BH    PASF41                                                           
         CLI   RCONKOFF+1,C'0'                                                  
         BL    PASF41                                                           
         CLI   RCONKOFF+1,C'9'                                                  
         BH    PASF41                                                           
*                                                                               
         OI    RCONRF1-RCONRFEL(R6),X'20'   MAKE IT LOCAL!                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
PASF40B  DS    0H                                                               
         CLC   KEY(RCON8FDT-RCON8FTP),KEYSAVE                                   
         BNE   PASF40D                                                          
         CLC   K.RCON8FCN,RCONKCON   CONTRACT NUMBER                            
         BE    PASF40C                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     PASF40B                                                          
*                                                                               
PASF40C  DS    0H                                                               
         OI    KEY+27,X'80'          DELETE 8F KEY                              
         BAS   RE,DODMWRT                                                       
         MVI   KEY,X'9F'             WRITE IDENTICAL 9F KEY                     
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PASF40D  DS    0H                                                               
         MVC   KEY,KEYSAVE                    RESTORE KEY STUB                  
         MVI   K.RCON8FTP,X'9F'               USE 9F KEY                        
*                                                                               
PASF41   DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
*                                                                               
PASF42   DS    0H                                                               
         LA    R6,RCONREC                                                       
         CLC   KEY(RCON8FDT-RCON8FTP),KEYSAVE                                   
         BNE   PASF55                                                           
         CLC   K.RCON8FCN,RCONKCON   CONTRACT NUMBER                            
         BE    PASF44                                                           
         DROP  K                                                                
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     PASF42                                                           
*                                                                               
PASF44   DS    0H                                                               
******************************************************************              
* SONNET KEY FOUND                                                              
         CLI   BYTE,0              CONTRACT STILL ACTIVE FOR SONNET?            
         BE    PASF51              NO                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A3'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE HERE IF SONNET KEY FOUND             
         USING RCONSON,R6                                                       
         CLC   RCONSRVR,DUB       REP VERSION NUMBER CHANGED ?                  
         BE    PASF53              NO                                           
         DROP  R6                                                               
                                                                                
PASF51   LA    R6,RCONREC           START FROM SCRATCH                          
         MVI   ELCODE,X'A3'                                                     
         BAS   RE,GETEL            IF A3 ELEMENT EXISTS, COMPLETE IT            
         BNE   PASF52                                                           
         USING RCONSON,R6                                                       
         MVI   RCONSNST,X'FF'      'COMPLETED' FLAG                             
         DROP  R6                                                               
         CLI   BYTE,0              WILL WE CREATE NEW A3 ELEM ?                 
         BNE   PASF52              YES                                          
*                                                                               
*                                  NO -  PUT 'COMPLETED' A3 BACK NOW            
***>     GOTO1 VPUTREC,DMCB,RCONREC      PUT NEW CONTRACT RECORD                
                                                                                
PASF52   OI    KEY+27,X'80'              DELETE OLD SONNET KEY                  
         BAS   RE,DODMWRT                                                       
         B     PASF55                    BUILD NEW KEY                          
                                                                                
                                                                                
* - HAS STATUS CHANGED ?                                                        
PASF53   LA    R6,RCONREC           STATUS CHANGED?                             
         MVI   ELCODE,X'A3'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSON,R6                                                       
         CLC   BYTE,RCONSONS       STATUS CHANGED?                              
         BE    *+10                NO                                           
         MVC   RCONSONS,BYTE          YES-SET NEW STATUS                        
         BAS   RE,DODMWRT                                                       
***>     GOTO1 VPUTREC,DMCB,RCONREC       PUT NEW CONTRACT RECORD               
         B     P8FX                       AND THAT'S ALL                        
         DROP  R6                                                               
                                                                                
                                                                                
********************************************************************            
* NO PREVIOUS 8F KEY FOUND - BUILD VIRGIN 8F KEY                                
PASF55   CLI   BYTE,0              IS IT INCOMPLETE STATUS?                     
         BE    P8FX                NO/NO NEED TO CREATE 8F KEY                  
                                                                                
* - BUILD 'NEW' A3 ELEMENT                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A3'        DELETE OLD A3                                
         BAS   RE,GETEL                                                         
         BNE   PASF56                                                           
         LA    R6,RCONREC                                                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'A3',0(R6)),0,0                  
PASF56   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A5'        DELETE OLD A5'S                              
         BAS   RE,GETEL                                                         
         BNE   PASF56A                                                          
         LA    R6,RCONREC                                                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'A5',0(R6)),0,0                  
PASF56A  DS    0H                                                               
         LA    R6,WORK                                                          
         USING RCONSON,R6                                                       
         XC    WORK,WORK                                                        
         MVI   WORK,X'A3'                                                       
         MVI   WORK+1,RCONSOVQ                 40=LENGTH OF BASIC ELEM          
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSDTE)  TODAY'S DATE                     
         MVC   RCONSRVR,DUB                    SENT BY REP' VER #               
         MVC   RCONSONS,BYTE                   SET STATUS                       
         DROP  R6                                                               
* ADD THE ELEMENT                                                               
         LA    R6,RCONREC                                                       
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R6),WORK,0                       
***>     GOTO1 VPUTREC,DMCB,RCONREC      PUT NEW CONTRACT RECORD                
                                                                                
* BUILD KEY                                                                     
         LA    R6,RCONREC                                                       
         MVC   KEY,KEYSAVE         YES/RESET OLD KEY                            
K        USING RCON8FTP,KEY                                                     
         MVC   K.RCON8FIN,=C'NEW'       'NEW' ID                                
         MVC   K.RCON8FCN,RCONKCON       CONTRACT NUMBER                        
         GOTO1 DATCON,DMCB,(5,0),(2,K.RCON8FDT)                                 
         DROP  K                                                                
*                                                                               
PASF57   DS    0H                                                               
         OI    DMINBTS,X'08'                      PASS DELETED RECS             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         NI    DMINBTS,X'FF'-X'08'                TURN IT OFF                   
         CLC   KEY(27),KEYSAVE     IS IT ON FILE ?                              
         BNE   PASF60              NO                                           
         NI    KEY+27,X'FF'-X'80'  YES/TURN OFF DELETE                          
         BAS   RE,DODMWRT                                                       
         B     P8FX                                                             
                                                                                
PASF60   MVC   KEY,KEYSAVE         RESET NEW KEY                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28,TWAKADDR     DISK ADDRESS                                 
         DROP  RF                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
P8FX     DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC      PUT NEW CONTRACT RECORD                
*                                                                               
*****    BRAS  RE,AUDTXT           PUT OUT AUDIT TEXT RECORD                    
*                                                                               
         XIT1                                                                   
                                                                                
DODMWRT  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                .2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0                       
BCABSENT DC    C'ER#0 BUYCODE MISSING: HIT <ENTER>'                             
BCABSLQ  EQU   *-BCABSENT                                                       
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKRK                                                        
MYWORKD  DSECT                                                                  
RELO     DS    A                                                                
REVNUM   DS    X                                                                
SVDARKEY DS    XL27                                                             
MYFLAG   DS    X                                                                
WEBCF    EQU   X'80'               STATION CONFIRM VIA WEB                      
SVMKGKEY DS    XL32                MAKEGOOD KEY SAVE AREA                       
SVWIPNEW DS    XL1                 NEW WIP SAVEAREA                             
MYWORKX  EQU   *                                                                
*                                                                               
PRWORKD  DSECT                                                                  
PRDARKEY DS    XL27                                                             
PRWORKQ  EQU   *-PRWORKD                                                        
         EJECT                                                                  
*********************************************************************           
* FOR DARE LINKED ORDERS:                                                       
* FORBID SEND ACTION IF RECALLED                                                
*********************************************************************           
T8026A   CSECT                                                                  
CKDARERC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'SEND',CONACT                                                  
         BNE   CKDRCX                                                           
*                                                                               
CKDRC10  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        CHECK IF DARE                                
         BAS   RE,GETEL                                                         
         BNE   CKDRC20                                                          
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CKDRC15                                                          
         OC    RCONDRRV,RCONDRRV                                                
         BNZ   CKDRC20                                                          
*                                                                               
CKDRC15  DS    0H                                                               
         TM    RCONDRFG,X'10'      SEND ALLOWED ONLY IF DARE                    
         BZ    CKDRC20             AGENCY ORDER HAS NOT BEEN RECALLED           
         LA    R3,471                                                           
         B     ERROR                                                            
*                                                                               
CKDRC20  DS    0H                  CHECK IF REVISION                            
*                                                                               
*        CLI   RCONDRLN,RCONDL2Q   MUST BE APPROVE BEFORE IT CAN BE             
*        BL    CKDRCX              SENT TO STATION                              
*        OC    RCONDRRV,RCONDRRV                                                
*        BZ    CKDRCX                                                           
*        TM    RCONDRF2,X'04'      MANUAL CHANGES INITIATED                     
*        BZ    CKDRCX                                                           
*        LA    R3,733              MUST APPROVE BEFORE SENDING                  
*        TM    RCONDRFG,X'40'      APPROVED??                                   
*        BZ    ERROR                                                            
*        OC    RCONDRDD,RCONDRDD   DELIVERY DATE??                              
*        BZ    ERROR               MUST HAVE SOMETHING TO COMPARE!              
*        CLC   RCONDRDA(4),RCONDRDD                                             
*        BL    ERROR               MUST BE MORE RECENT THAN DEL. DATE           
*        CLC   RCONDRDA(4),RCONDRDR                                             
*        BL    ERROR               MUST BE MORE RECENT THAN REJ. DATE           
*                                                                               
CKDRCX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*   BUYCODES:  WHEN PROFILE REQUIRES THAT ALL BUYS HAVE BUYCODES,               
*        THIS ROUTINE WILL RETRIEVE ALL BUYS AND CHECK.  FIRST ERROR            
*        CAUSES PROCESS TO HALT, RETURN MESSAGE WITH ERROR NUMBER               
*                                                                               
***********************************************************************         
BUYCODES NTR1  BASE=*,LABEL=*                                                   
         LA    R1,KEY                                                           
         USING RBUYREC,R1                                                       
*                                                                               
         MVI   RBUYKTYP,X'0B'      INSERT BUY TYPE                              
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER 9'S COMPLEMENT                      
         DROP  R1,RF                                                            
*                                                                               
         GOTO1 VHIGH               READ FIRST RECORD                            
         B     BCOD0020                                                         
*                                                                               
BCOD0010 DS    0H                                                               
         GOTO1 VSEQ                READ NEXT RECORDS                            
*                                                                               
BCOD0020 DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
*                                  SAME KEY THRU CONTRACT?                      
         BNE   BCOD0800            NO  - FINISHED - EXIT CC ZERO                
K        USING RBUYREC,KEY                                                      
         CLC   K.RBUYKPLN,=X'FFFFFF' PLAN?                                      
         BE    BCOD0030                                                         
         CLC   K.RBUYKMLN,=X'FF'   SKIP NON-HEADER PLAN RECORD                  
         BE    BCOD0010                                                         
         DROP  K                                                                
*                                                                               
BCOD0030 GOTO1 VGETREC,DMCB,RBUYREC                                             
         LA    R6,RBUYREC          LOOK FOR BUYCODE ELT                         
*                                                                               
*   DELETED OR CANCELLED BUYS ARE EXEMPT FROM BUYCODE REQUIREMENT               
*                                                                               
         CLI   RBUYCHGI,C'C'       BUY CANCELLED?                               
         BE    BCOD0010            YES - GO BACK FOR NEXT BUY                   
         CLI   RBUYCHGI+1,C'C'     BUY CANCELLED?                               
         BE    BCOD0010            YES - GO BACK FOR NEXT BUY                   
         CLI   RBUYCHGI,C'X'       BUY DELETED?                                 
         BE    BCOD0010            YES - GO BACK FOR NEXT BUY                   
         CLI   RBUYCHGI+1,C'X'     BUY DELETED?                                 
         BE    BCOD0010            YES - GO BACK FOR NEXT BUY                   
         MVI   ELCODE,X'5F'                                                     
         BAS   RE,GETEL                                                         
         BNE   BCOD0850            ELEMENT NOT FOUND                            
         USING RBYSCDEL,R6                                                      
         CLC   RBYSCDBC,MYSPACES   CODE PRESENT IN ELEMENT?                     
*                                                                               
         DROP  R6                                                               
*                                                                               
         BNH   BCOD0850            NO  - ERROR                                  
         B     BCOD0010            YES - GO BACK FOR NEXT BUY                   
BCOD0800 EQU   *                                                                
         SR    R0,R0                                                            
         J     BCOD0900                                                         
BCOD0850 EQU   *                                                                
         LTR   RB,RB                                                            
BCOD0900 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOR MEDIA OCEAN USE, TRAP EXISTING BUYLINE MOD CODES AND VERSION              
*     NUMBER AND ADD THEM AS AN ELEMENT.  THE BUY MUST ALWAYS BE                
*     REWRITTEN.                                                                
***********************************************************************         
TRAPMODS NTR1  BASE=*,LABEL=*                                                   
         CLC   CONACT,=C'CF  '     CF / CFX?                                    
         BE    TMOD0020            YES - DELETE MOD CODE ELTS                   
         CLC   =C'CFX',CONACT            AND COMMENT ELTS                       
         BNE   TMOD0040                                                         
TMOD0020 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'D0',RBUYREC)    DELETE MODCODE ELTS             
         B     TMOD0900                                                         
TMOD0040 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   TMOD0050                                                         
         USING RMODELEM,R6                                                      
         CLC   RBUYVER,RMODEL1V    BUY VER =< LAST CONFIRMED VER?               
         BNH   TMOD0900            YES -                                        
         DROP  R6                                                               
         SPACE 1                                                                
TMOD0050 EQU   *                                                                
         MVC   MODELT+MODVER(1),RBUYVER                                         
*                                  INSERT VERSION NUMBER                        
         MVC   MODELT+MODMODS(2),RBUYCHGI                                       
*                                  INSERT EXISTING MOD CODES                    
         LA    R6,RBUYELEM                                                      
TMOD0060 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    TMOD0120            YES - ADD ELEMENT                            
         CLI   0(R6),X'D0'         TRAP MOD ELEMENT?                            
         BNE   TMOD0080            NO  - BUMP TO NEXT ELEMENT                   
         CLC   MODELT,0(R6)        MOD ELT ALREADY IN RECORD?                   
         BE    TMOD0900            YES - DON'T ADD IT AGAIN                     
TMOD0080 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     TMOD0060            GO BACK AND CHECK NEXT ELT                   
TMOD0120 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,MODELT                                     
*                                  ADD ELT TO BUY RECORD                        
TMOD0900 EQU   *                                                                
         XIT1                                                                   
MODELT   DC    X'D005000000'                                                    
MODVER   EQU   2                                                                
MODMODS  EQU   3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOR EOP USERS, IF ORD X'9F' AGENCY AND ADVERTISER ARE                         
* NULLS, REPLACE THESE FIELDS WITH CORRESPONDING EOP AGY AND ADV FLDS           
* PROCCESS FOR BIAS                                                             
***********************************************************************         
ECCHECK  NTR1  BASE=*,LABEL=*                                                   
         LR    RF,RA               MUST BE BIAS/C OR K FORMAT                   
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLI   TWATRFMT,C'J'                                                    
         BE    JDSEC                                                            
         CLI   TWATRFMT,C'B'                                                    
         BE    BIASEC05                                                         
         CLI   TWATRFMT,C'W'                                                    
         BE    BIASEC05                                                         
         CLI   TWATRFMT,C'C'                                                    
         BE    BIASEC05                                                         
         CLI   TWATRFMT,C'K'                                                    
         BNE   ECCHECKX                                                         
         DROP  RF                                                               
                                                                                
BIASEC05 DS    0H                                                               
         LA    R6,RCONREC          PROCESS FOR BIAS/W EC                        
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BE    BIASEC10                                                         
                                                                                
         XC    WORK2(80),WORK2                                                  
         MVC   WORK2(2),=X'9F28'                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
                                                                                
         LA    R6,RCONREC          PROCESS FOR BIAS EC                          
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
                                                                                
BIASEC10 DS    0H                                                               
EKEYD    USING REOPKEY,KEY         CONSTRUCT A EOP B/C/K ADV KEY                
         XC    KEY,KEY                                                          
         MVI   EKEYD.REOPKTYP,X'1B'                                             
         MVC   EKEYD.REOPKREP,REPALPHA                                          
*                                                                               
         LR    RF,RA               MUST BE BIAS/C OR K FORMAT                   
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVI   EKEYD.REOPKSYS,1          BIAS                                   
         CLI   TWATRFMT,C'B'                                                    
         BE    BIASEC15                                                         
         CLI   TWATRFMT,C'W'                                                    
         BE    BIASEC15                                                         
         MVI   EKEYD.REOPKSYS,3                                                 
         CLI   TWATRFMT,C'K'                                                    
         BE    BIASEC15                                                         
         MVI   EKEYD.REOPKSYS,4                                                 
         CLI   TWATRFMT,C'C'                                                    
         BNE   ECCHECKX                                                         
         DROP  RF                                                               
*                                                                               
BIASEC15 DS    0H                                                               
         MVC   EKEYD.REOPKSTA,RCONKSTA                                          
         MVC   EKEYD.REOPKCOD(L'RCONKADV),RCONKADV                              
         DROP  EKEYD                                                            
                                                                                
         GOTO1 VHIGH                 AND MOVE IT INTO ORD ADV FIELD             
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BNE   BIASEC20                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R4,IOAREA                                                        
         USING REOPREC,R4                                                       
         USING RCONXXEL,R6                                                      
         MVC   RCONXADV(6),REOPEQUV                                             
         DROP  R4                                                               
                                                                                
BIASEC20 DS    0H                                                               
         XC    KEY,KEY             CONSTRUCT A EOP BIAS AGY KEY                 
         MVI   KEY,X'1C'                                                        
         MVC   KEY+13(2),REPALPHA                                               
*                                                                               
         LR    RF,RA               MUST BE BIAS/C OR K FORMAT                   
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVI   KEY+15,1            BIAS                                         
         CLI   TWATRFMT,C'B'                                                    
         BE    BIASEC30                                                         
         CLI   TWATRFMT,C'W'                                                    
         BE    BIASEC30                                                         
         MVI   KEY+15,3                                                         
         CLI   TWATRFMT,C'K'                                                    
         BE    BIASEC30                                                         
         MVI   KEY+15,4                                                         
         CLI   TWATRFMT,C'C'                                                    
         BNE   ECCHECKX                                                         
         DROP  RF                                                               
*                                                                               
BIASEC30 DS    0H                                                               
         MVC   KEY+16(5),RCONKSTA                                               
         MVC   KEY+21(6),RCONKAGY                                               
         BAS   RE,TRADEAGY         CHECK FOR TRADE/AGENCY CODE                  
         CLI   HALF,0              TRADE?                                       
         BE    BIASEC40            NO                                           
*                                                                               
         MVI   KEY+25,C'#'         YES - SET UP TRADE AGENCY                    
         MVC   KEY+26(1),HALF         KEY VALUES                                
BIASEC40 EQU   *                                                                
*                                  GET THE EOP RECORD                           
         GOTO1 VHIGH                 AND MOVE IT INTO ORD AGY FIELD             
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BNE   ECCHECKX                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R4,IOAREA                                                        
         USING REOPREC,R4                                                       
         MVC   RCONXAGY(6),REOPEQUV                                             
         B     ECCHECKX                                                         
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* TRADEAGY:  CHECK IF ORDER IS A TRADE ORDER.  IF SO, RETRIEVE AGENCY2          
*        RECORD AND TRADE AGENCY CODE.                                          
*        PASS BACK SETTINGS:                                                    
*         A. BINARY ZERO:  NOT TRADE - USE EXISTING                             
*         B. BINARY X'FF': TRADE, BUT NO TRADE AGENCY ASSIGNED                  
*         C. NOT A OR B  : TRADE AGENCY CODE                                    
*                                                                               
***********************************************************************         
TRADEAGY NTR1                                                                   
         MVC   TRADEKEY,KEY        SAVE 1A KEY TEMPORARILY                      
         MVI   HALF,0              USE HALF AS PASS-BACK                        
**>>>>                                                                          
*                                                                               
*   PASS-BACK FIELD IS USED FOR TRADE AGENCY PROCESSING.  THIS WILL             
*        CONTAIN THE ALTERNATE TRADE OFFICE CODE IF A TRADE                     
*        ORDER'S AGENCY CODE MUST BE ASSIGNED.                                  
*        DO NOT - REPEAT, NOT - STEP ON THE FIRST BYTE OF HALF                  
*        IN THIS ROUTINE.                                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RANDOM FLAG ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   TRAG0060            NOT FOUND                                    
         TM    RCONRF1-RCONRFEL(R6),X'08'                                       
*                                  TRADE ORDER SET?                             
         BNO   TRAG0060            NO                                           
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'1A'           SET AGENCY 2 REC                             
         MVC   KEY+19(6),RCONKAGY  INSERT AGENCY+OFF                            
         MVC   KEY+25(2),RCONKREP  INSERT REP CODE                              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                KEY MUST BE ON FILE                          
         GOTO1 VGETREC,DMCB,IOAREA RETRIEVE THE AGENCY2 RECORD                  
         LA    R6,IOAREA                                                        
         USING RAGY2REC,R6                                                      
         LA    RF,RAGY2FXE         1ST ELEMENT IN RAGY2REC                      
TRAG0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO  -                                        
         DC    H'0'                MUST FIND THE ELEMENT                        
         CLI   0(RF),X'1F'         AGENCY ELEMENT?                              
         BE    TRAG0040            YES -                                        
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     TRAG0020            GO BACK FOR NEXT                             
TRAG0040 EQU   *                                                                
         MVI   HALF,X'FF'          SET NO-FIND FOR ALT TRADE                    
         CLI   RAG2TRAD-RAG2ELEM(RF),C' '                                       
*                                  ANYTHING IN ALT TRADE CODE FIELD?            
         BNH   TRAG0060            NO  - PASS BACK EMPTY FIELD                  
         MVC   HALF(1),RAG2TRAD-RAG2ELEM(RF)                                    
*                                  PASS BACK ALT TRADE CODE                     
TRAG0060 EQU   *                                                                
**>>>>                                                                          
         MVC   KEY(27),TRADEKEY    RESET KEY                                    
         XIT1                                                                   
TRADEKEY DS    CL27                TEMPORARY STORAGE                            
         DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* FOR ELECTRONIC CONTRACTS, IF ORD X'9F' AGENCY AND ADVERTISER ARE              
* NULLS, REPLACE THESE FIELDS WITH CORRESPONDING EOP AGY AND ADV FLDS           
* PROCCESS FOR JDS                                                              
***********************************************************************         
JDSEC    DS    0H                  PROCESS FOR JDS EC                           
         LR    RF,RA               MUST BE A JDS ELECTRONIC CONTRACT            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'J'                                                     
         BNE   ECCHECKX                                                         
         DROP  RF                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BE    JDSEC10                                                          
                                                                                
         XC    WORK2(80),WORK2                                                  
         MVC   WORK2(2),=X'9F50'                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
                                                                                
         LA    R6,RCONREC          PROCESS FOR JDS EC                           
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
                                                                                
JDSEC10  DS    0H                                                               
         USING RCONXXEL,R6                                                      
                                                                                
EKEYD    USING REOPKEY,KEY         CONSTRUCT A EOP JDS ADV KEY                  
         XC    KEY,KEY                                                          
         MVI   EKEYD.REOPKTYP,X'1B'                                             
         MVC   EKEYD.REOPKREP,REPALPHA                                          
         MVI   EKEYD.REOPKSYS,2    JDS                                          
         MVC   EKEYD.REOPKSTA,RCONKSTA                                          
         MVC   EKEYD.REOPKCOD(L'RCONKADV),RCONKADV                              
         DROP  EKEYD                                                            
*                                  GET THE EOP RECORD                           
         GOTO1 VHIGH                 AND MOVE IT INTO ORD ADV FIELD             
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BNE   JDSEC20                                                          
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R4,IOAREA                                                        
         USING REOPREC,R4                                                       
         MVC   RCONXADV(4),REOPEQUV                                             
         DROP  R4                                                               
                                                                                
JDSEC20  DS    0H                                                               
         XC    KEY,KEY             CONSTRUCT A EOP JDS AGY KEY                  
         MVI   KEY,X'1C'                                                        
         MVC   KEY+13(2),REPALPHA                                               
         MVI   KEY+15,2            JDS                                          
         MVC   KEY+16(5),RCONKSTA                                               
         MVC   KEY+21(6),RCONKAGY                                               
*                                  GET THE EOP RECORD                           
         GOTO1 VHIGH                 AND MOVE IT INTO ORD AGY FIELD             
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BNE   ECCHECKX                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R4,IOAREA                                                        
         USING REOPREC,R4                                                       
         MVC   RCONXAGY(6),REOPEQUV                                             
         DROP  R4,R6                                                            
                                                                                
ECCHECKX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* FOR CONTRACTS FROM MASTER REP:                                                
* READ THE CONTROL FILE FOR THE REP SENDING ID                                  
* RETURNS REP ID NUMBER IN WORK                                                 
*********************************************************************           
MASTERID NTR1  BASE=*,LABEL=*                                                   
         LA    R3,573                                                           
         XC    KEY,KEY                                                          
         LA    RF,REPTRANS         SET A(START OF TABLE)                        
MSTR0010 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE REACHED?                        
         BE    ERROR                                                            
         CLC   RCONKREP,DRTPOWER(RF)                                            
*                                  REP FOUND IN TABLE?                          
         BE    MSTR0020            YES                                          
         LA    RF,LREPTRAN(RF)     NO  - BUMP TO NEXT ENTRY                     
         B     MSTR0010            GO BACK FOR NEXT                             
MSTR0020 EQU   *                                                                
         MVI   KEY,C'I'                                                         
         ZIC   RE,DRTLEN(RF)       GET LENGTH OF ENTRY                          
         BCTR  RE,0                BACK OFF 1 FOR EX                            
         EX    RE,MSTR0030         MOVE CODE BY LENGTH                          
         B     MSTR0040                                                         
MSTR0030 MVC   KEY+15(0),DRTSIGN(RF)                                            
MSTR0040 EQU   *                                                                
         CLI   DRTFLAG(RF),0       NEED OFFICE?                                 
         BNE   MSTR0060            NO                                           
         LA    RE,1(RE)            YES - RESET LENGTH OF ENTRY                  
         LA    RF,KEY+15           SET A(START OF SIGNON CODE)                  
         AR    RF,RE               ADD L(POWER CODE)                            
         MVC   0(2,RF),RCONKOFF    INSERT OFFICE OF CONTRACT                    
MSTR0060 EQU   *                                                                
         OC    KEY+15(10),MYSPACES SPACE PAD                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BNE   ERROR                                                            
*                                                                               
         LA    R6,28(R6)                                                        
MSTR0220 CLI   0(R6),X'02'       TEST DESC ELEMENT                              
         BE    MSTR0240                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   MSTR0220                                                         
         B     ERROR                                                            
*                                                                               
MSTR0240 MVC   WORK(2),2(R6)      SIGN-ON                                       
         B     EXXMOD                                                           
*                                                                               
*   REP TRANSLATION TABLE                                                       
*        BYTES   0  -  1  =  POWERCODE FROM CONTRACT                            
*        BYTES   2  -  7  =  CODE FOR CONTROL                                   
*        BYTES   8  -  9  =  LENGTH OF CODE IN 2-7                              
*        BYTES   10 -  11 =  1  =  NO OFFICE NEEDED                             
*                                                                               
DRTPOWER EQU   0                                                                
DRTSIGN  EQU   2                                                                
DRTLEN   EQU   8                                                                
DRTFLAG  EQU   9                                                                
*                                                                               
REPTRANS EQU   *                                                                
         DC    C'U1',C'SUB1  ',AL1(4,1)  TEST: MASTER/SUB                       
LREPTRAN EQU   *-REPTRANS                                                       
         DC    C'U2',C'SUB2  ',AL1(4,1)                                         
         DC    C'UR',C'SUB3  ',AL1(4,1)                                         
         DC    C'K4',C'SYN   ',AL1(3,0)  KRG                                    
         DC    C'KU',C'KR    ',AL1(2,0)  KRG                                    
         DC    C'KF',C'KH    ',AL1(2,0)  KRG                                    
         DC    C'CR',C'CHR   ',AL1(3,0)  KRG                                    
         DC    C'K6',C'NOT   ',AL1(3,0)  KRG                                    
         DC    C'S3',C'SEN   ',AL1(3,0)  KRG                                    
         DC    C'RS',C'ABC   ',AL1(3,0)  KRG                                    
         DC    C'QD',C'SPS   ',AL1(3,0)  KRG                                    
         DC    C'G8',C'KIM   ',AL1(3,0)  KRG                                    
         DC    C'J0',C'KRD   ',AL1(3,0)  KRG                                    
         DC    C'WC',C'WSM   ',AL1(3,0)  KRG                                    
         DC    C'NU',C'CCRS  ',AL1(4,0)  KRG CLEAR CHANNEL                      
         DC    C'AQ',C'ALL   ',AL1(3,0)  INTEREP                                
         DC    C'MG',C'MG    ',AL1(2,0)  INTEREP                                
         DC    C'IF',C'INF   ',AL1(3,0)  INTEREP                                
         DC    C'I2',C'NON   ',AL1(3,0)  INTEREP                                
         DC    C'D4',C'DAR   ',AL1(3,0)  INTEREP                                
         DC    C'CN',C'CCR   ',AL1(3,0)  INTEREP                                
         DC    C'IB',C'ABCR  ',AL1(3,0)  INTEREP                                
         DC    C'NX',C'PRN   ',AL1(3,0)  INTEREP                                
         DC    C'UO',C'CUML  ',AL1(3,0)  INTEREP                                
         DC    C'RA',C'RMR   ',AL1(3,0)  REGIONAL MARKET RADIO                  
         DC    X'FFFF'                                                          
         DS    0H                                                               
*********************************************************************           
* FOR CONVERTED CONTRACTS (SELTEL, PETRY, KATZ, ETC),                           
* READ THE CONTROL FILE FOR THE REP SENDING ID                                  
* RETURNS REP ID NUMBER IN WORK                                                 
*********************************************************************           
GETREPID NTR1  BASE=*,LABEL=*                                                   
         LA    R3,573                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(3),=C'NBC'                                                
         CLC   =C'B1',REPALPHA     B1? NBC/TEL MERGE                            
         BNE   GETRID03                                                         
         CLC   RCONKCON,=X'01000000'                                            
         BL    GETRID02                                                         
         CLC   RCONKCON,=X'01321812'                                            
         BNH   GETRID20                                                         
*                                                                               
GETRID02 DS    0H                                                               
         MVC   KEY+15(3),=C'TEL'   ALL ELSE ARE TEL                             
         B     GETRID20                                                         
*                                                                               
GETRID03 DS    0H                                                               
         MVC   KEY+15(3),=C'FTS'                                                
         CLC   =C'FN',REPALPHA     FN? FOX TELEVISION                           
         BE    GETRID20                                                         
         MVC   KEY+15(3),=C'FSS'                                                
         CLC   =C'FB',REPALPHA     FB? FOX STATION SALES                        
         BE    GETRID20                                                         
         MVC   KEY+15(3),=C'BLR'                                                
         CLC   =C'BL',REPALPHA     BL? BLAIR TELEVISION                         
         BE    GETRID20                                                         
         MVC   KEY+15(3),=C'PET'                                                
         CLC   =C'PV',REPALPHA     PV? PETRY                                    
         BE    GETRID20                                                         
         MVC   KEY+15(3),=C'LAT'                                                
         CLC   =C'PR',REPALPHA     PR? LATINO PETRY                             
         BE    GETRID20                                                         
         MVC   KEY+15(3),=C'SEL'                                                
         CLC   =C'SZ',REPALPHA     SZ OR S2?                                    
         BE    GETRID10                                                         
         MVC   KEY+15(3),=C'INT'                                                
         CLC   =C'S2',REPALPHA     SZ OR S2?                                    
         BE    GETRID10                                                         
*                                                                               
         MVC   KEY+15(3),=C'KCO'   KATZ CONTINENTAL?                            
         CLC   =C'CQ',REPALPHA                                                  
         BE    GETRID05                                                         
         MVC   KEY+15(3),=C'ETV'   KATZ AMERICA NOW EAGLE TV                    
         CLC   =C'AM',REPALPHA                                                  
         BE    GETRID05                                                         
*                                                                               
         MVC   KEY+15(4),=C'TTV4'                                               
         CLC   =C'V4',REPALPHA     TTV4 TEST FILE?                              
         BE    GETR100                                                          
*                                                                               
         MVC   KEY+15(4),=C'KTVX'                                               
         MVC   KEY+19(2),RCONKOFF                                               
         CLC   =C'J1',REPALPHA     KTVXNY/UTS SPLIT OUT FILE?                   
         BE    GETR100                                                          
         B     ERROR                                                            
*                                                                               
GETRID05 DS    0H                                                               
         MVC   KEY+18(2),=C'CR'    KATZ USES DIFFERENT OFFICE CODES             
         CLC   =C'CA',RCONKOFF     FOR CR, CL, AND DN                           
         BE    GETRID25                                                         
         MVC   KEY+18(2),=C'CL'                                                 
         CLC   =C'CV',RCONKOFF                                                  
         BE    GETRID25                                                         
         MVC   KEY+18(2),=C'DN'                                                 
         CLC   =C'DV',RCONKOFF                                                  
         BE    GETRID25                                                         
         B     GETRID20                                                         
*                                                                               
GETRID10 DS    0H                                                               
         MVC   KEY+18(2),=C'NC'    SELTEL USES DIFFERENT OFFICE CODES           
         CLC   =C'CA',RCONKOFF     FOR NC, CL, AND DN                           
         BE    GETR100                                                          
         MVC   KEY+18(2),=C'CL'                                                 
         CLC   =C'CV',RCONKOFF                                                  
         BE    GETR100                                                          
         MVC   KEY+18(2),=C'DN'                                                 
         CLC   =C'DV',RCONKOFF                                                  
         BE    GETR100                                                          
*                                                                               
         CLC   =C'SZ',REPALPHA                                                  
         BNE   GETRID20                                                         
         CLC   =C'DC',RCONKOFF                                                  
         BNE   GETRID20                                                         
         MVC   KEY+18(3),=C'PH1'                                                
         B     GETR100                                                          
*                                                                               
GETRID20 DS    0H                                                               
         MVC   KEY+18(2),RCONKOFF                                               
*                                                                               
* SPECIAL DESTINATION ROUTING FOR FOX STATION SALES, FB                         
*                                                                               
         CLC   =C'FB',REPALPHA                                                  
         BNE   GETRID25                                                         
         MVI   KEY+19,C'Y'                                                      
         CLC   =C'NY',RCONKOFF                                                  
         BE    GETR100                                                          
         MVI   KEY+19,C'P'                                                      
         CLC   =C'AT',RCONKOFF                                                  
         BE    GETR100                                                          
         MVI   KEY+19,C'B'                                                      
         CLC   =C'CH',RCONKOFF                                                  
         BE    GETR100                                                          
         MVI   KEY+19,C'M'                                                      
         CLC   =C'DA',RCONKOFF                                                  
         BE    GETR100                                                          
         MVI   KEY+19,C'S'                                                      
         CLC   =C'LA',RCONKOFF                                                  
         BE    GETR100                                                          
         MVI   KEY+19,C' '                                                      
         B     GETR100                                                          
*                                                                               
*                                                                               
* SPECIAL DESTINATION ROUTING FOR KATZ AM NOW EAGLE TV                          
*                                                                               
GETRID25 DS    0H                                                               
         CLC   =C'AM',REPALPHA                                                  
         BNE   GETRID50                                                         
*                                                                               
* CHECK IF AT OFFICE                                                            
*                                                                               
         CLC   =C'AT',RCONKOFF                                                  
         BNE   GETRID28                                                         
*&&DO                                                                           
         MVI   KEY+20,C'K'                                                      
         CLC   =C'AA',RCONTEM                                                   
         BE    GETR100                                                          
*&&                                                                             
         MVI   KEY+20,C' '                                                      
         B     GETR100                                                          
*                                                                               
* CHECK IF CH OFFICE                                                            
*                                                                               
GETRID28 DS    0H                                                               
         CLC   =C'CH',RCONKOFF                                                  
         BNE   GETRID29                                                         
         MVI   KEY+20,C'1'                                                      
         CLC   =C'T7',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C'2'                                                      
         CLC   =C'TC',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C'3'                                                      
         CLC   =C'T6',RCONTEM                                                   
         BE    GETR100                                                          
*&&DO                                                                           
         MVI   KEY+20,C'K'                                                      
         CLC   =C'AC',RCONTEM                                                   
         BE    GETR100                                                          
*&&                                                                             
         MVI   KEY+20,C' '                                                      
         B     GETR100                                                          
*                                                                               
* CHECK IF DA OFFICE                                                            
*                                                                               
GETRID29 DS    0H                                                               
         CLC   =C'DA',RCONKOFF                                                  
         BNE   GETRID30                                                         
*&&DO                                                                           
         MVI   KEY+20,C'K'                                                      
         CLC   =C'AD',RCONTEM                                                   
         BE    GETR100                                                          
*&&                                                                             
         MVI   KEY+20,C' '                                                      
         B     GETR100                                                          
*                                                                               
* CHECK IF DE OFFICE                                                            
*                                                                               
GETRID30 DS    0H                                                               
         CLC   =C'DE',RCONKOFF                                                  
         BNE   GETRID35                                                         
*&&DO                                                                           
         MVI   KEY+20,C'K'                                                      
         CLC   =C'AT',RCONTEM                                                   
         BE    GETR100                                                          
*&&                                                                             
         MVI   KEY+20,C' '                                                      
         B     GETR100                                                          
*                                                                               
* CHECK IF LA OFFICE                                                            
*                                                                               
GETRID35 DS    0H                                                               
         CLC   =C'LA',RCONKOFF                                                  
         BNE   GETRID40                                                         
         MVI   KEY+20,C'P'                                                      
         CLI   RCONTYPE,C'P'                                                    
         BE    GETR100                                                          
*                                                                               
         MVI   KEY+20,C'1'                                                      
         CLC   =C'T2',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C'2'                                                      
         CLC   =C'TY',RCONTEM                                                   
         BE    GETR100                                                          
*&&DO                                                                           
         MVI   KEY+20,C'K'                                                      
         CLC   =C'AL',RCONTEM                                                   
         BE    GETR100                                                          
*&&                                                                             
         MVI   KEY+20,C' '                                                      
         B     GETR100                                                          
*                                                                               
* CHECK IF NY OFFICE                                                            
*                                                                               
GETRID40 DS    0H                                                               
         CLC   =C'NY',RCONKOFF                                                  
         BNE   GETR100                                                          
         MVI   KEY+20,C'D'                                                      
         CLI   RCONTYPE,C'P'                                                    
         BE    GETR100                                                          
         CLI   RCONTYPE,C'Z'                                                    
         BE    GETR100                                                          
         MVI   KEY+20,C'U'                                                      
         CLI   RCONTYPE,C'B'                                                    
         BE    GETR100                                                          
         CLI   RCONTYPE,C'N'                                                    
         BE    GETR100                                                          
*                                                                               
         MVI   KEY+20,C'1'                                                      
         CLC   =C'TE',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C'2'                                                      
         CLC   =C'TA',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C'3'                                                      
         CLC   =C'T5',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C'4'                                                      
         CLC   =C'TW',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C'6'                                                      
         CLC   =C'AN',RCONTEM                                                   
         BE    GETR100                                                          
         MVI   KEY+20,C' '                                                      
         B     GETR100                                                          
*                                                                               
* SPECIAL DESTINATION ROUTING FOR BLAIR BL                                      
*                                                                               
GETRID50 DS    0H                                                               
         CLC   =C'BL',REPALPHA                                                  
         BNE   GETR100                                                          
         CLC   =C'IT',RCONKOFF                                                  
         BNE   GETRID60                                                         
         MVC   KEY+18(2),=C'NY'                                                 
         B     GETR100                                                          
*                                                                               
* BECAREFUL WITH THIS MAPPING!!                                                 
*                                                                               
GETRID60 DS    0H                                                               
         CLC   =C'LP',RCONKOFF                                                  
         BNE   GETR100                                                          
         MVC   KEY+18(4),=C'LATK'                                               
*                                                                               
GETR100  DS    0H                                                               
         OC    KEY+15(10),MYSPACES SPACE PAD                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BNE   ERROR                                                            
*                                                                               
         LA    R6,28(R6)                                                        
GETR500  CLI   0(R6),X'02'       TEST DESC ELEMENT                              
         BE    GETR510                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETR500                                                          
         B     ERROR                                                            
*                                                                               
GETR510  MVC   WORK(2),2(R6)      SIGN-ON                                       
         B     EXXMOD                                                           
*********************************************************************           
* FOR DARE LINKED ORDERS:                                                       
* VALIDATE CONFIRM ACTION                                                       
* ALSO CHECK FOR EXISTENCE OF AGENCY EQUIVALENCY CODES                          
*********************************************************************           
VALIDARE DS    0H                                                               
         NMOD1 0,*VDAR*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        CHECK IF DARE                                
         BAS   RE,GETEL                                                         
         BNE   VDAREX                                                           
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'04'      SKIP IF EDI CONVERTED                        
         BO    VDAREX                                                           
*&&DO                                                                           
         CLI   RCONKSTA+4,C'F'     FM RADIO STATION?                            
         BE    VDARE05             YES                                          
         CLI   RCONKSTA+4,C'A'     AM RADIO STATION?                            
         BE    VDARE05             NO  - NOT RADIO: SKIP                        
*&&                                                                             
*******                                                                         
* TV CODE                                                                       
*******                                                                         
         TM    RCONDRFG,X'80'      CONF/CNF ALLOWED IF UNLINKED                 
         BO    VDARE03                                                          
         CLC   =C'CFX',CONACT                                                   
         BE    VDARE80                                                          
         B     VDAREX                                                           
*                                                                               
VDARE03  DS    0H                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    VDARE04                                                          
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BO    VDAREX                                                           
VDARE04  DS    0H                                                               
         TM    RCONDRFG,X'40'+X'01' CONF/CNF ALLOWED ONLY IF DARE               
         BNZ   VDARE40             AGENCY ORDER HAS STATUS APPROVED             
*                                  OR IF IT IS A TAKEOVER CONTRACT              
         OC    RCONDRRV,RCONDRRV   REVISION ??                                  
         BNZ   VDARE04A                                                         
         LA    R3,471              IF ORIGINAL ORDER CHECK IF                   
         TM    RCONDRFG,X'10'      ORDER IS RECALLED                            
         BO    ERROR                                                            
*                                                                               
VDARE04A DS    0H                                                               
         LA    R3,452              MUST BE APPROVED TO CONFIRM                  
         TM    RCONDRF2,X'02'      REVISION CANCELLED??                         
         BO    VDAREX                                                           
*                                                                               
* IF LINKED TO XML, OK IF APPROVED BEFORE AND CURRENTLY REJECTED                
*                                                                               
         TM    RCONDRF2,X'01'                                                   
         BZ    VDARE04D                                                         
         OC    RCONDRDA,RCONDRDA                                                
         BNZ   VDAREX                                                           
         B     ERROR                                                            
*                                                                               
VDARE04D DS    0H                                                               
         OC    RCONDRRV,RCONDRRV   REVISION RESEND ??                           
         BZ    ERROR                                                            
*                                  IF REVISION RESEND, MAKE SURE                
         CLC   RCONDRDA,RCONDRDD   REVISION WAS NOT APPROVED                    
         BH    ERROR               SOMETIME IN THIS VERSION                     
         BL    VDAREX              BY CHECKING THE APPROVED DATE/TIME           
         CLC   RCONDRTA,RCONDRTD   VS. RECEIVED DATE/TIME                       
         BH    ERROR                                                            
         B     VDAREX                                                           
*                                                                               
*******                                                                         
* RADIO CODE                                                                    
*******                                                                         
VDARE05  DS    0H                                                               
         TM    RCONDRFG,X'80'      CONF/CNF ALLOWED IF UNLINKED                 
         BO    VDARE10                                                          
         CLC   =C'CFX',CONACT                                                   
         BE    VDARE80                                                          
         B     VDAREX                                                           
*                                                                               
VDARE10  DS    0H                                                               
         CLC   =C'CFX!',CONACT     SPECIAL FOR JODI 12/26/02                    
         BE    VDARE15                                                          
         CLC   =C'CFX',CONACT                                                   
         BNE   VDARE15                                                          
*                                                                               
VDARE13  DS    0H                                                               
         LA    R3,943                                                           
*&&DO                                                                           
         CLI   RCONKSTA+4,C'A'                                                  
         BE    ERROR                                                            
         CLI   RCONKSTA+4,C'F'                                                  
         BE    ERROR                                                            
*&&                                                                             
*                                                                               
VDARE15  DS    0H                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    VDARE20                                                          
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BO    VDAREX                                                           
*                                                                               
VDARE20  DS    0H                                                               
         TM    RCONDRFG,X'40'+X'01' CONF/CNF ALLOWED ONLY IF DARE               
         BNZ   VDARE40             AGENCY ORDER HAS STATUS APPROVED             
*                                  OR IF IT IS A TAKEOVER CONTRACT              
         OC    RCONDRRV,RCONDRRV   REVISION ??                                  
         BNZ   VDARE30                                                          
         LA    R3,471              IF ORIGINAL ORDER CHECK IF                   
         TM    RCONDRFG,X'10'      ORDER IS RECALLED                            
         BO    ERROR                                                            
*                                                                               
VDARE30  DS    0H                                                               
* CALL REPFACS TO RETURN DARE KEY                                               
         LR    R3,R6               BACK UP R6                                   
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFGETDAR,VREPFACS),DMCB,RCONREC,KEY,0,DUB                       
         BL    VDARE35                                                          
         MVI   UPDATE,C'N'                                                      
         GOTO1 VGETREC,DMCB,AIO3                                                
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
         CLI   RDARMEDI,C'R'       ONLY DO THIS FOR RADIO                       
         BNE   VDARE35                                                          
         CLI   RDARBSTS,C'M'       AMEND IS ALLOWED TO BE CONFIRMED             
         BE    VDAREX                                                           
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         BNE   VDAREX                                                           
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'C0'      MATCH-S IS ALLOWED                           
         BO    VDAREX                                                           
*                                                                               
VDARE35  DS    0H                                                               
         LR    R6,R3               RESTORE R6                                   
         USING RCONDREL,R6                                                      
         LA    R3,452              MUST BE APPROVED TO CONFIRM                  
         TM    RCONDRF2,X'02'      REVISION CANCELLED??                         
         BO    VDAREX                                                           
         OC    RCONDRRV,RCONDRRV   REVISION RESEND ??                           
         BZ    ERROR                                                            
*                                  IF REVISION RESEND, MAKE SURE                
         CLC   RCONDRDA,RCONDRDD   REVISION WAS NOT APPROVED                    
         BH    ERROR               SOMETIME IN THIS VERSION                     
         BL    VDAREX              BY CHECKING THE APPROVED DATE/TIME           
         CLC   RCONDRTA,RCONDRTD   VS. RECEIVED DATE/TIME                       
         BH    ERROR                                                            
         B     VDAREX                                                           
*                                                                               
VDARE40  DS    0H                                                               
         CLC   =C'CFX',CONACT                                                   
         BE    VDARE80                                                          
*                                                                               
VDARE50  DS    0H                                                               
         CLI   TWAOFFC,C'*'                                                     
         BE    VDARE70                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,WORK)                                       
*                                                                               
         CLC   RCONDRDA,WORK       WAS LAST ACTION TODAY?                       
         BNE   VDARE70                                                          
*                                                                               
         THMS  DDSTIME=YES         YES, NEED TO CHECK TIME                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         SP    DUB(4),=P'500'      MUST BE AT LEAST 5 MINUTES APART             
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,6,WORK+2                                                      
*                                                                               
* NEED TO ADJUST FOR MINUTES. IF ORIGINAL TIME IS 15:03:14, AND WE PACK         
* SUBTRACT 5 MINUTES, THE RESULT IS 15:98:14. THIS CAN BE ADJUSTED BY           
* SUBTRACTING AN ADDITIONAL 40 MINUTES = 15:58:14                               
*                                                                               
         CLI   WORK+3,X'60'                                                     
         BL    VDARE60                                                          
         SP    DUB(4),=P'4000'                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,WORK+2                                                      
*                                                                               
VDARE60  DS    0H                                                               
         CLC   RCONDRTA,WORK+2                                                  
         BL    VDARE70                                                          
*                                                                               
         LA    R3,RCONREC                                                       
CONRECD  USING RCONREC,R3                                                       
         CLI   CONRECD.RCONKSTA+4,C'A'                                          
         BE    VDARE70                                                          
         CLI   CONRECD.RCONKSTA+4,C'F'                                          
         BE    VDARE70                                                          
         DROP  CONRECD                                                          
*                                                                               
*        L     R3,AIO3                                                          
*        USING RDARREC,R3                                                       
*        CLI   RDARMEDI,C'R'       NO WAITING FOR RADIO                         
*        BE    VDARE70                                                          
*        DROP  R3                                                               
*                                                                               
         LA    R3,651              NEED 5 MIN BREAK                             
         B     ERROR                                                            
*                                                                               
VDARE70  DS    0H                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    VDARE80                                                          
         OC    RCONDRRV,RCONDRRV   REVISION IN THE DARE INBOX??                 
         BNZ   VDARE90             YES, OK TO CONFIRM                           
         DROP  R6                                                               
*                                                                               
VDARE80  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'        CHECK IF CONFIRMED ALREADY                   
         BAS   RE,GETEL                                                         
         BNE   VDARE90                                                          
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BNZ   VDAREX                                                           
         DROP  R6                                                               
                                                                                
VDARE90  DS    0H                  PROCESS ONLY IF NOT CONFIRMED                
         LA    R3,430              MISSING EQUIVALENCY CODE                     
         LA    R2,CONCACTH         POINT AT CONTRACT ACTION IF ERROR            
                                                                                
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         CLC   RAGY2DAR,MYSPACES   ANY CODE ASSIGNED?                           
         BE    ERROR                                                            
         OC    RAGY2DAR,RAGY2DAR                                                
         BZ    ERROR                                                            
                                                                                
         LA    R5,KEY              CHECK IF DARE AGENCY ORDER EXISTS            
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'41'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,MYSPACES                                                
         OC    RAGY2DAR,RAGY2DAR                                                
         BZ    VDARE150                                                         
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDARKORD,RCONDRLK   ORDER NUMBER                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
                                                                                
         LA    R4,RAGY2DAR         CHECK UP TO 4 AGENCY EQUIVALENTS             
         LA    R3,4                                                             
         B     VDARE120                                                         
*                                                                               
** PREP KEY FOR SKIP READING: SKIP TO NEXT AGENCY OFFICE IF AGENCY              
** OFFICE DIDN'T CHANGE                                                         
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
VDARE100 CLC   RDARKAOF,PRVKEY.RDARKAOF  SAME AGENCY OFFICE?                    
         DROP  PRVKEY                                                           
         BNE   VDARE110                                                         
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
VDARE110 XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
*                                                                               
VDARE120 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE   SAME KEY?                        
         BNE   VDARE130                                                         
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,RCONDRLK     MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE   SAME KEY?                        
         BNE   VDARE130                                                         
         CLC   RDARKORD,RCONDRLK     SAME ORDER NUMBER?                         
         BNE   VDARE100              NO -- SKIP READ                            
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    VDAREX                YES -- DARE RECORD BUILT...                
         B     VDARE150                                                         
*                                                                               
VDARE130 CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
VDARE140 LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    VDARE150              YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,VDARE140           CHECK NEXT EQUIVALENCY CODE                
         B     VDARE150                                                         
*                                                                               
         MVC   RDARKAGY,0(R4)      EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF  CLEAR FIELDS AFTER AGENCY CODE             
         BCT   R3,VDARE120                                                      
*                                                                               
VDARE150 DS    0H                                                               
         TM    RCONDRFG,X'01'      TAKEOVER ORDER MIGHT NOT HAVE                
         BO    VDAREX              DARE ORDER IN THE FILE                       
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        SFM/TAKEOVER ORDER MIGHT HAVE DARE           
         BAS   RE,GETEL            HEADER CONFIRMED ALREADY                     
         BE    VDAREX                                                           
*                                                                               
         LA    R3,431              MISSING DARE AGENCY ORDER                    
         LA    R2,CONCACTH         POINT AT CONTRACT ACTION IF ERROR            
         B     ERROR                                                            
         DROP  R5                                                               
*                                                                               
VDAREX   DS    0H                                                               
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHECK IF RADIO EDI. IF SO, CHECK IF THERE IS AN ACTIVE EDI ORDER              
* AND COMPARE THE BUYS WITH THE CONTRACT'S:                                     
*                                                                               
* - IF ALL BUYS MATCH:                                                          
*   1. FLAG -S IN ACTIVE EDI ORDER                                              
*   2. CREAT SCRIPT TO CALL DARE TO MARK ORDER OPEN AND                         
*        SEND OPEN MESSAGE TO AGENCY                                            
*   3. PROCESS AS USUAL                                                         
*                                                                               
* - IF BUYS DO NOT MATCH:                                                       
*   1. GLOBBER TO DARE AND INVOKE AMEND SCREEN                                  
*                                                                               
*********************************************************************           
PROCREDI NTR1  BASE=*,WORK=(R5,PRWORKQ),LABEL=*                                 
         USING PRWORKD,R5                                                       
*                                                                               
         CLI   RCONKSTA+4,C' '                                                  
         BE    PREDIYES                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   PREDIYES                                                         
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      LINKED TO EDI ORDER?                         
         BZ    PREDIYES                                                         
*                                                                               
* CALL REPFACS TO RETURN DARE KEY                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFGETDAR,VREPFACS),DMCB,RCONREC,KEY,0,DUB                       
         BL    PREDIYES                                                         
         MVC   PRDARKEY,KEY                                                     
*                                                                               
         MVC   KEY(27),PRDARKEY                                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PREDIYES                                                         
         GOTO1 VGETREC,DMCB,AIO3                                                
*                                                                               
* CANNOT SEND IF DARE ORDER IS IN STATUS REJECT OR RECALL                       
*                                                                               
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
         LA    R3,947                                                           
         CLI   RDARBSTS,C'R'                                                    
         BE    ERROR                                                            
         CLI   RDARBSTS,C'C'                                                    
         BE    ERROR                                                            
         DROP  R6                                                               
*                                                                               
         LA    R3,183              NO CHANGES SINCE LAST SEND                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BNZ   ERROR                                                            
         DROP  R6                                                               
*                                                                               
*  ONLY ALLOW SEND IF CONTRACT ORDER COMMENT EXISTS                             
*                                                                               
         LA    R3,128              ERROR, NO ORDER COMMENT                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
*                                                                               
* CALL DIFFERENCES BLACK BOX                                                    
*                                                                               
         MVC   WORK(4),ACOMFACS                                                 
         L     RE,=V(REDARTKO)                                                  
         A     RE,RELO                                                          
         STCM  RE,15,WORK+4                                                     
         MVC   WORK+8(4),VGTBROAD                                               
         GOTO1 =V(REREPDIF),DMCB,KEY,WORK,RR=RELO                               
         BNE   PREDI100                                                         
*                                                                               
* ORDER MATCHES CONTRACT. SET FLAG TO GLOBBER TO DARE TO                        
* SEND APPROVE/OPEN NOTICE TO THE AGENCY W/O GOING                              
* THRU THE DIFFERENCES LOGIC SINCE IT HAS BEEN DONE IN THIS MODULE.             
*                                                                               
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
*                                                                               
         CLI   RDARBSTS,0          DO WE NEED TO SEND OPEN NOTICE TO            
         BNE   PREDIYES            AGENCY?                                      
         TM    RDARMISC,X'20'      CHECK NOTDARE                                
         BO    PREDIYES                                                         
*                                                                               
         LR    R7,RA               YES, SET FLAG TO CALL DARE                   
         AHI   R7,TWAWORKQ                                                      
         USING TWAWORK,R7                                                       
         OI    TWAGENFG,TWQGOOPN                                                
         B     PREDIYES                                                         
         DROP  R6,R7                                                            
*                                                                               
* ORDER DOES NOT MATCH CONTRACT, SWAP TO ROM AMEND                              
*                                                                               
PREDI100 DS    0H                                                               
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
         CLI   RDARBSTS,C'M'       ORDER AMENDED ALREADY?                       
         BE    PREDIYES            DON'T BOTHER SWAPPING TO DARE                
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTF',CONCNUMH,,GLRCONNO                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    FULL,FULL                                                        
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
         MVC   FULL(2),=C'*A'     TRIGGER ROM/AMEND                             
         MVC   FULL+2(2),REPALPHA  INCASE I'M MASTER                            
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD',FULL,L'FULL,GLRSTAT                           
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'ROM'    TO ROM                                       
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
***      OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RD,BASERD                                                        
         B     PREDIX              EXIT AND SWAP TO DARE                        
*                                                                               
PREDIYES SR    RC,RC                                                            
PREDINO  LTR   RC,RC                                                            
PREDIX   B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* ORDER MATCHES CONTRACT. GLOBBER TO DARE TO SEND APPROVE/OPEN                  
* NOTICE TO THE AGENCY W/O GOING THRU THE DIFFERENCES LOGIC SINCE               
* IT HAS BEEN DONE IN THIS MODULE EARLIER.                                      
*********************************************************************           
SENDOPEN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTF',CONCNUMH,,GLRCONNO                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    FULL,FULL                                                        
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
         MVC   FULL(2),=C'*O'     TRIGGER DARE/OPEN                             
         MVC   FULL+2(2),REPALPHA  INCASE I'M MASTER                            
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD',FULL,L'FULL,GLRSTAT                           
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'ROM'    TO ROM                                       
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    SOPNNO                                                           
         DC    H'0'                                                             
*                                                                               
SOPNYES  SR    RC,RC                                                            
SOPNNO   LTR   RC,RC                                                            
SOPNX    B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
* FLAG DARE ORDER WITH -S                                                       
*********************************************************************           
FLAGDARE NTR1  BASE=*,WORK=(R4,FDWORKQ),LABEL=*                                 
*                                                                               
         USING FDWORKD,R4                                                       
*                                                                               
         LR    R7,RA                                                            
         AHI   R7,TWAWORKQ                                                      
         USING TWAWORK,R7                                                       
         CLI   TWAACCS,C'$'        NOT FOR STATION                              
         BE    FDAREX                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   FDAREX                                                           
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      LINKED TO EDI ORDER?                         
         BZ    FDAREX                                                           
         DROP  R6                                                               
*                                                                               
* CALL REPFACS TO RETURN DARE KEY                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFGETDAR,VREPFACS),DMCB,RCONREC,KEY,0,DUB                       
         BL    FDAREX                                                           
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FDAREX                                                           
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO3                                                
         MVC   FDHDRDA,KEY+28                                                   
*                                                                               
* REBUILD DARE PASSIVE KEYS                                                     
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO2   : KEY BUILD AREA                                                
*        AIO3   : CURRENT LOCATION OF AGENCY ORDER RECORD                       
*        AIO4   : IO AREA                                                       
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',ACOMFACS),AIO2,AIO3,    X        
               AIO4                                                             
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                                                         
         BNE   FDARE20                                                          
         USING RDARFLEM,R6                                                      
         OI    RDARFLG1,X'40'          FLAG -S                                  
*        NI    RDARFLG1,X'FF'-X'10'    TURN OFF STACF                           
* STILL NOT SURE IF WE SHOULD CLEAR OFF STACF HERE                              
         B     FDARE50                                                          
*                                                                               
FDARE20  DS    0H                                                               
         XC    TWAELEM,TWAELEM                                                  
FLG      USING RDARFLEM,TWAELEM                                                 
*                                                                               
         MVI   FLG.RDARFLCD,X'0F'                                               
         MVI   FLG.RDARFLLN,RDARFLLQ                                            
         OI    FLG.RDARFLG1,X'40'                                               
         DROP  FLG,R6                                                           
*                                                                               
         GOTO1 VADDELEM,DMCB,AIO3,TWAELEM                                       
*                                                                               
FDARE50  DS    0H                                                               
         GOTO1 VPUTREC,DMCB,AIO3                                                
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO2:  KEY BUILD AREA                                                  
*        AIO3:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO4:  IO AREA                                                         
*                                                                               
         L     R6,AIO2             SET A(KEY BUILD AREA)                        
         AHI   R6,800              ADD 800 TO ADDRESS                           
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',ACOMFACS),(R6),AIO3,    X        
               AIO4                                                             
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         L     R6,AIO2             A(PASSIVE BUILD AREA)                        
         AHI   R6,800              R6->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',ACOMFACS),AIO2,(R6),    X        
               FDHDRDA                                                          
*                                                                               
FDAREX   B     EXXMOD                                                           
         DROP  R4,R7                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* FOR DARE LINKED ORDERS:                                                       
* VALIDATE CONFIRM ACTION                                                       
*********************************************************************           
CFRMDARE NTR1  BASE=*,WORK=(R4,CFWORKQ),LABEL=*                                 
*                                                                               
         USING CFWORKD,R4                                                       
         XC    CFFLG,CFFLG         CLEAR LOCAL FLAG                             
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         NI    TWADARE,X'FF'-X'20'-X'80' CLEAR PENDCF                           
         DROP  RF                                                               
*                                                                               
         CLI   RCONKSTA+4,C'F'     FM RADIO STATION?                            
         BE    CF030               YES                                          
         CLI   RCONKSTA+4,C'A'     AM RADIO STATION?                            
         BNE   CFDARNO             NO  - NOT RADIO: SKIP                        
*                                                                               
CF030    DS    0H                                                               
*                                                                               
* CALL REPFACS TO RETURN DARE KEY                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFGETDAR,VREPFACS),DMCB,RCONREC,KEY,0,DUB                       
         BL    CFDARYES                                                         
         MVC   SVDARKEY,KEY                                                     
         MVC   CFHDRDA,KEY+28                                                   
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO3                                                
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
*                                                                               
*        CLI   RDARMEDI,C'R'       ONLY FOR REDI ORDER                          
*        BNE   CFDARX                                                           
         MVC   REVNUM,RDARRNUM                                                  
*                                                                               
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO2:  KEY BUILD AREA                                                  
*        AIO3:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO4:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',ACOMFACS),AIO2,AIO3,    X        
               AIO4                                                             
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERCFCON             ERROR CONFIRMING CONTRACT                    
         LR    R3,R6               R3->X'0F' ELT IN DARE                        
         USING RDARFLEM,R3                                                      
         L     R6,AIO3                                                          
*                                                                               
         TM    RDARFLG1,X'40'      -S?                                          
         BZ    ERCFCON             NO                                           
*                                                                               
         TM    RDARFLG1,X'80'      MATCH-S?                                     
         BO    CF050                                                            
         CLI   RDARBSTS,C'A'       OPEN-S?                                      
         BE    CF050                                                            
         CLI   RDARBSTS,C'M'       AMEND-S?                                     
         BNE   ERCFCON             OTHER STATUS AREN'T ALLOWED TO CF            
*                                                                               
CF050    DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION CONFIRM BECOME STACF                 
         BE    UWCF010             REGARDLESS SPOT/UNWIRE FOR NOW!              
         GOTOR ISSPOT                                                           
         BE    SPOTCF                                                           
*                                                                               
*        GOTOR ISSPOT              WISH THESE WOULD COME TRUE                   
*        BE    SPOTCF                                                           
*F060    DS    0H                  UNWIRE ORDER                                 
*        CLI   TWAACCS,C'$'                                                     
*        BE    UWCF010                                                          
*                                                                               
*                                                                               
UWCF     DS    0H                  CONFIRM UNWIRE ORDER                         
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAGENFG,TWQMASTR                                                
         BO    MSTCF               MASTER CONFIRM FROM DARE                     
*                                                                               
         CLC   REPALPHA,TWARMAST   SUBREP CF                                    
         DROP  RF                                                               
         BE    ERCFCON                                                          
*                                                                               
UWCF010  DS    0H                                                               
         TM    RDARFLG1,X'10'      IF WE GET HERE, WE ARE A SUBREP/STA          
         BO    ERUWCF              ONLY UNWIRE P/PERSON CAN CF TO BUYER         
*                                                                               
         OI    CFFLG,X'80'         NO, STACF                                    
         OI    RDARFLG1,X'10'      ->STACF                                      
         GOTOR SCFDT               RECORD STACF DATE/TIME                       
         B     CF150               PULL NEW PASSIVE                             
*                                                                               
MSTCF    DS    0H                  SAME AS SPOT CONFIRM                         
*                                                                               
SPOTCF   DS    0H                                                               
         CLI   RDARBSTS,C'M'       AMEND-S?                                     
         BNE   CFDARYES            NO, REAL CONFIRM                             
         OI    RDARFLG1,X'20'      YES,MARK ORDER AS PENDING CONFIRM            
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         OI    TWADARE,X'20'       SAVE OFF TO TWA                              
         DROP  RF                                                               
*                                                                               
CF150    DS    0H                                                               
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO2:  KEY BUILD AREA                                                  
*        AIO3:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO4:  IO AREA                                                         
*                                                                               
         L     R6,AIO2             SET A(KEY BUILD AREA)                        
         AHI   R6,800              ADD 800 TO ADDRESS                           
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',ACOMFACS),(R6),AIO3,    X        
               AIO4                                                             
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         L     R6,AIO2             A(PASSIVE BUILD AREA)                        
         AHI   R6,800              R6->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',ACOMFACS),AIO2,(R6),    X        
               CFHDRDA                                                          
*                                                                               
CF200    DS    0H                                                               
         L     R6,AIO3                                                          
         MVC   KEY(27),0(R6)                                                    
         GOTO1 VPUTREC,DMCB,(R6)                                                
*                                  GENERATE AUDIT TRAIL                         
         MVI   DMCB,DHSTACFQ       DEFAULT TO STACF                             
         TM    CFFLG,X'80'                                                      
         BO    *+8                                                              
         MVI   DMCB,DHPENCFQ       PENDING CF                                   
         BRAS  RE,DOAUDIT                                                       
*                                                                               
         TM    CFFLG,X'80'                                                      
         BO    CFDONE              IF STACF, EXIT ALL THE WAY OUT               
*                                                                               
CFDARYES SR    RC,RC                                                            
CFDARNO  LTR   RC,RC                                                            
CFDARX   DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
ERUWCF   DS    0H                                                               
         LA    R3,933              ONLY P/P CAN CF UW CONTRACT TO BUYER         
         B     ERROR                                                            
CFDONE   DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         OI    TWADARE,X'80'       RETURN THIS TO DARE                          
         MVC   DMCB+2(2),=H'937'   ORDER CHANGED TO STACF STATUS                
         GOTO1 VDISMSG,DMCB,,                                                   
         DROP  RF                                                               
         L     RD,BASERD            RETURN TO USER                              
         B     EXXMOD                                                           
ERCFCON  DS    0H                                                               
         LA    R3,934              CONTRACT IS NOT CONFIRMED                    
         B     ERROR                                                            
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
**********************************************************************          
* ROUTINE TO ADD STACF DATE/TIME ELEMENT TO DARE RECORD                         
**********************************************************************          
SCFDT    NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO3                                                          
         GOTO1 VDELELEM,DMCB,(X'50',(R6))                                       
         XC    WORK2,WORK2                                                      
K        USING RDARSTEM,WORK2                                                   
         MVI   WORK2,X'50'                                                      
         MVI   K.RDARSTLN,RDARSTLQ                                              
         GOTO1 DATCON,DMCB,(5,0),(2,K.RDARSTDT)                                 
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,K.RDARSTTM                                                  
         GOTO1 VADDELEM,DMCB,AIO3,WORK2                                         
         DROP  K                                                                
SCFDTX   EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD DARE CONFIRMATION AUDIT TRAIL TO X'51' RECORD                             
***********************************************************************         
DOAUDIT  NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
*                                                                               
         MVC   BYTE,DMCB           SAVE AUDIT TYPE(PCF/STACF)                   
*                                                                               
         XC    KEY,KEY                                                          
KEYD     USING RDARKEY,KEY                                                      
         MVC   KEY(RDARKRT-RDARKEY),SVDARKEY                                    
         MVI   KEYD.RDARKRT,X'70'  GET TRAILER RECORD                           
         DROP  KEYD                                                             
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DOAUDX                                                           
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R4)                                                
*                                                                               
         MVC   WORK(4),HELLO       RECORD DARE HISTORY                          
         MVC   WORK+4(4),DATCON                                                 
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,X'FF'        VALID ACTION                                 
         MVC   DMCB+5(1),BYTE      ACTION CONFIRM                               
         MVC   DMCB+6(1),REVNUM    REVISION NUMBER                              
         MVI   DMCB+7,HFGCFQ       STATUS CHANGES BY CONFIRM                    
         GOTO1 =V(REGENDHT),DMCB,(R4),,WORK,RR=RELO                             
*                                                                               
         GOTO1 VPUTREC,DMCB,(R4)                                                
*                                                                               
DOAUDX   B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ISSPOT: IS THIS A SPOT ORDER?                                                 
***********************************************************************         
ISSPOT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CFWORKD,R4                                                       
         L     R6,AIO3                                                          
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                                                         
         BNE   ISSPOTX                                                          
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRE?                                      
         BO    ISSPNO                                                           
ISSPYES  SR    RC,RC                                                            
ISSPNO   LTR   RC,RC                                                            
ISSPOTX  DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        CAPTURE STAION/REP COMMENTS ON SEND/CONFIRM                  *         
*                                                                     *         
***********************************************************************         
*                                                                               
AUDTXT   NTR1  BASE=*,LABEL=*,WORK=(R4,500)                                     
*                                                                               
         LA    R5,RCONREC          ESTABLISH CONTRACT RECORD                    
         USING RCONREC,R5                                                       
*                                                                               
         MVI   ELCODE,X'20'        FIND SEND ELEMENT                            
         LA    R6,RCONREC                                                       
         BRAS  RE,GETEL                                                         
         BNE   AUDTXTX                DONE IF NONE                              
*                                                                               
         USING RCONSEND,R6         ESTABLISH SEND ELEMENT                       
*                                                                               
         USING RAUDREC,R4          BUILD RECORD HERE                            
*                                                                               
         XC    RAUDKEY(RAUDEL1-RAUDKEY),RAUDKEY     INIT KEY                    
*                                                                               
         MVI   RAUDREC,X'4D'       SET RECORD ID                                
         MVC   RAUDKREP,RCONKREP   SET REP ID                                   
         MVC   RAUDKCON,RCONKCON   SET CONTRACT NUMBER                          
*                                                                               
         MVC   RAUDKVER,RCONSRV    ASSUME REP VERSION                           
*                                                                               
         CLC   RCONSSV,RCONSRV     IF STATION VERSION HIGHER                    
         BNH   *+10                                                             
         MVC   RAUDKVER,RCONSSV       USE IT                                    
*                                                                               
         XI    RAUDKVER,X'FF'      COMPLEMENT VERSION NUMBER                    
*                                                                               
         MVI   RAUDKMOD,X'FF'      DEFAULT TO ZERO MOD NUMBER                   
*                                                                               
         MVI   ELCODE,X'22'        FIND MOD ELEMENT                             
         LA    R6,RCONREC                                                       
         BRAS  RE,GETEL                                                         
         BNE   ATXMODX                DONE IF NONE                              
*                                                                               
         USING RMODELEM,R6         ESTABLISH MODIFICATION ELM                   
*                                                                               
         MVC   FULL(1),RMODEL1V    COPY VERSION OF LATEST MODIFY                
         XI    FULL,X'FF'          COMPLEMENT                                   
*                                                                               
         CLC   =C'CFX ',CONACT     IF CFX ACTION                                
         BNE   *+10                                                             
         MVC   RAUDKVER,FULL          USE MOD ELM VERSION #                     
*                                                                               
         CLC   RAUDKVER,FULL       IF VERSION THAT WAS MODIFIED                 
         BNE   ATXMODX                                                          
*                                                                               
         LLC   RF,RMODEL1M            GET MODIFICATION NUMBER                   
         AHI   RF,1                   BUMP BY ONE                               
         STC   RF,RAUDKMOD            STORE IN KEY                              
         XI    RAUDKMOD,X'FF'         COMPLEMENT                                
*                                                                               
ATXMODX  DS    0H                                                               
*                                                                               
         MVC   KEY,RAUDKEY         READ FOR RECORD                              
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   RAUDKEY,KEY         TROUBLE IF EXISTING RECORD FOUND             
         BNE   ATXRDOK                                                          
*                                                                               
*        EXPECTED FOR CFX XONTRACTS                                             
*                                                                               
         MVI   ELCODE,X'1F'        FIND EXTENDED DESCRIPTION ELEMENT            
         LA    R6,RCONREC                                                       
         BRAS  RE,GETEL                                                         
         BNE   ATXRDDMP               NOT CFX IF NOT FOUND                      
*                                                                               
         USING RCONXEL,R6          ESTABLISH EXTENDED DESCRIPTION ELM           
*                                                                               
         CLI   RCONCFX#,0          IF CFX MOD NUMBER EXISTS                     
         BNE   AUDTXTX                DON'T PUT OUT AUDIT RECS                  
*                                  ELSE DUMP                                    
ATXRDDMP DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
ATXRDOK  DS    0H                                                               
*                                                                               
*        BUILD AUDIT COMMENTS RECORD                                            
*                                                                               
         MVC   RAUDLEN,=AL2(RAUDEL1-RAUDKEY)  SET MINIMUM REC LEN               
*                                                                               
         LA    R3,RAUDEL1          POINT TO FIRST ELEMENT IN RECORD             
         USING RAUDTHEL,R3         ESTABLISH AS COMMENT HEADER ELEMENT          
*                                                                               
         XC    RAUDTHEL(RAUDTHL),RAUDTHEL INIT ELEMENT                          
*                                                                               
         MVI   RAUDTHEL,RAUDTHQ    SET ELEMENT CODE                             
         MVI   RAUDTHLN,RAUDTHL    SET ELEMENT LENGTH                           
*                                                                               
         MVI   ELCODE,X'20'        FIND SEND ELEMENT                            
         LA    R6,RCONREC                                                       
         BRAS  RE,GETEL                                                         
         BNE   AUDTXTX                DONE IF NONE                              
*                                                                               
         USING RCONSEND,R6         ESTABLISH SEND ELEMENT                       
*                                                                               
         CLI   TWAACCS,C'$'        SKIP IF STATION SIGNED ON                    
         BE    ATXREPHN            YES                                          
*                                                                               
         MVI   RAUDTHTP,C'R'       SET TYPE AS REP                              
         MVC   RAUDTHDT,RCONSRDT   SET SEND DATE                                
         MVC   RAUDTHTM,RCONSRTI   SET SEND TIME                                
*                                                                               
         B     ATXVERX                                                          
*                                                                               
ATXREPHN DS    0H                  STATION SEND                                 
*                                                                               
         MVI   RAUDTHTP,C'S'       SET TYPE AS STATION                          
         MVC   RAUDTHDT,RCONSSDT   SET SEND DATE                                
         MVC   RAUDTHTM,RCONSSTI   SET SEND TIME                                
*                                                                               
ATXVERX  DS    0H                                                               
*                                                                               
         BRAS  RE,PID              ADD SENDER'S PID                             
*                                                                               
         MVC   FULL(1),RAUDTHTP    SAVE SEND TYPE                               
*                                                                               
         MVI   ELCODE,X'1F'                                                     
         LA    R6,RCONREC          FIND EXTENDED DESCRIPTION ELEMENT            
         BRAS  RE,GETEL                                                         
         BNE   ATXTOTX               NONE FOUND                                 
*                                                                               
         USING RCONXEL,R6          ESTABLISH EXTENSION ELEMENT                  
*                                                                               
         ICM   RF,15,RCONTOT       STORE CONTRACT TOTAL                         
         CVD   RF,DUB                                                           
         ZAP   RAUDTH$,DUB                                                      
*                                                                               
ATXTOTX  DS    0H                                                               
*                                                                               
         MVI   ELCODE,X'9F'                                                     
         LA    R6,RCONREC          FIND EXTRA DESCRIPTION ELEMENT               
         BRAS  RE,GETEL                                                         
         BNE   ATXASTX               NONE FOUND                                 
*                                                                               
         USING RCONXXEL,R6         ESTABLISH EXTRA DESCRIPTION ELEMENT          
*                                                                               
         MVC   RAUDTHAS,RCONXAST   SAVE SALES ASSISTANT                         
*                                                                               
ATXASTX  DS    0H                                                               
*                                                                               
ATXHDRX  DS    0H                                                               
*                                                                               
         LLC   RF,RAUDTHLN         BUMP TO NEXT ELEMENT                         
         LA    R3,0(RF,R3)                                                      
*                                                                               
         USING RAUDTXEL,R3         ESTABLISH AS TEXT ELEMENT                    
*                                                                               
         XC    RAUDTXEL(128),RAUDTXEL INIT ELEMENT                              
*                                                                               
*        FIND COMMENT ELEMENTS TO SAVE                                          
*                                                                               
         SR    R0,R0               INIT ELEMENT SEQUENCE NUMBER                 
*                                                                               
         MVI   ELCODE,X'82'        ASSUME NEED REP COMMENT ELEMENTS             
*                                                                               
         CLI   FULL,C'S'           IF STATION SEND                              
         BNE   *+8                                                              
         MVI   ELCODE,X'92'           FIND STATION COMMENT ELEMENTS             
*                                                                               
         XC    RAUDTXEL(128),RAUDTXEL INIT TEXT ELEMENT                         
*                                                                               
         CLC   =C'CF',CONACT       SPECIAL IF A CONFIRM ACTION                  
         BE    ATXCON                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         BRAS  RE,GETEL            FIND FIRST ELEMENT OF TYPE                   
*                                                                               
ATXTXTLP DS    0H                                                               
*                                                                               
         BNE   ATXTXTDN                                                         
*                                                                               
         USING RCONROEL,R6         ESTABLISH GENERIC COMMENT ELEMENT            
         AHI   R0,1                BUMP ELEMENT COUNTER                         
*                                                                               
         MVI   RAUDTXEL,RAUDTXQ    SET ELEMENT ID                               
         STC   R0,RAUDTXSQ         SET ELEMENT SEQUENCE NUMBER                  
*                                                                               
         LLC   RF,RCONROCO+1       GET COMMENT ELM LENGTH                       
         SHI   RF,2                DECREMENT BY HEADER LENGTH                   
         BNP   ATXTXTCN            MUST BE SOME TEXT                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RAUDTEXT(0),RCONROCM MOVE COMMENT TO TEXT ELEMENT                
*                                                                               
         LA    RF,1+RAUDTXHL(RF)   ELEMENT LENGTH                               
         STC   RF,RAUDTXLN                                                      
*                                                                               
ATXTXTCN DS    0H                                                               
*                                                                               
         LLC   RF,RAUDTXLN         GET ELEMENT LENGTH                           
         LA    R3,0(RF,R3)         BUMP TO NEXT ELEMENT AREA                    
*                                                                               
         XC    RAUDTXEL(128),RAUDTXEL INIT TEXT ELEMENT                         
*                                                                               
         BRAS  RE,NEXTEL           GET NEXT COMMENT ELEMENT                     
*                                                                               
         B     ATXTXTLP                                                         
*                                                                               
ATXTXTDN DS    0H                                                               
*                                                                               
ASTAUELM MVI   ELCODE,RCONSUEQ     PREPARE TO BUILD STATION USER NAME           
         LA    R6,RCONREC                                                       
         BRAS  RE,GETEL                                                         
         JNE   ASTAUELX                                                         
         USING RCONSUEL,R6                                                      
         CLI   RCONSULN,RCONSUOQ+1 HAVE STATION USER NAME?                      
         JH    *+12                                                             
         CLI   RCONSUNM,C' '       STATION USER NAME IS BLANKED OUT?            
         JNH   ASTAUELX                                                         
         USING RAUDSUEL,R3                                                      
         XC    RAUDSUEL(RAUDSUHL),RAUDSUEL                                      
         MVI   RAUDSUEL,RAUDSUQ                                                 
         LLC   RF,RCONSULN                                                      
         SHI   RF,RCONSUOQ                                                      
         BCTR  RF,0                FOR EX INSTRUCTION                           
         BASR  RE,0                                                             
         MVC   RAUDSUNM(0),RCONSUNM                                             
         EX    RF,0(RE)                                                         
         AHI   RF,1+RAUDSUHL       AUD STATION USER NAME ELEM LENGTH            
         STC   RF,RAUDSULN                                                      
         AR    R3,RF               NEXT AVAILABLE POSITION IN RECORD            
*                                                                               
ASTAUELX DS    0H                                                               
*                                                                               
         MVI   0(R3),0             CLEAR AN EXTRA BYTE                          
*                                                                               
         AHI   R3,1                BUMP 1 BYTE                                  
         SR    R3,R4               RECORD LENGTH                                
*                                                                               
         STCM  R3,3,RAUDLEN        SET RECORD LENGRH                            
*                                                                               
         B     ATXADD              GO ADD RECORD                                
*                                                                               
*        ADD SPECIAL MESSAGES IF CONFIRM ACTION                                 
*                                                                               
ATXCON   DS    0H                                                               
*                                                                               
         MVI   RAUDTXEL,RAUDTXQ    SET ELEMENT ID                               
         MVI   RAUDTXSQ,1          ONLY ONE LINE OF COMMENTS                    
         LA    R5,RAUDTEXT         POINT TO START OF TEXT                       
         MVC   0(3,R5),=C'MOD'     MOD TITLE                                    
         LA    R5,3(R5)            NEXT PRINT POSITION                          
*                                                                               
         MVC   FULL(1),RAUDKMOD    MODIFICATION NUMBER                          
         XI    FULL,X'FF'          COMPLEMENT                                   
         LLC   RF,FULL                                                          
         BCTR  RF,0                DECREMENT BY ONE                             
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R5),DUB+6(2)    PRINT NUMBER                                 
*                                                                               
         LR    RF,R5               KILL LEADING ZEROS                           
         LA    R0,2                                                             
*                                                                               
         CLI   0(RF),C'0'                                                       
         BNE   *+16                                                             
         MVI   0(RF),C' '                                                       
         AHI   RF,1                                                             
         BCT   R0,*-16                                                          
*                                                                               
         MVI   3(R5),C' '                                                       
*                                                                               
         LA    R5,4(R5)            NEXT PRINT POSITION                          
*                                                                               
         CLC   =C'CFX',CONACT      IF REP CONFIRMATION                          
         BE    *+18                                                             
         MVC   0(10,R5),=C'CONFIRMED '                                          
         LA    R5,10(R5)                                                        
         B     ATXCF10                                                          
*                                                                               
         MVC   0(4,R5),=C'CFX '                                                 
         LA    R5,4(R5)                                                         
*                                                                               
ATXCF10  DS    0H                                                               
*                                                                               
         MVI   0(R5),C'V'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         MVC   FULL(1),RAUDKVER    VERSION NUMBER                               
         XI    FULL,X'FF'                                                       
*                                                                               
         LLC   RF,FULL                                                          
         CVD   RF,DUB                                                           
*                                                                               
         MVC   FULL,=X'40202020'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         LHI   RF,3                MAX VERSION DIGITS                           
         LA    R1,FULL+1                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         AHI   R1,1                                                             
         BCT   RF,*-12                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R1)                                                    
*                                                                               
         LA    R5,1(RF,R5)         NEXT PRINT POSITION                          
*                                                                               
         SR    R5,R3               ELEMENT LENGTH                               
         STC   R5,RAUDTXLN                                                      
*                                                                               
         LA    R3,0(R5,R3)         NEXT AVAILABLE POSITION IN RECORD            
         J     ASTAUELM                                                         
*                                                                               
ATXADD   DS    0H                                                               
*                                                                               
         GOTO1 VADDREC,DMCB,RAUDREC ADD AUDIT COMMENT RECORD TO FILE            
*                                                                               
AUDTXTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3,R4,R5,R6                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*   PID - THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK               *         
*         WHICH ARE "PERSONAL ID"                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
PID      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RAUDTHEL,R3         ESTABLISH AS COMMENT HEADER ELEMENT          
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   RAUDTHAG,FATAGYSC   SECURITY FILE AGENCY                         
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         BZ    *+10                                                             
         MVC   RAUDTHPD,FAPASSWD   SAVE PASSWORD ID NUMBER                      
*                                                                               
PIDX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PASSREWR:  RETRIEVE AND REWRITE PASSIVE KEYS                                
*                                                                               
PASSREWR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVMKGKEY,KEY        SAVE CURRENT KEY                             
*                                                                               
*        DELETE A0 PASSIVES                                                     
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
WKMGKEY  USING RMKGKEY,IOAREA                                                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R5,KEY                                                           
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,WKMGKEY.RMKGKREP REPCODE                       
         MVC   RMGSPKEY.RMGSPSTA,WKMGKEY.RMKGKSTA STATION                       
         PACK  RMGSPKEY.RMGSPCON(1),WKMGKEY.RMKGKCON+3(1) CONTRACT              
         PACK  RMGSPKEY.RMGSPCON+1(1),WKMGKEY.RMKGKCON+2(1) REVERSE             
         PACK  RMGSPKEY.RMGSPCON+2(1),WKMGKEY.RMKGKCON+1(1) TO GET 9'S          
         PACK  RMGSPKEY.RMGSPCON+3(1),WKMGKEY.RMKGKCON(1)   COMPLEMENT          
         MVC   RMGSPKEY.RMGSPGRP,WKMGKEY.RMKGKGRP MAKEGOOD GROUP                
*                                                                               
         MVC   27(1,R5),WKMGKEY.RMKGCNTL  STATUS                                
*                                                                               
         LA    RF,WKMGKEY.RMKGELEM SET A(DESCRIPTOR ELT OF MKG REC)             
         USING RMKGSDEM,RF         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   RMGSPKEY.RMGSPDAT,RMKGFOFD 1ST OFFERED DATE                      
         MVC   RMGSPKEY.RMGSPWIP,SAVWIP   OLD WIP STATUS                        
         MVC   SVWIPNEW,RMKGSFG2   NEW WIP STATUS                               
         MVC   RMGSPKEY.RMGSPSTT,RMKGSCST OFFER STATUS                          
         NI    RMGSPKEY.RMGSPSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                 
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    RMGSPKEY.RMGSPSTT,RMKGSLFQ SET INDICATOR                         
*                                                                               
         MVC   RMGSPKEY.RMGSPDST,RMKGSFG1 DARE STATUS                           
         DROP  RF                                                               
*                                                                               
PASA0220 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PASAX               YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PASA0240            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PASA0220            GO BACK FOR NEXT                             
PASA0240 EQU   *                                                                
         USING RMKGXEL,RF                                                       
         MVC   RMGSPKEY.RMGSPSAL,RMKGXSAL SALESPERSON                           
         MVC   RMGSPKEY.RMGSPADV,RMKGXADV ADVERTISER                            
         MVC   SAVDSL,RMKGXDSP     SAVE DEVELOPMENTAL SALESPERSON               
*                                                                               
         DROP  RF                                                               
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
PASA0260 DS    0H                                                               
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0520            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0540            PROCESS NEXT KEY                             
*                                                                               
PAWR0520 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECK                                                         
*                                                                               
PAWR0540 EQU   *                                                                
*                                                                               
*        ADD NEW PASSIVE                                                        
*                                                                               
         MVC   RMGSPKEY.RMGSPWIP,SVWIPNEW NEW WIP STATUS                        
*                                                                               
         MVC   KEY+28(4),SVMKGKEY+28 INSERT D/A                                 
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR NEW PASSIVE                         
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PASA0280            NO                                           
*                                                                               
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         MVC   KEY+28(4),SVMKGKEY+28 INSERT D/A                                 
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PASA0300            GO PROCESS NEXT KEY (IF ANY)                 
*                                                                               
PASA0280 EQU   *                                                                
*                                                                               
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         MVC   KEY+28(4),SVMKGKEY+28 INSERT D/A                                 
         GOTO1 VADD                ADD NEW KEY                                  
*                                                                               
PASA0300 EQU   *                                                                
*                                                                               
         CLI   KEY+1,X'02'         SECOND KEY PROCESSED?                        
         BE    PASA0320            YES - BOTH KEYS DONE                         
*                                                                               
         MVI   KEY+1,X'02'         NO  - SET SECOND KEY TYPE                    
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL   USE DEV SALESPERSON                   
         MVC   RMGSPKEY.RMGSPWIP,SAVWIP   USE OLD WIP                           
*                                                                               
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SAL                           
         BZ    PASA0320                                                         
*                                                                               
         B     PASA0260            GO BACK AND PROCESS                          
*                                                                               
PASA0320 EQU   *                                                                
PAWR0600 DS    0H                                                               
*                                                                               
PASAX    DS    0H                                                               
         MVC   KEY,SVMKGKEY        RESTORE INCOMING KEY                         
         GOTO1 VHIGH               RESTORE FILE POINTERS                        
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
CHECK    EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
         DROP  WKMGKEY,RMGSPKEY                                                 
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
FDWORKD  DSECT                                                                  
FDHDRDA  DS    F                   DARE KEY D/A                                 
FDWORKQ  EQU   *-FDWORKD                                                        
*                                                                               
CFWORKD  DSECT                                                                  
CFHDRDA  DS    F                   DARE KEY D/A                                 
CFFLG    DS    X                   DARE FLAG                                    
CFWORKQ  EQU   *-CFWORKD                                                        
*                                                                               
IMWORKD  DSECT                                                                  
IMKEY    DS    XL32                                                             
IMKEYSAV DS    XL32                                                             
IMDMWORK DS    12D                                                              
IMIO     DS    XL2000                                                           
IMWORKQ  EQU   *-IMWORKD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'202RECNT6A   06/19/15'                                      
         END                                                                    
