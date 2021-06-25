*          DATA SET PPBUY16    AT LEVEL 086 AS OF 04/20/16                      
*PHASE T41116A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY16 - ADDITIONAL CHARGES DISPLAY/CHANGE'                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 04/12/16 LINE NUMBER FIX FOR PBU                                         
*                                                                               
* KWAN 03/20/13 Adjustment for billed additional charge codes                   
*                                                                               
* KWAN 09/27/11 TC CHARGE CAN ONLY BE ATTACHED TO MIDAS PUB 666666              
*                                                                               
* SMYE 06/27/08 MINOR FIX FOR "COMMISSION-ONLY ESTIMATE" ERROR                  
*                                                                               
* KWAN 04/18/08 FX CHARGE CODE                                                  
*                                                                               
* KWAN 09/00/07 PBU UPLOAD OF ADDITIONAL CHARGES                                
*                                                                               
* KWAN 10/09/02 DISALLOW ADDITIONAL CHARGES FOR C RATE INSERTIONS               
*                                                                               
* KWAN 07/22/02 FIX MASTER/SUB CLIENT RECORD LOCKING BUG                        
*                                                                               
* KWAN 05/25/01 NO ADDTNL CHRGS CHANGES ALLOWED IF MATCHED & NOT PAID           
*                                                                               
* KWAN 05/16/01 BEFORE CHANGING REC, NEED TO CHECK FOR LOCKS                    
*                                                                               
* KWAN 05/15/01 DO NOT GROSS UP IF AMT IS NET AND NOT COMMISSIONED              
*                                                                               
* KWAN 04/17/01 MOVE ALL CORE RESIDENT WORK AREAS TO AWRKREC                    
*                                                                               
* KWAN 02/13/01 NEW CODES FOR MAINTAINING ADDITIONAL CHARGES IN BUY             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41116   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41116*                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T41116+4096,R8      NOTE: 2ND REGISTER                           
*                                                                               
         L     R7,AWRKREC          INITIALIZED TO NULLS ALREADY                 
         USING WKT41116,R7                                                      
*                                                                               
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
*                                                                               
         LA    R2,BUYCONH                                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               NEED TO CLR HDR TITLE (MATRLS)               
         XC    8(6,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT FLD                                 
*                                                                               
         LA    R6,BUYHDH                                                        
         USING PPECD,R6            ADDITIONAL CHARGES LOWER SCREEN              
*                                                                               
         LA    R0,WKAREA           CLEAR WORKING AREAS                          
         LA    R1,WKAREAX          SHOULD BE LESS THAN 4095 BYTES               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
         MVI   ACELTABX,X'FF'      INITIALIZE END OF TABLE MARKER               
*                                                                               
         ZAP   LNCTR,=P'0'         INIT COUNTERS                                
         ZAP   FLDCTR,=P'0'                                                     
*                                                                               
         CLI   SVESPROF+28,C'C'    COMMISSION-ONLY ESTIMATE?                    
         BNE   *+16                                                             
         LA    R2,ACHTRH                                                        
         LA    R3,CRATEERR         CHARGES NOT ALLOWED FOR "C" RATES            
         B     ERROR                                                            
*                                                                               
         LA    R2,ACHTRH                                                        
         ST    R2,TRADDR                                                        
         CLC   SVTRCODE,=C'CC'     CALLING FROM BUY00 OR BUY01?                 
         BE    AC30H                                                            
*                                                                               
         CLI   ACHTR,C'*'                                                       
         BNE   *+12                                                             
         LA    R3,NOTRERR                                                       
         B     ERROR                                                            
*                                                                               
         CLC   ACHTR,=C'RC'        RECALL?                                      
         BNE   *+12                                                             
         MVI   ACACT,RECALLAC      ADDNTL CHRGS RECALL ACTION                   
         B     AC45                                                             
         CLC   ACHTR,=C'CC'        CHANGE?                                      
         BNE   *+12                                                             
         MVI   ACACT,CHANGEAC      ADDNTL CHRGS CHANGE ACTION                   
         B     AC45                                                             
         CLI   5(R2),1                                                          
         BNE   AC30                                                             
         CLI   8(R2),C'C'          CHANGE?                                      
         BNE   AC30                                                             
         MVI   ACACT,CHANGEAC      ADDNTL CHRGS CHANGE ACTION                   
         B     AC45                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NOTE: FOLLOWING PARAMETERS ARE GIVEN WHEN CALLED BY T41100                    
*                                                                               
* SVTRCODE - SAVED TRANSACTION CODE                                             
* BYTE4    - SAVED LENGTH OF DATE INPUT (MAX IS 8)                              
* DOUBLE   - SAVED DATE INPUT (IF ANY)                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
AC30     CLC   SVTRCODE,=C'CC'     CHANGE BEFORE A RECALL?                      
         BNE   AC40                                                             
AC30H    MVC   ACHTR,=C'CC'        CODE MIGHT BE LOST FROM OVLAY CALLS          
         MVI   ACHTRH+5,2                                                       
         OI    ACHTRH+6,X'80'                                                   
         MVI   ACACT,CHANGEAC      ADDNTL CHRGS CHANGE ACTION                   
         OC    DOUBLE,DOUBLE       ANY DATE SAVED FROM T41100 CALL?             
         BZ    AC45                                                             
         CLI   BYTE4,0                                                          
         BE    AC45                                                             
         MVC   ACHDTEH+5(1),BYTE4  GET LENGTH                                   
         ZIC   RE,BYTE4                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACHDTE(0),DOUBLE    GET DATE DATE                                
         OI    ACHDTEH+6,X'80'                                                  
*                                                                               
         OI    CHGSW,NEWCCSCR      FIRST TR CODE IS CC                          
         B     AC45                GO VALIDATE INS DATE AGAIN                   
*                                                                               
AC40     DS    0H                                                               
         XC    TRADDR,TRADDR       NXTTR ROUTINE WILL FIGURE IT OUT             
         BRAS  R9,NXTTR                                                         
         BNZ   AC45                                                             
         LA    R3,NOTRERR          NO VALID TR CODE ENTERED ERR MSG             
         B     ERROR                                                            
*                                                                               
AC45     CLI   SVSCRN,X'EC'        ADDITIONAL CHARGE SCREEN?                    
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
*                                                                               
         LA    R2,ACHTRH                                                        
         CLC   ACHTR,=C'DL'                                                     
         BNE   *+12                                                             
         LA    R3,ACDELERR         DELETING A DELETED BUY ERR MDG               
         B     ERROR                                                            
*                                                                               
         CLI   ACHTR,C'B'                                                       
         BNE   *+12                                                             
         LA    R3,NBACERR          NO BUYS ON ADDTNL CHRGS SCREEN               
         B     ERROR                                                            
*                                                                               
         LA    R2,ACHDTEH          POINT TO DATE FLD                            
         TM    4(R2),X'80'         FIELD INPUT THIS TIME?                       
         BZ    *+8                                                              
         OI    CHGSW,DATECHGD      DATE FLD CHANGED, FORCE TO REDISPLAY         
*                                                                               
         TM    CHGSW,DATECHGD      DATE FLD CHANGED?                            
         BZ    AC45M                                                            
         TM    4(R2),X'20'         DATE FLD PREVIOUSLY VALIDATED?               
         BZ    AC45M                                                            
         NI    CHGSW,X'FF'-DATECHGD                                             
*                                                                               
AC45M    CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,INVDTERR         INVALID DATE FORMAT ERR MSG                  
         B     ERROR                                                            
         BRAS  R9,EDTINS           EDIT INSERTION DATE                          
*                                                                               
         BRAS  R9,NXTINS           INSERTION FOUND?                             
         BE    *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,NOINSERR         NO INSERTIONS ON FILE ERR MSG                
         B     ERROR                                                            
*                                                                               
         CLI   BYTE2,C'D'          DELETED INSERTION? PASSED BY NXTINS          
         BNE   AC60                                                             
         CLI   ACACT,RECALLAC      RECALL?                                      
         BE    AC47                OKAY TO DSIP DELETED ADDTNL CHRGS            
         LA    R2,ACHDTEH                                                       
         LA    R3,INDELERR                                                      
         B     ERROR                                                            
*                                                                               
AC47     MVC   ACHTR,=C'D '        INDICATE DELETED BUY                         
         MVI   ACHTRH+5,2                                                       
         OI    ACHTRH+6,X'80'                                                   
*                                                                               
AC60     DS    0H                  CHECK FOR OTHER VALIDATIONS                  
******** TM    T411FFD+12,X'08'    A/C CHANGES ALLOWED?                         
******** BNO   AC70                                                             
******** LA    R2,ACHTRH                                                        
******** LA    R3,FACCERR                                                       
******** B     ERROR                                                            
*                                                                               
         CLI   ACACT,CHANGEAC      ADDNTL CHRGS CHANGE?                         
         BNE   AC70                                                             
         BRAS  RE,CKMATPD          NEED TO CHECK MATCHED & NOT PAID             
         BE    AC70                                                             
         LA    R2,ACHTRH                                                        
         LA    R3,MATPDERR         MATCHED & NOT PAID, NO CHG TO CHRGS          
         B     ERROR                                                            
*                                                                               
AC70     LA    RE,REC+33                                                        
         CLI   0(RE),X'20'         BUY DESCRIPTION ELEM?                        
         BE    *+6                                                              
         DC    H'0'                WORKING WITH WRONG RECORD!                   
         USING PBDELEM,RE                                                       
*                                                                               
         ZAP   SVAGYCOM,PBDACP                                                  
         CP    SVAGYCOM,=P'-1'     MINUS ONE IS 100% IN PBDAC                   
         BNE   *+10                                                             
         ZAP   SVAGYCOM,=P'100000' SAVE THIS AGY COMMISSION FOR LATER           
         DROP  RE                                                               
*                                                                               
         XC    ACCODES(20),ACCODES                                              
         MVI   ACCODESX,X'FF'      END OF TABLE MARKER                          
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
         CLC   ELCODE,0(R5)        PAY ELEM?                                    
         BE    AC80M                                                            
AC80H    BRAS  R9,NEXTEL                                                        
         BNE   AC80X                                                            
AC80M    CLI   1(R5),24            PAY ELEM WITH CHARGE CODE IN IT?             
         BNH   AC80H                                                            
         USING PPAYELEM,R5                                                      
         TM    PPDSTAT,X'01'       IF SECONDARY SQN PRESENT                     
         BNO   *+12                                                             
         CLI   1(R5),26            LENGTH MUST BE OVER 26                       
         BNH   AC80X                                                            
*                                                                               
         LA    RF,ACCODES                                                       
AC80P    CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    AC80X               CANNOT STORE ANY MORE                        
         CLC   0(2,RF),PPACCODE    ALREADY IN TABLE?                            
         BE    AC80H                                                            
         OC    0(2,RF),0(RF)                                                    
         BZ    *+12                                                             
         LA    RF,2(RF)            NEXT ADDTNL CHRG CODE ENTRY                  
         B     AC80P                                                            
         MVC   0(2,RF),PPACCODE                                                 
         B     AC80H                                                            
         DROP  R5                                                               
*                                                                               
AC80X    DS    0H                                                               
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'26'                                                     
         CLC   ELCODE,0(R5)        BILL ELEM?                                   
         BE    AC85M                                                            
AC85H    BRAS  R9,NEXTEL                                                        
         BNE   AC85X                                                            
AC85M    CLI   1(R5),PBACCODE-PBILELEM       Have AC code in element?           
         JL    AC85H                                                            
         USING PBILELEM,R5                                                      
         LA    RF,ACCODES                                                       
AC85P    CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    AC85X               CANNOT STORE ANY MORE                        
         OC    PBACCODE,PBACCODE   Have AC code?                                
         JZ    AC85H                                                            
         CLC   0(2,RF),PBACCODE    ALREADY IN TABLE?                            
         BE    AC85H                                                            
         OC    0(2,RF),0(RF)                                                    
         BZ    *+12                                                             
         LA    RF,2(RF)            NEXT ADDTNL CHRG CODE ENTRY                  
         B     AC85P                                                            
         MVC   0(2,RF),PBACCODE                                                 
         B     AC85H                                                            
         DROP  R5                                                               
*                                                                               
AC85X    DS    0H                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   ACACT,RECALLAC      ADDNTL CHRGS RECALL?                         
         BNE   AC90                                                             
         BRAS  RE,ACDISPLY         DISPLAY ADDITIONAL CHARGES                   
         B     ALLDONE                                                          
*                                                                               
AC90     CLI   ACACT,CHANGEAC      ADDNTL CHRGS CHANGE?                         
         BNE   DEADEND                                                          
         TM    CHGSW,NEWCCSCR      FIRST TR CODE IS CC?                         
         BO    AC92                YES, GO DISPLAY ADDTNL CHRGS                 
         TM    CHGSW,DATECHGD      DATE FLD CHANGED?                            
         BZ    AC93                YES, GO DISPLAY ADDTNL CHRGS                 
AC92     BRAS  RE,ACDISPLY         DISPLAY ADDITIONAL CHARGES                   
         B     AC95                                                             
*                                                                               
AC93     BRAS  RE,SAVESCR                                                       
         BRAS  RE,ACDISPLY         DISPLAY ADDITIONAL CHARGES                   
         BRAS  RE,LOADSCR                                                       
AC95     BRAS  RE,ACCHANGE                                                      
         BRAS  RE,ACDISPLY         DISPLAY ADDITIONAL CHARGES                   
         B     ALLDONE                                                          
*                                                                               
DEADEND  DS    0H                  NO OTHER DEFINED ACTIONS                     
         LA    R2,ACHTRH                                                        
         LA    R3,INVTRERR         INVALID TR CODE ERR MSG                      
         B     ERROR                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ACCHANGE DS    0H                  ADDITIONAL CHARGES CHANGE LOGIC              
         ST    RE,WKSVDSCH                                                      
         LA    R0,ACELTAB                                                       
         LA    R1,ACELTABQ*10      CLEAR AND READY TO REBUILD                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
         MVI   ACELTABX,X'FF'      INITIALIZE END OF TABLE MARKER               
*                                                                               
         LA    R2,ACHCOD1H         POINT TO 1ST FLD ON ADDTNL CHRG LST          
         ZAP   LNCTR,=P'1'         INIT LINE COUNTER                            
ACCHG05  SR    RF,RF                                                            
         LH    RF,WKCOUNT1                                                      
         CHI   RF,10                                                            
         BNL   ACCHG10                                                          
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         BRAS  RE,GETCHRGS         GET ADDITIONAL CHARGES RECORD                
         MVI   DMGSW,C'Y'                                                       
         B     ACCHG10             ADDTNL CHRGS REC READ, STOP LOOP             
*                                                                               
         LA    RF,7                1    2    3      4   5    6    7             
         BRAS  R9,BUMPFLDS         CODE,PROT,DESP,CHARGE,COM,COM%,C/D           
         SR    RF,RF                                                            
         LH    RF,WKCOUNT1                                                      
         AHI   RF,1                                                             
         STH   RF,WKCOUNT1         LOOP COUNTER INCREMENTED                     
         B     ACCHG05                                                          
*                                                                               
ACCHG10  LA    R2,ACHCOD1H         POINT TO 1ST FLD ON ADDTNL CHRG LST          
         ST    R2,FFLDADDR         SAVE FIRST FLD ADDRESS                       
         LA    RE,ACELTAB                                                       
         ST    RE,ACTABPTR                                                      
*                                                                               
         ZAP   LNCTR,=P'0'         INIT LINE COUNTER                            
*                                                                               
ACCHG12  SR    RF,RF                                                            
         LH    RF,WKCOUNT2                                                      
         CHI   RF,10                                                            
         BNL   ACCHG70             DONE CHECKING SCREEN                         
*                                                                               
         AP    LNCTR,=P'1'         BUMP LINE COUNTER                            
         ZAP   FLDCTR,=P'1'        FIRST FIELD                                  
*                                                                               
         CLI   5(R2),0             ANYTHING IN CODE FLD?                        
         BE    ACCHG15                                                          
         OC    8(2,R2),8(R2)       INPUT IS NULL? (POSSIBLE)                    
         BZ    ACCHG15                                                          
         MVC   WKCODE(2),8(R2)     GET INPUT                                    
         CLI   WKCODE+0,0                                                       
         BNE   *+8                                                              
         MVI   WKCODE+0,C' '       PUT IN A SPACE                               
         CLI   WKCODE+1,0                                                       
         BNE   *+8                                                              
         MVI   WKCODE+1,C' '       PUT IN A SPACE                               
*                                                                               
         CLC   WKCODE,=C'TC'       SPECIAL TC CHARGE CODE?                      
         JNE   ACCHG14                                                          
         TM    GENBYSW1,MIDASTPQ   MIDAS TEST PUB?                              
         JNZ   ACCHG14                                                          
         LA    R3,INVERR                                                        
         J     ERROR               ONLY MIDAS PUB ALLOWS TC CHARGE              
*                                                                               
ACCHG14  BRAS  RE,ACNEXTEL         GET ADDITIONAL CHARGES ELEMS                 
         L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         MVC   PACCODE,WKCODE                                                   
         DROP  RE                                                               
         MVI   ERRSW,C'C'          GOT VALIDATED CODE AS INPUT                  
*                                                                               
ACCHG15  BRAS  R9,BUMPFLD          PASS CODE FLD                                
         BRAS  R9,BUMPFLD          PASS ONE CHAR PROTECTED FLD                  
         BRAS  R9,BUMPFLD          PASS DESCRIPTION FLD (PROTECTED)             
*                                                                               
         ZAP   FLDCTR,=P'2'        SECOND INPUT FIELD                           
*                                                                               
         L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         MVI   PACGN,C'G'          G=GROSS BY DEFAULT                           
         ZAP   PACAMT,=P'0'        PACK ZERO                                    
         DROP  RE                                                               
*                                                                               
         CLI   5(R2),0             ANYTHING IN CHARGE FLD?                      
         BE    ACCHG18                                                          
         OC    8(11,R2),8(R2)      INPUT IS NULL? (POSSIBLE)                    
         BZ    ACCHG18                                                          
*                                                                               
         CLI   ERRSW,C'C'          GOT CODE?                                    
         BE    ACCHG17H                                                         
*                                                                               
ACCHG17  L     R2,FFLDADDR                                                      
         LA    R3,MSSNGERR         MISSING ERROR MSG                            
         B     ERROR                                                            
*                                                                               
ACCHG17H MVI   BYTE2,C'$'          INDICATES DOLLAR VALUE                       
         BRAS  RE,CKNUMBER                                                      
         BE    *+12                                                             
         LA    R3,INVERR           NUMBER ENTERED IS INVALID                    
         B     ERROR                                                            
*                                                                               
         L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         MVC   PACGN,BYTE2         N=NET, G=GROSS                               
         MVC   PACAMT,DUB+3                                                     
         DROP  RE                                                               
*                                                                               
ACCHG18  BRAS  R9,BUMPFLD          PASS CHARGE FLD                              
*                                                                               
         ZAP   FLDCTR,=P'3'        THIRD INPUT FIELD                            
*                                                                               
         CLI   5(R2),0             ANYTHING IN COMMISSION Y/N FLD?              
         BE    ACCHG20                                                          
         CLI   8(R2),0             INPUT IS NULL? (POSSIBLE)                    
         BE    ACCHG20                                                          
         CLI   ERRSW,C'C'          GOT CODE?                                    
         BNE   ACCHG17             MISSING CODE, GO FLAG MSSNG ERR MSG          
         BRAS  RE,CKYESNO          CHECKING FOR YES OR NO                       
         L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         MVC   PACAC,8(R2)                                                      
         DROP  RE                                                               
         B     ACCHG20X                                                         
ACCHG20  CLI   ERRSW,C'C'          GOT CODE?                                    
         BNE   ACCHG20X            OKAY IF NO CODE IS ENTERED                   
         LA    R3,MSSNGERR         COMMISSION Y/N FLD IS REQUIRED               
         B     ERROR                                                            
ACCHG20X BRAS  R9,BUMPFLD          PASS COMMISSION Y/N FLD                      
*                                                                               
         ZAP   FLDCTR,=P'4'        FOURTH INPUT FIELD                           
*                                                                               
         CLI   5(R2),0             ANYTHING IN COMMISSION % FLD?                
         BE    ACCHG25M                                                         
         CLC   8(6,R2),=CL6' '     INPUT IS NULL? (POSSIBLE)                    
         BNH   ACCHG25M                                                         
*                                                                               
         CLI   ERRSW,C'C'          GOT CODE?                                    
         BNE   ACCHG17             MISSING CODE, GO FLAG MSSNG ERR MSG          
*                                                                               
         L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         CLI   PACAC,C'Y'          SUBJECT TO AGY COMMISSION?                   
         BE    ACCHG25E                                                         
         CLI   PACAC,C'N'                                                       
         BNE   *+12                                                             
         LA    R3,COMNERR          COM Y/N IS N, CANNOT ENTER COM%              
         B     ERROR               R2 SHOULD BE ON COM (Y/N) FLD                
         DROP  RE                                                               
         LA    R3,COMYERR          COM% ENTERED BUT COM Y/N IS MISSING          
         L     R2,FFLDADDR                                                      
         LA    RF,4                                                             
         BRAS  R9,BUMPFLDS         POINT TO COM (Y/N) FIELD                     
         B     ERROR                                                            
*                                                                               
ACCHG25E MVI   BYTE2,C'%'          INDICATES PERCENTAGE                         
         BRAS  RE,CKNUMBER                                                      
         BE    *+12                                                             
         LA    R3,INVERR           NUMBER ENTERED IS INVALID                    
         B     ERROR                                                            
*                                                                               
ACCHG25H L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         MVC   PACACOM,DUB+4                                                    
         DROP  RE                                                               
*                                                                               
ACCHG25M L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         CLI   PACGN,C'G'          GROSS?                                       
         BE    ACCHG30                                                          
         CP    PACAMT,=P'0'        AMT IS ZERO?                                 
         BE    ACCHG30                                                          
*                                                                               
         CLI   PACAC,C'N'          SUBJECT TO COMMISSION?                       
         BE    ACCHG30             NO, NET AND GROSS ARE SAME                   
*                                                                               
         ZAP   FULL,SVAGYCOM       USE AGY COMMISSION IN BUY DESP ELEM          
         OC    PACACOM,PACACOM                                                  
         BZ    *+10                                                             
         ZAP   FULL,PACACOM                                                     
         ZAP   WKDUB2,=P'100000'                                                
         SP    WKDUB2,FULL         NET PCT                                      
*                                                                               
         CP    WKDUB2,=P'0'                                                     
         BNE   ACCHG25N                                                         
         L     R2,FFLDADDR                                                      
         LA    RF,3                                                             
         BRAS  R9,BUMPFLDS         POINT TO CHARGE FIELD                        
         LA    R3,NETACERR         INVALID NET AMT                              
         B     ERROR                                                            
*                                                                               
ACCHG25N ZAP   FULL,WKDUB2         PUT IT IN FULL                               
         ZAP   WKPACK,PACAMT                                                    
         MP    WKPACK,=P'1000'                                                  
         DP    WKPACK,FULL                                                      
         MP    WKPACK(8),=P'100'                                                
         ZAP   WKDUB2,WKPACK(8)    INTEGER PART OF GROSS AMT                    
         ZAP   WKPACK,WKPACK+8(4)                                               
         MP    WKPACK,=P'1000'     THREE DECIMAL PRECISION                      
         DP    WKPACK,FULL                                                      
         CP    WKPACK(8),=P'995'                                                
         BL    *+14                                                             
         AP    WKDUB2,=P'100'      ROUND UP                                     
         B     ACCHG25U                                                         
         CP    WKPACK(8),=P'-995'                                               
         BH    ACCHG25P                                                         
         AP    WKDUB2,=P'-100'     ROUND UP (NEGATIVE AMT)                      
         B     ACCHG25U                                                         
ACCHG25P ZAP   WKDUB,WKPACK(8)                                                  
         DP    WKDUB,=P'10'        SEE IF THRID DECIMAL NEED ROUNDED            
         OI    WKDUB+7,X'0F'       ALWAYS POSITIVE                              
         CP    WKDUB+6(2),=P'5'                                                 
         BL    ACCHG25R                                                         
         CP    WKDUB(6),=P'0'      NEGATIVE?                                    
         BL    *+14                                                             
         AP    WKDUB(6),=P'1'      ROUND UP                                     
         B     ACCHG25R                                                         
         AP    WKDUB(6),=P'-1'     ROUND UP (NEGATIVE AMT)                      
ACCHG25R AP    WKDUB2,WKDUB(6)                                                  
ACCHG25U CP    WKDUB2,=P'999999999'                                             
         BNH   ACCHG25X            MAX IS RECHED FOR PL5                        
         L     R2,FFLDADDR                                                      
         LA    RF,3                                                             
         BRAS  R9,BUMPFLDS         POINT TO CHARGE FIELD                        
         LA    R3,INVERR                                                        
         B     ERROR               R2 SHOULD BE POINTING TO CHARG FLD           
*                                                                               
ACCHG25X ZAP   PACAMT,WKDUB2+3(5)  CALCULATED GROSS AMT                         
         DROP  RE                                                               
*                                                                               
ACCHG30  BRAS  R9,BUMPFLD          PASS COMMISSION % FLD                        
*                                                                               
         ZAP   FLDCTR,=P'5'        FIFTH INPUT FIELD                            
*                                                                               
         CLI   5(R2),0             ANYTHING IN CASH DISCOUNT Y/N FLD?           
         BE    ACCHG35                                                          
         CLI   8(R2),C' '          INPUT IS NULL? (POSSIBLE)                    
         BNH   ACCHG35                                                          
         CLI   ERRSW,C'C'          GOT CODE?                                    
         BNE   ACCHG17             MISSING CODE, GO FLAG MSSNG ERR MSG          
         BRAS  RE,CKYESNO          CHECKING FOR YES OR NO                       
         L     RE,ACTABPTR                                                      
         USING PACELEM,RE                                                       
         MVC   PACCD,8(R2)                                                      
         DROP  RE                                                               
*                                                                               
ACCHG35  BRAS  R9,BUMPFLD          PASS CASH DISCOUNT Y/N FLD                   
*                                                                               
         L     RE,ACTABPTR                                                      
         MVI   0(RE),X'44'         ADDTNL CHRGS ELEM CODE IN BUY REC            
         MVI   1(RE),32            ADDTNL CHRGS ELEM LENGHT IN BUY REC          
         CLI   4(RE),0             VALID CODE IS ENTERED?                       
         BNE   *+10                                                             
         XC    0(ACELTABQ,RE),0(RE)                                             
         LA    RE,ACELTABQ(RE)                                                  
         ST    RE,ACTABPTR         ADDRESS OF NEXT ELEM IN TAB                  
         SR    RF,RF                                                            
         LH    RF,WKCOUNT2                                                      
         AHI   RF,1                                                             
         STH   RF,WKCOUNT2         LOOP COUNTER INCREMENTED                     
         ST    R2,FFLDADDR         FIRST FLD ADDRESS OF NEXT LINE               
         MVI   ERRSW,0             RESET SWITCH FOR NEXT ROUND                  
         B     ACCHG12                                                          
*                                                                               
ACCHG70  DS    0H                                                               
         CLI   DMGSW,C'Y'                                                       
         BNE   *+12                                                             
         BRAS  RE,GETBUYR          GET BUY RECORD BACK INTO REC                 
         MVI   DMGSW,0                                                          
*                                                                               
         GOTOR VGETINS,DMCB,REC,PVALUES,REC+7,(C'F',0),0,0                      
         MVC   SVGROSS(12),GROSS                                                
*                                                                               
         BRAS  RE,DELACEL                                                       
         BRAS  R9,CLRREC           PREPARE REC FOR MODIFICATIONS                
         BRAS  RE,PEOREC           POINT R5 TO END OF RECORD                    
*                                                                               
         LA    R4,ACELTAB                                                       
ACCHG75  CLI   0(R4),X'FF'         END OF ADDTNL CHRGS TABLE?                   
         BE    ACCHG80                                                          
         CLI   0(R4),0             BLANK ENTRY?                                 
         BE    ACCHG75M                                                         
         GOTO1 VRECUP,DMCB,(1,REC),(R4),(R5)                                    
         BRAS  RE,PEOREC           POINT R5 TO END OF RECORD                    
ACCHG75M LA    R4,ACELTABQ(R4)                                                  
         B     ACCHG75                                                          
*                                                                               
ACCHG80  DS    0H                                                               
         LA    R0,ACELTAB          INIT TABLE FOR UPDATED X'44' ELEMS           
         LA    R1,ACELTABQ*10                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
         BRAS  R2,CHRGSTAB         GET CHARGES INTO TABLE                       
*                                                                               
         GOTO1 VDATCON,DMCB,(3,BTODAY),(2,FULL)                                 
*                                                                               
         LA    RE,ACELTAB                                                       
         LA    RF,SVACTB                                                        
ACCHG80B CLI   0(RE),X'FF'                                                      
         BE    ACCHG90             NO REAL CHANGES AT ALL!                      
         CLC   0(SVACTBQ,RF),0(RE) ACTUALLY CHANGED?                            
         BNE   ACCHG85C            YES                                          
         LA    RE,ACELTABQ(RE)                                                  
         LA    RF,SVACTBQ(RF)                                                   
         B     ACCHG80B                                                         
*                                                                               
ACCHG85C BRAS  RE,PRCFXRT          PROCESS FX RATE                              
*                                                                               
         GOTOR VGETINS,DMCB,REC,PVALUES,REC+7,(C'F',0),0,0                      
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'24'        LOOKING FOR CHANGE ELEM                      
         CLC   ELCODE,0(R5)                                                     
         BE    ACCHG85H                                                         
*                                                                               
ACCHG85D BRAS  R9,NEXTEL                                                        
         BE    ACCHG85H                                                         
         XC    WKELEM,WKELEM       BUILD CHANGE ELEM                            
         MVI   WKELEM+00,X'24'                                                  
         MVI   WKELEM+01,PCHGNEWS  NEW SHORTER CHANGE ELEM VERSION              
         MVC   WKELEM+02(2),FULL   TODAY'S DATE IN BINARY                       
         OI    WKELEM+04,X'10'     INDICATOR FOR ADDTNL CHRGS CHANGE            
         CLC   SVGROSS(12),GROSS   CHANGE IN CHARGE AMOUNT?                     
         BE    ACCHG85E                                                         
         MVI   WKELEM+01,PCHGNEWL  NEW LONGER CHANGE ELEM VERSION               
         MVC   WKELEM+08(12),SVGROSS                                            
         MVC   WKELEM+PCHGOLDL(L'SVPID),SVPID                                   
         MVI   WKELEM+PCHGOLDL+L'SVPID,X'00'                                    
         B     ACCHG85P            GO ADD NEWLY BUILT CHANGE ELEM               
*                                                                               
ACCHG85E DS    0H                                                               
         MVC   WKELEM+PCHGOLDS(L'SVPID),SVPID                                   
         MVI   WKELEM+PCHGOLDS+L'SVPID,X'00'                                    
         B     ACCHG85P            GO ADD NEWLY BUILT CHANGE ELEM               
*                                                                               
ACCHG85H DS    0H                                                               
         CLC   FULL(2),2(R5)       CHANGE DATE IS TODAY?                        
         BNE   ACCHG85D                                                         
         CLI   1(R5),PCHGOLDS      SHORT VERSION?                               
         BE    ACCHG85D                                                         
         CLI   1(R5),PCHGOLDL      LONG VERSION?                                
         BE    ACCHG85D                                                         
         CLI   1(R5),PCHGNEWS      NEW SHORT VERSION?                           
         BE    ACCHG85I                                                         
         CLI   1(R5),PCHGNEWL      NEW LONG VERSION?                            
         BE    ACCHG85J                                                         
         B     ACCHG85D                                                         
*                                                                               
ACCHG85I DS    0H                                                               
         CLC   PCHGOLDS(2,R5),SVPID                                             
         BNE   ACCHG85D                                                         
         B     ACCHG85K                                                         
*                                                                               
ACCHG85J DS    0H                                                               
         CLC   PCHGOLDL(2,R5),SVPID                                             
         BNE   ACCHG85D                                                         
         B     ACCHG85K                                                         
*                                                                               
ACCHG85K DS    0H                                                               
         XC    WKELEM,WKELEM                                                    
         CLI   1(R5),PCHGNEWS      SHORT OR LONG VERSION?                       
         BH    ACCHG85L                                                         
         MVC   WKELEM(08),0(R5)    MOVE UP TO ALL CHANGE INDICATORS             
         OI    WKELEM+4,X'10'      INDICATOR FOR ADDTNL CHRGS CHANGED           
         CLC   SVGROSS(12),GROSS   CHANGE IN CHARGE AMOUNT?                     
         BE    ACCHG85M                                                         
         MVC   WKELEM+08(12),SVGROSS                                            
         MVI   WKELEM+01,PCHGNEWL  LONGER CHANGE ELEM VERSION                   
         MVC   WKELEM+PCHGOLDL(L'SVPID),SVPID                                   
         MVI   WKELEM+PCHGOLDL+L'SVPID,X'00'                                    
         B     ACCHG85M                                                         
*                                                                               
ACCHG85L MVC   WKELEM(PCHGNEWL),0(R5)    LONG VERSION                           
         OI    WKELEM+4,X'10'      INDICATOR FOR ADDTNL CHRGS CHANGED           
*                                                                               
ACCHG85M GOTO1 VRECUP,DMCB,(1,REC),0(R5)         REMOVE OLD ELEM                
ACCHG85P GOTO1 VRECUP,DMCB,(1,REC),WKELEM,(R5)   ADD NEW ELEM                   
         LA    RE,REC+33                                                        
         CLI   0(RE),X'20'         BUY DESP ELEM MUST BE THERE                  
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PBDELEM,RE                                                       
         MVC   PBDDATE,BTODAY      LATEST DATE OF CHANGE                        
         MVC   PBDBUYER,BUYNM      BUYER'S INITIAL                              
         CLI   BUYNM,C'*'                                                       
         BNE   *+10                                                             
         MVC   PBDBUYER,BUYNM+1                                                 
*                                                                               
ACCHG90  BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,ACHTRH                                                        
         LA    R3,DATALOCK                                                      
         B     ERROR                                                            
         BRAS  RE,PUTREC           ELEM ADDED AND BUY REC IS UPDATED            
*                                                                               
ACCHGX   DS    0H                                                               
         XC    WKCOUNT1,WKCOUNT1                                                
         XC    WKCOUNT2,WKCOUNT2   RESET LOOP COUNTERS                          
*                                                                               
         MVI   CHGSW,0                                                          
         L     RE,WKSVDSCH                                                      
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCHRGS DS    0H                                                               
         ST    RE,WKSAVER                                                       
         MVC   WKKEY,REC           SAVE OFF KEY FROM CURRENT BUY REC            
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PSPLKEY,RE                                                       
         MVC   PSPLKAGY,AGYALPHA                                                
         MVC   PSPLKMED,BUYMD                                                   
         MVI   PSPLKRCD,X'60'                                                   
         DROP  RE                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    GETCHR50                                                         
         LA    R2,BUYMDH                                                        
         LA    R3,ACNMERR          ADDNTL CHRGS NOT SET UP FOR MEDIA            
         B     ERROR                                                            
*                                                                               
GETCHR50 LA    RE,REC                                                           
         ST    RE,AREC                                                          
         BRAS  RE,GETREC                                                        
*                                                                               
BBACKX   L     RE,WKSAVER                                                       
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ACDISPLY DS    0H                  ADDITIONAL CHARGES DISPLAY LOGIC             
         ST    RE,WKSVDSCH                                                      
*                                                                               
ACDSP15  LA    R0,ACELTAB                                                       
         LA    R1,ACELTABQ*10      CLEAR TABLE (10 LINES)                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
         MVI   ACELTABX,X'FF'      INITIALIZE END OF TABLE MARKER               
*                                                                               
         BRAS  R2,CHRGSTAB         GET CHARGES INTO TABLE                       
*                                                                               
         LA    R2,ACHCOD1H         POINT TO 1ST FLD ON ADDTNL CHRG LST          
         LA    R4,ACELTAB                                                       
         USING PACELEM,R4                                                       
*                                                                               
ACDSP30  CLI   0(R4),X'FF'         END OF ADDTNL CHRGS TABLE?                   
         BE    ACDSP40                                                          
         XC    8(02,R2),8(R2)      CLR CODE FLD                                 
         OC    PACCODE,PACCODE                                                  
         BZ    ACDSP31H                                                         
         MVC   8(2,R2),PACCODE                                                  
         MVI   5(R2),2             MAX INPUT LENGHT OF 2                        
ACDSP31H OI    6(R2),X'80'         TRANSMIT CODE FLD                            
         NI    1(R2),X'FF'-X'20'   UNPROTECT FLD (IF PROTECTED)                 
         CLC   PACCODE,=C'FX'                                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD FOR FX                         
         BRAS  R9,BUMPFLD          DONE DISPLAY CODE, SKIP                      
*                                                                               
         MVI   8(R2),C' '          CLR ONE CHAR PROTECTED FLD                   
         OI    6(R2),X'80'                                                      
         BRAS  R9,BUMPFLD          SKIP ONE CHAR PROTECTED                      
         BRAS  R9,BUMPFLD          SKIP DESCRIPTION (PROTECTED)                 
*                                                                               
         XC    8(12,R2),8(R2)      CLR CHARGE FLD                               
         CLI   PACGN,0                                                          
         BNE   *+14                                                             
         OC    PACAMT,PACAMT                                                    
         BZ    ACDSP33V            NO AMT PRESENT                               
         CLC   PACCODE,=C'FX'                                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD FOR FX                         
*                                                                               
         MVC   8(1,R2),PACGN                                                    
         CLI   PACGN,C'G'          GROSS AMOUNT?                                
         BE    ACDSP33E                                                         
         CLI   PACGN,C'N'          NET AMOUNT?                                  
         BE    ACDSP33G                                                         
*                                                                               
         DC    H'0'                NO OTHER TYPE OF AMT FOR NOW                 
*                                                                               
ACDSP33E ZAP   WKDUB2,PACAMT       AMT IN GROSS, NO CALCULATIONS NEEDED         
         B     ACDSP33H                                                         
*                                                                               
ACDSP33G CLI   PACAC,C'N'          SUBJECT TO COMMISSION?                       
         BE    ACDSP33E            NO, NET AND GROSS ARE SAME                   
*                                                                               
         ZAP   FULL,SVAGYCOM       AGY COMM ALWAYS PRESENT IN BUY REC           
         OC    PACACOM,PACACOM     ADDTNL CHRGS AGY COMM IS ZERO?               
         BZ    *+10                                                             
         ZAP   FULL,PACACOM        AGY COMM ALWAYS PRESENT IN BUY REC           
*                                                                               
         ZAP   WKPACK,PACAMT                                                    
         ZAP   WKDUB2,=P'100000'                                                
         SP    WKDUB2,FULL         NET PCT                                      
         MP    WKPACK,WKDUB2+4(4)                                               
         DP    WKPACK,=P'100000'   THREE DECIMALS AND PCT                       
         ZAP   WKDUB2,WKPACK(8)                                                 
         OI    WKPACK+11,X'0F'     ALWAYS POSITIVE                              
         CP    WKPACK+8(4),=P'50000'                                            
         BL    ACDSP33H                                                         
         CP    WKDUB2,=P'0'        NEGATIVE AMT?                                
         BL    *+14                                                             
         AP    WKDUB2,=P'1'        ROUND UP                                     
         B     ACDSP33H                                                         
         AP    WKDUB2,=P'-1'       ROUND UP (NEGATIVE)                          
*                                                                               
ACDSP33H DS    0H                                                               
         CP    WKDUB2,=P'0'                                                     
         BNE   *+14                                                             
         MVC   9(4,R2),=C'0.00'                                                 
         B     ACDSP33K                                                         
         EDIT  (P8,WKDUB2),(11,9(R2)),2,ALIGN=LEFT,FLOAT=-                      
ACDSP33K MVI   5(R2),11                                                         
*                                                                               
ACDSP33V OI    6(R2),X'80'         TRANSMIT CHARGE FLD                          
         BRAS  R9,BUMPFLD          DONE DISPLAY CHARGE, SKIP                    
*                                                                               
         CLC   PACCODE,=C'FX'                                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD FOR FX                         
         MVI   8(R2),0             CLR AGY COMMISSION Y/N FLD                   
         CLI   PACAC,0                                                          
         BE    ACDSP34V                                                         
         MVC   8(1,R2),PACAC       AGENCY COMMISSION Y/N (CAN BE NULL)          
         MVI   5(R2),1                                                          
ACDSP34V OI    6(R2),X'80'         TRANSMIT CASH DISCOUNT FLD                   
         BRAS  R9,BUMPFLD          DONE DISPLAY AGY COMM Y/N, SKIP              
*                                                                               
         CLC   PACCODE,=C'FX'                                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD FOR FX                         
         XC    8(6,R2),8(R2)       CLR AGY COM %                                
         OC    PACACOM,PACACOM     AGY COMM % THERE? (NULL OR PACK)             
         BZ    ACDSP36V                                                         
         CP    PACACOM,=P'100000'  ONE HUNDRED PERCENT?                         
         BNE   ACDSP36H                                                         
         MVC   8(6,R2),=C'100.00'  TWO DECIMALS FOR ONE HUNDRED PCT             
         B     ACDSP36M                                                         
ACDSP36H EDIT  (P4,PACACOM),(6,8(R2)),3,ALIGN=LEFT,FLOAT=-                      
ACDSP36M MVI   5(R2),6             FLD HAS BEEN MODIFIED                        
ACDSP36V OI    6(R2),X'80'         TRANSMIT COMMISSION PCT FLD                  
         BRAS  R9,BUMPFLD          DONE DISPLAY COMMISSION %, SKIP              
*                                                                               
         CLC   PACCODE,=C'FX'                                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD FOR FX                         
         MVI   8(R2),0             CLR CASH DISCOUNT Y/N FLD                    
         CLI   PACCD,0                                                          
         BE    ACDSP37V                                                         
         MVC   8(1,R2),PACCD       CASH DISCOUNT Y/N (CAN BE NULL)              
         MVI   5(R2),1             FLD HAS BEEN MODIFIED                        
ACDSP37V OI    6(R2),X'80'         TRANSMIT CASH DISCOUNT FLD                   
         BRAS  R9,BUMPFLD          DONE DISPLY CASH DISCOUNT, SKIP              
*                                                                               
         LA    R4,ACELTABQ(R4)     POINT TO NEXT ITEM IN TABLE                  
         B     ACDSP30                                                          
         DROP  R4                                                               
*                                                                               
ACDSP40  DS    0H                  NOW DISPLAY ADDTNL CHRGS DESCRIPTION         
         LA    R2,ACHCOD1H         POINT TO 1ST FLD ON ADDTNL CHRG LST          
         LA    R4,ACELTAB                                                       
*                                                                               
ACDSP45  CLI   0(R4),X'FF'         END OF ADDTNL CHRGS TABLE?                   
         BE    ACDSP50                                                          
         OC    0(ACELTABQ,R4),0(R4)                                             
         BNZ   ACDSP45H                                                         
         LA    RF,2                1    2                                       
         BRAS  R9,BUMPFLDS         CODE,PROTECTED                               
         XC    8(20,R2),8(R2)      CLR DESCRPTION FLD                           
         B     ACDSP45P                                                         
*                                                                               
ACDSP45H CLI   DMGSW,C'Y'          ALREADY READ IN THIS LOOP?                   
         BE    *+12                                                             
         BRAS  RE,GETCHRGS         GET ADDITIONAL CHARGES RECORD                
         MVI   DMGSW,C'Y'                                                       
*                                                                               
         LA    RF,2                1    2                                       
         BRAS  R9,BUMPFLDS         CODE,PROTECTED                               
         XC    8(20,R2),8(R2)      CLR DESCRPTION FLD                           
*                                                                               
         MVC   WKCODE,3(R4)        SAVE CODE FOR DESCRIPTION LOOK UP            
         OC    WKCODE,WKCODE       CODE IS THERE?                               
         BZ    *+14                                                             
         BRAS  RE,ACNEXTEL         GET ADDITIONAL CHARGES ELEMS                 
         MVC   8(20,R2),5(R5)                                                   
*                                                                               
ACDSP45P OI    6(R2),X'80'         TRANSMIT DESCRIPTION FLD                     
         LA    RF,5                1    2      3   4    5                       
         BRAS  R9,BUMPFLDS         DESP,CHARGE,COM,COM%,C/D                     
*                                                                               
         LA    R4,ACELTABQ(R4)     POINT TO NEXT ITEM IN TABLE                  
         B     ACDSP45                                                          
*                                                                               
ACDSP50  DS    0H                                                               
         CLI   DMGSW,C'Y'          NEED TO RESTORE BUY RECORD?                  
         BNE   *+12                                                             
         BRAS  RE,GETBUYR          GET BUY RECORD BACK INTO REC                 
         MVI   DMGSW,0             RESET SWITCH                                 
*                                                                               
         XC    ACHTEXT,ACHTEXT                                                  
         OI    ACHTEXTH+6,X'80'                                                 
         LA    R2,ACHCOD1H         POINT TO FIRST CODE FLD                      
         LA    R4,ACELTAB                                                       
         LA    R5,ACCODES          TABLE OF BILL/PAY CHARGE CODES               
         XC    WORK,WORK                                                        
         MVI   WORK+20,X'FF'       MAXIMUM OF 10 UNIQUE CHRG CODES              
*                                                                               
ACDSP73  CLI   0(R4),X'FF'         END OF ADDTNL CHRGS TABLE?                   
         BE    ACDSP73X                                                         
ACDSP73H CLI   0(R5),0             BLANK ENTRY?                                 
         BNE   *+12                                                             
         LA    R5,2(R5)                                                         
         B     ACDSP73H                                                         
         CLI   3(R4),0             CHARGE CODE NOT IN ELEMENT?                  
         BNE   *+12                                                             
         LA    R4,ACELTABQ(R4)                                                  
         B     ACDSP73                                                          
         CLC   0(2,R5),3(R4)       CHARGE CODE IS IN BOTH TABLE?                
         BE    ACDSP73P                                                         
         CLI   0(R5),X'FF'         END OF ADDTNL CHRGS TABLE?                   
         BE    *+12                                                             
         LA    R5,2(R5)                                                         
         B     ACDSP73H                                                         
         LA    RF,7                1    2    3      4   5    6    7             
         BRAS  R9,BUMPFLDS         CODE,PROT,DESP,CHARGE,COM,COM%,C/D           
ACDSP73K LA    R4,ACELTABQ(R4)                                                  
         LA    R5,ACCODES          TABLE OF BILL/PAY CHARGE CODES               
         B     ACDSP73                                                          
*                                                                               
ACDSP73P DS    0H                                                               
         LA    R9,WORK                                                          
ACDSP73Q CLI   0(R9),X'FF'         END?                                         
         BE    ACDSP73T                                                         
         CLI   0(R9),0             NOT IN TABLE YET?                            
         BE    ACDSP73R                                                         
         CLC   0(2,R9),0(R5)       ALREADY PROTECTED?                           
         BE    ACDSP73T                                                         
         LA    R9,2(R9)                                                         
         B     ACDSP73Q                                                         
ACDSP73R OI    1(R2),X'20'         PROTECT CHARGE CODE FLD                      
         MVC   0(2,R9),0(R5)       PUT CHRG CODE INTO ALREADY PROT TAB          
*                                                                               
ACDSP73T BRAS  R9,BUMPFLD          POINT TO ONE CHAR PROTECTED FLD              
         MVI   8(R2),C'*'                                                       
         OI    6(R2),X'80'         BILL/PAY OCCURED FOR THIS CODE               
         LA    RF,6                1    2    3      4   5    6                  
         BRAS  R9,BUMPFLDS         PROT,DESP,CHARGE,COM,COM%,C/D                
         CLI   BPACSW,C'Y'                                                      
         BE    ACDSP73U                                                         
         XC    ACHTEXT,ACHTEXT                                                  
         MVC   ACHTEXT(L'BPACTEXT),BPACTEXT                                     
         OI    ACHTEXTH+6,X'80'                                                 
ACDSP73U MVI   BPACSW,C'Y'                                                      
         B     ACDSP73K                                                         
ACDSP73X DS    0H                  END OF CHARGE REMOVAL CHECKING               
*                                                                               
         MVI   BPACSW,0            RESET BILL/PAY TEXT SWITCH                   
*                                                                               
         LA    R0,SVACTB                                                        
         LA    R1,SVACTBQ*10       CLEAR SAVE TABLE (10 LINES)                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
         MVI   SVACTBX,X'FF'                                                    
*                                                                               
         LA    RE,ACELTAB                                                       
         LA    RF,SVACTB                                                        
ACDSP80  CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    ACDSP80X                                                         
         MVC   0(SVACTBQ,RF),0(RE)                                              
         LA    RE,ACELTABQ(RE)                                                  
         LA    RF,SVACTBQ(RF)                                                   
         B     ACDSP80                                                          
*                                                                               
ACDSP80X DS    0H                                                               
         CLI   0(RF),X'FF'         END OF SAVE TABLE?                           
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG IN COPYING LOGIC             
*                                                                               
         L     RE,WKSVDSCH                                                      
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETBUYR  DS    0H                                                               
         ST    RE,WKSAVER                                                       
         MVC   KEY(25),WKKEY                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BUY REC MUST BE RESTORED!                    
*                                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         BRAS  RE,GETREC                                                        
         CLI   REC+33,X'20'        BUY DESCRIPRION ELEM?                        
         BE    *+6                                                              
         DC    H'0'                RECORD RETURNED IS WRONG!                    
*                                                                               
         NI    DMINBTS,X'F7'       RESET DELETES                                
         B     BBACKX              RESTORE RE AND BRANCH BACK                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ACNEXTEL DS    0H                                                               
         ST    RE,WKSAVER                                                       
         LA    R5,REC+33           FIRST ELEM OF ADDTNL CHGRS REC               
         MVI   ELCODE,X'10'                                                     
         LA    R3,CDNFERR          ADDTNL CHRGS CODE NOT FOUND                  
         CLI   0(R5),X'10'                                                      
         BE    *+8                                                              
         B     ERROR                                                            
*                                                                               
         USING PSPLELEM,R5                                                      
ACNEX50  CLC   PSPLCODE,WKCODE     CODE MATCHED?                                
         BE    ACNEX90X                                                         
         BRAS  R9,NEXTEL                                                        
         BE    ACNEX50             FOUND, TRY NEXT ONE                          
         CLC   WKCODE,=C'FX'       FOREIGN EXCHANGE?                            
         BNE   ERROR                                                            
         XC    TEMP1,TEMP1                                                      
         MVC   TEMP1+5(20),=C'USD TO CAD EXCHG AMT'                             
         LA    R5,TEMP1                                                         
*                                                                               
ACNEX90X B     BBACKX              RESTORE RE AND BRANCH BACK                   
         DROP  R5                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PEOREC   ST    RE,WKSAVER                                                       
         LA    R5,REC+33           FIRST ELEM OF BUY REC                        
         MVI   ELCODE,X'FF'                                                     
         BRAS  R9,NEXTEL           R5 SHOULD POINT TO END OF REC                
         B     BBACKX                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHRGSTAB LA    R4,ACELTAB          POINT TO ADDTNL CHRGS TAB                    
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'44'        ADDTNL CHRGS ELEM CODE                       
CHRGST10 BRAS  R9,NEXTEL           NOTE: 1ST ELEM IN BUY ALWAYS X'20'           
         BE    CHRGST20                                                         
         BR    R2                                                               
*                                                                               
CHRGST20 DS    0H                  PUT FOUND ELEM INTO ADDTNL CHRGS TAB         
         CLI   0(R4),X'FF'         END OF ADDTNL CHRGS TABLE?                   
         BNE   *+6                                                              
         DC    H'0'                MAXIMUM OF TEN ALLOWED IN BUY RECORD         
         MVC   0(ACELTABQ,R4),0(R5)                                             
         LA    R4,ACELTABQ(R4)     NEXT ITEM IN ADDTNL CHRGS TABLE              
         B     CHRGST10                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKYESNO  DS    0H                  CHECKING FOR YES OR NO                       
         CLI   8(R2),C'Y'                                                       
         BER   RE                                                               
         CLI   8(R2),C'N'                                                       
         BER   RE                                                               
         LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SAVESCR  DS    0H                                                               
         ST    RE,WKSAVER                                                       
         LA    R0,SVSCREEN                                                      
         LA    R1,SVSCREEQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         LA    R2,ACHCOD1H         POINT TO 1ST FLD ON ADDTNL CHRG LST          
         LA    R3,SVSCREEN         POINT TO TABLE                               
         LA    RF,10               TEN LINES TO BE CLEARED                      
*                                                                               
SAVESC30 MVC   00(02,R3),8(R2)     CODE                                         
         BRAS  R9,BUMPFLD                                                       
         BRAS  R9,BUMPFLD          ONE CHAR PROTECTED                           
         BRAS  R9,BUMPFLD          DESCRIPTION (NO NEED TO SAVE)                
         MVC   02(12,R3),8(R2)     CHARGE                                       
         BRAS  R9,BUMPFLD                                                       
         MVC   14(01,R3),8(R2)     COMMISSION                                   
         BRAS  R9,BUMPFLD                                                       
         MVC   15(06,R3),8(R2)     COMMISSION PERCENTAGE                        
         BRAS  R9,BUMPFLD                                                       
         MVC   21(01,R3),8(R2)     CASH DISCOUNT                                
         BRAS  R9,BUMPFLD                                                       
         LA    R3,SVSCRLQ(R3)                                                   
         BCT   RF,SAVESC30                                                      
*                                                                               
         B     BBACKX                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LOADSCR  DS    0H                                                               
         ST    RE,WKSAVER                                                       
*                                                                               
         LA    R2,ACHCOD1H         POINT TO 1ST FLD ON ADDTNL CHRG LST          
         LA    R3,SVSCREEN         POINT TO TABLE                               
         LA    RF,10               TEN LINES TO BE CLEARED                      
*                                                                               
LOADSC30 MVC   08(02,R2),00(R3)    CODE                                         
         BRAS  R9,BUMPFLD                                                       
         BRAS  R9,BUMPFLD          ONE CHAR PROTECTED                           
         BRAS  R9,BUMPFLD          DESCRIPTION (NO NEED TO SAVE)                
         MVC   08(12,R2),02(R3)    CHARGE                                       
         BRAS  R9,BUMPFLD                                                       
         MVC   08(01,R2),14(R3)    COMMISSION                                   
         BRAS  R9,BUMPFLD                                                       
         MVC   08(06,R2),15(R3)    COMMISSION PERCENTAGE                        
         BRAS  R9,BUMPFLD                                                       
         MVC   08(01,R2),21(R3)    CASH DISCOUNT                                
         BRAS  R9,BUMPFLD                                                       
         LA    R3,SVSCRLQ(R3)                                                   
         BCT   RF,LOADSC30                                                      
*                                                                               
         B     BBACKX                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRREC   DS    0H                  CLEAR END OF RECORD                          
         MVC   HALF,REC+25         RECORD LENGTH                                
         LA    R0,REC                                                           
         AH    R0,HALF             END OF REC (FROM RECORD LENGTH)              
         LHI   R1,4000             MAX RECORD LENGTH                            
         SH    R1,HALF             NUMBER OF BYTES TO BE CLEARED                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,BUMPFLDS                                                      
         BR    R9                                                               
*                                                                               
BUMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTR    GOTO1 VNXTTR                                                           
         CLI   ERRAREA,0                                                        
         BNE   TESTERR                                                          
         XC    INSDA,INSDA                                                      
         XC    INSKEY,INSKEY                                                    
         XC    INSADR,INSADR                                                    
         XC    BINSDT,BINSDT                                                    
         MVI   BSUBLN,0                                                         
         L     R2,TRADDR           GET NEW TR ADDR                              
         MVC   TRCODE,8(R2)                                                     
         LTR   R2,R2                                                            
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTINS   DS    0H                                                               
         GOTO1 VEDTINS,DMCB,(RC),(RA)                                           
         CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTINS   DS    0H                                                               
         MVI   BYTE2,0             FLAG FOR INDICATING DELETED BUYS             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         XC    KEY,KEY             INIT KEY BUILD AREA                          
         MVC   KEY+00(02),AGYALPHA                                              
         MVC   KEY+02(01),BUYMD                                                 
         MVI   KEY+03,X'20'        BUY RECORD ID CODE                           
         MVC   KEY+04(03),BUYCL                                                 
         MVC   KEY+07(03),BUYPR                                                 
         MVC   KEY+10(06),BPUB                                                  
         MVC   KEY+16(03),BINSDT                                                
         MVC   KEY+19(02),BEST                                                  
         MVC   KEY+24(01),BSUBLN                                                
         CLI   KEY+24,0                                                         
         BNE   *+8                                                              
         MVI   KEY+24,1                                                         
*                                                                               
         CLI   MADSW,C'Y'          SCRIPT UPLOAD?                               
         JNE   NXTINS1                                                          
         L     RE,ATHISTMP         POINT TO UPLOAD OBJECT                       
         LA    RE,2(RE)            POINT PASS LENGTH                            
         USING PINSD,RE                                                         
         CLC   =C'DEL',8(RE)       DELETE OBJECT?                               
         JE    *+10                                                             
         MVC   KEY+24(01),PINSLINE USE LINE NUMBER FROM UPLOAD OBJECT           
         DROP  RE                                                               
*                                                                               
NXTINS1  BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   NXTINSX                                                          
         CLI   KEY+25,X'FF'                                                     
         BNE   NXTINS2+4                                                        
         LTR   RE,RE                                                            
         B     NXTINSX                                                          
NXTINS2  BRAS  RE,SEQ                                                           
         CLI   KEY+25,X'FF'                                                     
         BE    NXTINS2                                                          
         CLC   KEY(16),KEYSAVE     TEST SAME THRU PUB                           
         BNE   NXTINSX                                                          
*                                                                               
         CLC   KEY+19(2),BEST      TEST RIGHT EST                               
         BNE   NXTINS2                                                          
         OC    KEY+21(3),KEY+21    TEST ACTIVE                                  
         BNZ   NXTINS2             NO                                           
*                                                                               
         TM    KEY+25,X'80'        DELETED?                                     
         BZ    *+8                                                              
         MVI   BYTE2,C'D'          YES, DELETED                                 
         BRAS  RE,GETREC                                                        
*                                                                               
NXTINS2X DS    0H                  SAVE DATA IN SVINS LIST                      
*                                                                               
NXTINS3  LA    R0,DUMEL                                                         
         C     R0,TRADDR           TEST FOR DUMMY LINE                          
         BE    NXTINSX                                                          
         LA    R1,SVINS            FIND A SLOT                                  
         OC    0(6,R1),0(R1)                                                    
         BZ    *+12                                                             
         LA    R1,6(R1)                                                         
         B     *-14                                                             
         L     R0,TRADDR           GET REL TWA ADDR                             
         SR    R0,RA                                                            
         STH   R0,0(R1)                                                         
NXTINS4  DS    0H                                                               
         MVC   2(4,R1),KEY+27      SAVE DISK ADDRESS                            
         CR    R2,R2               SET CC                                       
*                                                                               
NXTINSX  DS    0H                                                               
         LA    R1,1                PRESERVE CC                                  
         BNZ   *+6                                                              
         SR    R1,R1                                                            
         NI    DMINBTS,X'F7'       RESET DELETES                                
*                                                                               
         LTR   R1,R1                                                            
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   CLI   0(R5),0                                                          
         JE    NEXTELN                                                          
NEXTELL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NEXTELX             EXIT WITH CC EQUAL                           
         CLI   0(R5),0                                                          
         JNE   NEXTELL                                                          
NEXTELN  LTR   R5,R5               SET CC TO NOT EQUAL                          
NEXTELX  BR    R9                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ALLDONE  DS    0H                                                               
         CLC   ACHTR,=C'D '        DELETED BUY BEING DISPLAYED?                 
         BNE   ALLD55                                                           
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(L'DELMSG),DELMSG                                          
         OI    BUYMSGH+6,X'80'                                                  
         B     ALLD75                                                           
*                                                                               
ALLD55   CLI   ACACT,RECALLAC      ADDNTL CHRGS RECALL?                         
         BNE   ALLD60                                                           
         XC    ACHTR,ACHTR         BLANK OUT RECALL TRANSACTION CODE            
         MVI   ACHTR,C'*'                                                       
         OI    ACHTRH+6,X'80'                                                   
*                                                                               
ALLD60   XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(L'CMPMSG),CMPMSG                                          
         OI    BUYMSGH+6,X'80'                                                  
*                                                                               
ALLD75   MVI   ERRAREA,C'K'        FAKE ERROR TO SEND THIS MSG                  
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         B     EXIT                                                             
*                                                                               
CMPMSG   DC    C'** Action completed **'                                        
DELMSG   DC    C'** Deleted BUY with Additional Charges displayed **'           
BPACTEXT DC    C'* Billing/Paying has occurred for this charge code'            
*                                                                               
         EJECT                                                                  
*                                                                               
TESTERR  CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
         ZAP   DUB,LNCTR                                                        
         CVB   RF,DUB                                                           
         STC   RF,HALF2                                                         
*                                                                               
         ZAP   DUB,FLDCTR                                                       
         CVB   RF,DUB                                                           
         STC   RF,HALF2+1                                                       
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHANGEAC EQU   01                  CHANGE ADDITIONAL CHARGES ACTION             
RECALLAC EQU   02                  RECALL ADDITIONAL CHARGES ACTION             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPCLR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ACHCOD1H         POINT TO 1ST FLD ON ADDTNL CHRG LST          
         LA    RF,10               TEN LINES TO BE CLEARED                      
*                                                                               
DSPCLR30 XC    8(02,R2),8(R2)      CLR CODE                                     
         OI    6(R2),X'80'                                                      
         BRAS  R9,DBMPFLD                                                       
         MVI   8(R2),0             CLR ONE CHAR PROTECTED                       
         OI    6(R2),X'80'                                                      
         BRAS  R9,DBMPFLD                                                       
         XC    8(20,R2),8(R2)      CLR DESCRIPTION                              
         OI    6(R2),X'80'                                                      
         BRAS  R9,DBMPFLD                                                       
         XC    8(12,R2),8(R2)      CLR CHARGE                                   
         OI    6(R2),X'80'                                                      
         BRAS  R9,DBMPFLD                                                       
         MVI   8(R2),0             CLR COMMISSION                               
         OI    6(R2),X'80'                                                      
         BRAS  R9,DBMPFLD                                                       
         XC    8(06,R2),8(R2)      CLR COMMISSION PERCENTAGE                    
         OI    6(R2),X'80'                                                      
         BRAS  R9,DBMPFLD                                                       
         MVI   8(R2),0             CLR CASH DISCOUNT                            
         OI    6(R2),X'80'                                                      
         BRAS  R9,DBMPFLD                                                       
         BCT   RF,DSPCLR30                                                      
DSPCLRX  XIT1                                                                   
*                                                                               
DBMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,DBMPFLDS                                                      
         BR    R9                                                               
*                                                                               
DBMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
DBMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DELACEL  NTR1  BASE=*,LABEL=*      DEL ADDTNL CHRGS ELEM FROM BUY REC           
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'         BUY DESCRIPTION ELEM?                        
         BE    *+6                                                              
         DC    H'0'                WORKING WITH WRONG RECORD!                   
         MVI   ELCODE,X'44'                                                     
DELAC30  BRAS  R9,DNXTEL                                                        
         BE    *+8                                                              
         B     DELACX                                                           
         GOTO1 VRECUP,DMCB,(1,REC),0(R5)                                        
         LA    R5,REC+33                                                        
         B     DELAC30             SEARCH FOR MORE                              
DELACX   J     DSPCLRX                                                          
*                                                                               
DNXTEL   CLI   0(R5),0                                                          
         BC    8,DNXTELX                                                        
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                EXIT WITH CC EQUAL                           
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
DNXTELX  LTR   R5,R5               SET CC TO NOT EQUAL                          
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCFXRT  NTR1  BASE=*,LABEL=*      PROCESS FX RATE                              
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,BYCCIDQ                                                   
PRCFXR10 BRAS  R9,NEXTEL                                                        
         JNE   EXIT_XIT                                                         
         USING BYCCELD,R5                                                       
         CLC   =AL2(FXRATEQ),BYCCSQN                                            
         BNE   PRCFXR10                                                         
         ZAP   EXCRATE,BYCCDATA(8)                                              
*                                                                               
PRCFXR16 LA    R5,REC+33                                                        
         MVI   ELCODE,X'44'                                                     
PRCFXR20 BRAS  R9,NEXTEL                                                        
         BNE   PRCFXR30                                                         
         USING PACELEM,R5                                                       
         CLC   =C'FX',PACCODE      FX CHARGE CODE?                              
         BNE   PRCFXR20                                                         
         GOTOR VRECUP,DMCB,(1,REC),0(R5)                                        
         B     PRCFXR16                                                         
*                                                                               
PRCFXR30 GOTOR VGETINS,DMCB,REC,PVALUES,REC+7,(C'F',0),0,0                      
*                                                                               
         L     RF,GROSS            GET GROSS                                    
         CVD   RF,DUB              CVD                                          
         ZAP   PL16,DUB                                                         
         SP    EXCRATE,=P'100000'  DIFFERENCE IN CAD TO USD RATE                
         MP    PL16,EXCRATE        CALCULATE FX AMOUNT                          
         SRP   PL16,64-5,5         ROUND TO 2 DECIMALS                          
*                                                                               
         LA    R5,TEMP1            BUILD AND ADD FX ADDITIONAL CHG ELM          
         XC    TEMP1,TEMP1                                                      
         USING PACELEM,R5          ESTABLISH ADDITIONAL CHARGE ELEMENT          
         MVI   PACELEM,X'44'       SET ELEMENT ID                               
         MVI   PACELEM+1,32        ELEMENT LENGTH                               
         MVC   PACCODE,=C'FX'      ADDITIONAL CHARGE CODE                       
         MVI   PACGN,C'G'          GROSS ENTERED                                
         MVI   PACCD,C'Y'          SUBJECT TO CASH DISCOUNT                     
         MVI   PACAC,C'Y'          COMMISSIONABLE                               
         ZAP   PACAMT,PL16         FX AMOUNT                                    
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'44'        POINT TO 1ST AC ELEM OR END OF REC           
         BRAS  R9,NEXTEL                                                        
         GOTO1 VRECUP,DMCB,(1,REC),TEMP1,(R5)                                   
*                                                                               
EXIT_XIT XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKNUMBER NTR1  BASE=*,LABEL=*      VALIDATING NUMBERS                           
*                                                                               
         CLI   5(R2),11            OUTPUT CAN BE 12, BUT NEVER INPUT            
         BH    CKNUMERR                                                         
*                                                                               
         MVI   WKNUMSW,0           INIT SWITCH                                  
         CLI   BYTE2,C'$'                                                       
         BNE   *+12                                                             
         CLI   5(R2),11            LENGTH IS ARTIFICIAL?                        
         BE    CKNUM15                                                          
         CLI   BYTE2,C'%'                                                       
         BNE   *+12                                                             
         CLI   5(R2),6             LENGTH IS ARTIFICIAL?                        
         BNE   CKNUM25                                                          
*                                                                               
CKNUM15  XC    WKINPUT,WKINPUT                                                  
         MVC   WKINPUT,8(R2)                                                    
         LA    RE,WKINPUT                                                       
         SR    R1,R1               NUMBER OF SPACES ENCOUNTERED                 
         LA    RF,6                                                             
         CLI   BYTE2,C'$'          DALLAR?                                      
         BNE   *+8                                                              
         LA    RF,11                                                            
CKNUM20  CLI   0(RE),0                                                          
         BNE   *+8                                                              
         AHI   R1,1                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+12                                                             
         MVI   0(RE),0             REPLACE SPACE WITH NULL                      
         AHI   R1,1                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,CKNUM20                                                       
         LA    R4,6                                                             
         CLI   BYTE2,C'$'          DALLAR?                                      
         BNE   *+8                                                              
         LA    R4,11                                                            
         SR    R4,R1               NEW LENGTH                                   
         MVI   WKNUMSW,C'Y'        USE WKINPUT AS INPUT FOR VALIDATION          
*                                                                               
CKNUM25  CLI   BYTE2,C'$'          DALLAR?                                      
         BE    CKNUM26                                                          
         CLI   BYTE2,C'%'          PERCENTAGE?                                  
         BE    CKNUM50                                                          
*                                                                               
         DC    H'0'                CURRENTLY NO OTHER OPTIONS USED              
*                                                                               
CKNUM26  CLI   WKNUMSW,C'Y'                                                     
         BE    CKNUM27                                                          
         ZIC   R4,5(R2)            GET INPUT LENGTH                             
CKNUM27  MVI   BYTE2,C'G'          DEFAULT IS GROSS                             
         LA    R5,8(R2)            POINT TO INPUT                               
         CLI   WKNUMSW,C'Y'                                                     
         BNE   *+8                                                              
         LA    R5,WKINPUT                                                       
         CLI   0(R5),C'N'          NET?                                         
         BNE   CKNUM30H                                                         
         MVI   BYTE2,C'N'                                                       
         LA    R5,9(R2)                                                         
         CLI   WKNUMSW,C'Y'                                                     
         BNE   *+8                                                              
         LA    R5,WKINPUT+1                                                     
         BCTR  R4,0                MINUS ONE FOR N OR G CHARACTER               
         B     CKNUM30K                                                         
*                                                                               
CKNUM30H CLI   0(R5),C'G'          GROSS?                                       
         BNE   CKNUM30K                                                         
         LA    R5,9(R2)                                                         
         CLI   WKNUMSW,C'Y'                                                     
         BNE   *+8                                                              
         LA    R5,WKINPUT+1                                                     
         BCTR  R4,0                MINUS ONE FOR NET CHARACTER                  
*                                                                               
CKNUM30K GOTO1 VCASHVAL,DMCB,(2,(R5)),(R4)                                      
         CLI   0(R1),X'FF'                                                      
         BNE   CKNUM30R                                                         
         GOTO1 VCASHVAL,DMCB,(1,(R5)),(R4)                                      
         CLI   0(R1),X'FF'                                                      
         BE    CKNUM30P                                                         
         L     R4,4(R1)                                                         
         CVD   R4,DUB                                                           
         CP    DUB,=P'99999999'    MAX IS 9,999,999.9 FOR ONE DECIMAL           
         BH    CKNUMERR                                                         
         CP    DUB,=P'-99999999'   NEGATIVE MAX?                                
         BL    CKNUMERR                                                         
         MHI   R4,10                                                            
         B     CKNUM30U                                                         
CKNUM30P GOTO1 VCASHVAL,DMCB,(0,(R5)),(X'40',(R4))                              
         CLI   0(R1),X'FF'                                                      
         BE    CKNUMERR            BAD NUMBER                                   
         L     R4,4(R1)                                                         
         CVD   R4,DUB                                                           
         CP    DUB,=P'9999999'     MAX IS 9,999,999 FOR ZERO DECIMAL            
         BH    CKNUMERR                                                         
         CP    DUB,=P'-9999999'    NEGATIVE MAX?                                
         BL    CKNUMERR                                                         
         MHI   R4,100                                                           
         B     CKNUM30U                                                         
*                                                                               
CKNUM30R L     R4,4(R1)                                                         
CKNUM30U CVD   R4,DUB                                                           
         CP    DUB,=P'999999999'   MAX IS 9,999,999.99 FOR PL5                  
         BH    CKNUMERR                                                         
         CP    DUB,=P'-999999999'  NEGATIVE MAX?                                
         BL    CKNUMERR                                                         
*                                                                               
         B     CKNUMX                                                           
*                                                                               
CKNUM50  LA    R5,8(R2)                                                         
         CLI   WKNUMSW,C'Y'                                                     
         BNE   *+12                                                             
         LA    R5,WKINPUT                                                       
         B     CKNUM50H                                                         
         ZIC   R4,5(R2)            LENGTH OF INPUT                              
CKNUM50H GOTO1 VCASHVAL,DMCB,(3,(R5)),(R4)                                      
         CLI   0(R1),X'FF'                                                      
         BNE   CKNUM50R                                                         
         GOTO1 VCASHVAL,DMCB,(2,(R5)),(R4)                                      
         CLI   0(R1),X'FF'                                                      
         BE    CKNUM50M                                                         
         L     R4,4(R1)                                                         
         MHI   R4,10                                                            
         B     CKNUM30U                                                         
CKNUM50M GOTO1 VCASHVAL,DMCB,(1,(R5)),(R4)                                      
         CLI   0(R1),X'FF'                                                      
         BE    CKNUM50P                                                         
         L     R4,4(R1)                                                         
         MHI   R4,100                                                           
         B     CKNUM50U                                                         
CKNUM50P GOTO1 VCASHVAL,DMCB,(0,(R5)),(X'40',(R4))                              
         CLI   0(R1),X'FF'                                                      
         BE    CKNUMERR            BAD NUMBER                                   
         L     R4,4(R1)                                                         
         MHI   R4,1000                                                          
         B     CKNUM50U                                                         
*                                                                               
CKNUM50R L     R4,4(R1)                                                         
CKNUM50U CVD   R4,DUB                                                           
         CP    DUB,=P'0'                                                        
         BL    CKNUMERR                                                         
         CP    DUB,=P'100000'      MAX IS 100.000 (100%)                        
         BNH   CKNUMX                                                           
*                                                                               
CKNUMX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKNUMERR LTR   RB,RB               NOT EQUAL (ERROR, INVALID NUMBER)            
         J     DSPCLRX                                                          
*                                                                               
         DROP  R6                  FOR SCREEN LABLE USINGS                      
         DROP  R7                  FOR WORKING STORAGE USINGS                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECKS FOR MATCHED AND PAID INSERTIONS, IF IT IS MATCHED & NOT PAID           
* THEN NO ADDING AND CHANGING TO ADDITION CHARGES ARE ALLOWED                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMATPD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   MATSW,0             INITIALIZE SWITCH                            
         CLI   REC+33,X'20'        BUY DESCRIPTION ELEM?                        
         BE    *+6                                                              
         DC    H'0'                WORKING WITH WRONG RECORD!                   
*                                                                               
         LA    R5,REC+33           FIRST ELEM IS ALWAYS X'20'                   
         USING PBDELEM,R5                                                       
         TM    PBDSTAT,X'40'       MATCHED TO INVOICE?                          
         BZ    CKMPEQ              YES                                          
         OI    MATSW,X'40'         MATCHED                                      
         DROP  R5                                                               
*                                                                               
         MVI   ELCODE,X'25'                                                     
CKMP30   BRAS  R9,MPNXTEL          PAID ELEMENT FOUND?                          
         BNE   CKMP50              NO OR NO MORE TO BE FOUND                    
         OC    2(3,R5),2(R5)       PAID?                                        
         BZ    CKMP30              NO, CHECK NEXT PAID ELEMENT                  
         OI    MATSW,X'80'         PAID                                         
*                                                                               
CKMP50   TM    MATSW,X'C0'         MATCH AND PAID?                              
         BM    CKMPNEQ             MIXED, NO CHANGES TO CHRGS ALLOWED           
*                                                                               
CKMPEQ   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKMPNEQ  LTR   RB,RB               NOT EQUAL (ERROR)                            
         J     DSPCLRX                                                          
*                                                                               
MPNXTEL  CLI   0(R5),0                                                          
         BC    8,MPNXTELX                                                       
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                EXIT WITH CC EQUAL                           
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
MPNXTELX LTR   R5,R5               SET CC TO NOT EQUAL                          
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
*                                                                               
TSTLK2   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK3                                                           
*                                                                               
TSTLK4   XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
         MVC   L.LOCKPUB,REC+10    PACKED BASE PUB NUMBER                       
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
TSTLK5   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLKEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
TSTLKEQ  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB               NOT EQUAL                                    
         J     DSPCLRX                                                          
*                                                                               
         DROP  L                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLKWKA DS    0H                                                               
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WKT41116 DSECT                     WORKING STORAGE AREA DSECT                   
*                                                                               
WKAREA   DS    0H                  BEGINNING OF WORKING STORAGE AREA            
*                                                                               
WKELEM   DS    CL255               FOR BUILDING ELEMS OR OTHER USES             
WKKEY    DS    CL32                WORKING STORAGE KEY                          
WKCODE   DS    CL2                                                              
WKPACK   DS    PL12                                                             
WKDUB    DS    D                                                                
WKDUB2   DS    D                                                                
WKBYTE   DS    C                                                                
WKSAVER  DS    F                   FOR TEMPORARY REGISTER SAVES                 
WKSVDSCH DS    F                   SAVE REGISTER FOR DISP/CHANGE LOGIC          
ACTABPTR DS    F                   ADDTNL CHRGS TABLE POINTER                   
FFLDADDR DS    F                   TO SAVE ADDRESS OF FIRST FLD ON SCR          
ERRSW    DS    C                   GENERAL SWITCH USED TO CONTROL ERRS          
DMGSW    DS    C                   DATA MGR RESTORING REC SWITCH                
BPACSW   DS    C                   BILL/PAY CHARGES SWITCH                      
*                                                                               
CHGSW    DS    C                                                                
NEWCCSCR EQU   X'80'               FIRST TR CODE IS CC                          
DATECHGD EQU   X'40'               DATE FLD HAS BEEN CHANGED                    
*                                                                               
ACACT    DS    C                   ADDTNL CHRGS ACTION (CHANGE/RECALL)          
WKCOUNT1 DS    H                   FOR LOOP CONTROLS                            
WKCOUNT2 DS    H                                                                
SVAGYCOM DS    PL4                 TO SAVE AGY COMMISSION (PBDAC)               
*                                                                               
ACELTAB  DS    10CL32              TEN LINES OF 32 BYTES ELEM                   
ACELTABX DS    X                   END OF TABLE MARKER                          
ACELTABQ EQU   32                  LEN OF ONE ENTRY IN ADDTNL CHRGS TAB         
*                                                                               
ACCODES  DS    10CL2               TABLE OF ADDTNL CHRGS (10 MAX)               
ACCODESX DS    X                   END OF TABLE MARKER                          
*                                                                               
LNCTR    DS    PL2                 LINE COUNTER FOR PBU ERRORS                  
FLDCTR   DS    PL2                 FIELD COUNTER FOR PBU ERRORS                 
*                                                                               
TEMP1    DS    XL256                                                            
TEMP2    DS    XL256                                                            
*                                                                               
EXCRATE  DS    PL8                 EXCHANGE RATE                                
FXRATEQ  EQU   X'200F'             FXRATE STANDARD CUSTOM COLUMN CODE           
PL16     DS    PL16                WORK PACKED FIELD                            
*                                                                               
WKAREAX  EQU   *-WKAREA            LENGTH OF WORKING AREA                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SVSCREEN DS    0H                                                               
         DS    10CL(SVSCRLQ)       21 INPUT CHARS PER LINES (10 LINES)          
SVSCREEQ EQU   *-SVSCREEN                                                       
SVSCRLQ  EQU   22                  LENGTH OF ONE ENTRY                          
*                                                                               
SVACTB   DS    10CL32              TEN LINES OF 32 BYTES ELEM (SAVE)            
SVACTBX  DS    X                   END OF TABLE MARKER                          
SVACTBQ  EQU   32                  LEN OF ONE ENTRY IN ADDTNL CHRGS TAB         
*                                                                               
WKINPUT  DS    CL11                STORAGE FOR INPUT FROM SCREEN                
WKNUMSW  DS    CL1                 SWITCH                                       
SVSCRDAT DS    CL8                 SAVE DATE ON SCREEN                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PPBUYWRK2                                                      
         PRINT ON                                                               
*                                                                               
PPECD    DSECT                     ADDITIONAL CHARGES LOWER BUY SCR             
       ++INCLUDE PPBUYECD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086PPBUY16   04/20/16'                                      
         END                                                                    
