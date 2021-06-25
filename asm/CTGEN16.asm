*          DATA SET CTGEN16    AT LEVEL 003 AS OF 08/22/00                      
*PHASE TA0B16A                                                                  
*INCLUDE DYNALLOC                                                               
*INCLUDE GEUP                                                                   
         TITLE 'CTGEN16 - FINSTAT UPDATE - EXCHANGE RECORDS'                    
GEN16    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN16*,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         L     R2,AIOAREA1                                                      
         USING GEXCD,R2            R2=A(RECORD)                                 
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         L     R1,=V(GEUP)                                                      
         A     R1,APRELO                                                        
         ST    R1,VGEUP                                                         
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         MVI   INWHEN,MIXIOKS      FORCE SOON                                   
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPDSNH       VALIDATE DSN IF INPUT                        
         BE    VRQ20                                                            
         MVC   REPDSN(30),DDSNAME                                               
         MVC   DSNAME,DDSNAME                                                   
         OI    REPDSNH+1,X'01'     MODIFY                                       
         MVI   REPDSNH+5,X'1E'     CL30                                         
         B     VRQ30                                                            
VRQ20    MVC   DSNAME,FVIFLD                                                    
*                                                                               
VRQ30    MVC   REPDESC,REPDESCL                                                 
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
****************************************************                            
* ROUTINE TO GENERATE FINSTAT EXTRACT UPDATE       *                            
****************************************************                            
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(X'FF',=C'FTCURRY '),DSNAME                    
INIT010  OPEN  (FTCURRY,INPUT)                                                  
*                                                                               
         XC    GEUPBLK(GEUPBLKL),GEUPBLK                                        
         MVC   GEUPDMGR,VDMGR      INITIALISE GEUPBLK                           
         MVC   GEUPHELO,VHELLO                                                  
         LA    R0,GEUPBUF                                                       
         ST    R0,GEUPABUF                                                      
         L     R0,AIOAREA1                                                      
         ST    R0,GEUPAREC                                                      
         MVC   GEUPIUSR,TWAUSRID                                                
         MVI   GEUPISYS,X'0A'                                                   
         MVI   GEUPIPGM+0,C'E'     EXCHANGE RECORD TYPE                         
         MVI   GEUPIPGM+1,GEUPOREQ                                              
         MVC   GEUPIDAY,ASBDAT+2                                                
         GOTO1 GEUP,GEUPINI        INITIALISE NEW WORKER FILE                   
         SPACE 1                                                                
****************************************************                            
*   LOCATE CURRENCY ID AND VALIDATE FEED DATE      *                            
****************************************************                            
         SPACE 1                                                                
         MVI   MODE,1              LOCATE START OF FEED & DATE                  
GETCURRY GET   FTCURRY                                                          
         CLC   CURRID,0(R1)                                                     
         BNE   GETCURRY                                                         
         MVC   DUB,7(R1)           MOVE DD/MM/YY TO DUB                         
*                                                                               
         GOTO1 VDATCON,DMCB,(X'04',DUB),(X'02',DATE)                            
         OC    DATE,DATE                                                        
         BZ    ERR2                INVALID DATE                                 
*                                                                               
         XC    COUNT,COUNT                                                      
         XC    XCOUNT,XCOUNT                                                    
         XC    ECOUNT,ECOUNT                                                    
         EJECT                                                                  
****************************************************                            
*   VALIDATE EXCHANGE ENTRY, GET FROM CUR TO CUR   *                            
*   AND CALCULATE POSITION IN CLOSING RATE TABLE   *                            
****************************************************                            
         MVI   MODE,2              READ EXCHANGE RATES                          
CURRY1   GET   FTCURRY                                                          
         LR    R3,R1                                                            
         ST    R1,ADDR                                                          
         MVC   FTDATA,0(R1)                                                     
         CLC   0(8,R3),EOFEED      END OF FEED                                  
         BE    EOFCURRY                                                         
         CLC   0(2,R3),=C'F0'      ALL RATES START WITH F0                      
         LA    R3,2(R3)                                                         
         BNE   ERR3                NO 'F0' NO COMMENT                           
         LA    R8,CURTAB                                                        
         CLI   0(R3),C'P'          FROM POUNDS STERLING                         
         BNE   CURR010                                                          
         MVC   CURF,POUND                                                       
         LA    R3,1(R3)                                                         
         LA    R4,PCLOSE                                                        
         B     CURR100                                                          
CURR010  CLI   0(R3),C'D'          FROM US DOLLARS                              
         BNE   CURR020                                                          
         MVC   CURF,DOLLAR                                                      
         LA    R3,1(R3)                                                         
         LA    R4,DCLOSE                                                        
         B     CURR100                                                          
CURR020  CLC   0(3,R3),ECU         FROM ECUS                                    
         BNE   CURR030                                                          
         MVC   CURF,ECU                                                         
         LA    R3,3(R3)                                                         
         LA    R4,ECLOSE                                                        
         B     CURR100                                                          
CURR030  CLI   0(R3),C'E'          EUROCURRENCY INTEREST RATES                  
         BE    REJECT                                                           
         B     ERR3                                                             
*                                                                               
CURR100  CLC   0(3,R3),0(R8)       TEST 3 CHR FINSTAT CODE                      
         BE    CURR110                                                          
         LA    R8,L'CURTAB(R8)                                                  
         CLI   0(R8),0                                                          
         BNE   CURR100                                                          
         B     ERR4                CURRENCY NOT IN TABLE                        
*                                                                               
CURR110  CLC   3(3,R8),=C'   '     TEST NON EQUATED CURENCY                     
         BE    REJECT                                                           
         MVC   CURT,3(R8)          GET TO CURRENCY (DDS CODE)                   
         SR    R1,R1                                                            
         ICM   R1,1,6(R8)          GET CLOSE TABLE INDEX                        
         BNM   *+12                                                             
         LA    R4,WORK             X'FF' MEANS NO TABLE ENTRY                   
         B     CURR11X                                                          
         MH    R1,=H'12'           CALCULATE TRUE INDEX                         
         LA    R4,0(R1,R4)         INDEX INTO CLOSE TABLE                       
CURR11X  LA    R3,3(R3)            BUMP ONTO NEXT FIELD                         
         EJECT                                                                  
****************************************************                            
*   TEST IF SINGLE RATE OR BID & OFFER RATES       *                            
*   IF BID & OFFER GET OFFER RATE, VALIDATE IT     *                            
*   AND SHIFT BID PRICE INTO SAVE REC              *                            
****************************************************                            
         SPACE 1                                                                
CURR200  XC    SAVEREC,SAVEREC                                                  
         CLI   0(R3),C'B'          IS THIS A BID PRICE                          
         BNE   CURR300             NO SO SINGLE RATE ONLY                       
*                                                                               
         MVI   0(R3),C'*'          REPLACE WITH * FOR COMPARE                   
         L     R1,ADDR                                                          
         LR    R0,R3                                                            
         SR    R0,R1                                                            
         MVC   SAVEREC,0(R1)       SAVE BID PRICE REC                           
         GET   FTCURRY             GET OFFER PRICE REC                          
         LR    R3,R1                                                            
         ST    R1,ADDR                                                          
         AR    R3,R0                                                            
         CLI   0(R3),C'O'          TEST THIS IS AN OFFER                        
         BNE   ERR5                                                             
         MVI   0(R3),C'*'          REPLACE WITH * FOR COMPARE                   
*                                                                               
CURR201  LA    R3,1(R3)            FIND ','                                     
         CLI   0(R3),C' '                                                       
         BE    ERR9                                                             
         CLI   0(R3),C','                                                       
         BNE   CURR201                                                          
         SR    R3,R1                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SAVEREC     TEST RECORDS ARE IDENTICAL UP TO ','         
         BNE   ERR5                                                             
*                                                                               
         LA    R1,SAVEREC(R3)                                                   
         MVC   SAVEREC(20),1(R1)   SHIFT BID PRICE INTO SAVEREC                 
         L     R3,ADDR                                                          
         AR    R3,R0               POINT R3 TO '*' IN OFFER REC                 
         LA    R3,1(R3)                                                         
*                                                                               
CURR300  CLI   0(R3),C','          IF ',' THIS IS CLOSING RATE                  
         BE    CLOS010                                                          
         B     FORD010             ELSE MUST BE A FORWARD RATE                  
         EJECT                                                                  
****************************************************                            
*   TRANSLATE EXCHANGE RATES INTO GENFIL FORMAT.   *                            
*   INSERT CLOSING RATES INTO CLOSE TABLE FOR      *                            
*   USE IN FORWARD AND CROSS RATE CALCULATION.     *                            
****************************************************                            
         SPACE 1                                                                
CLOS010  MVI   TYPE,C'C'                                                        
         LA    R3,1(R3)            DROP ','                                     
         GOTO1 VALNUM,0(R3)                                                     
         BNER  R1                  IF CC NEQ R1=ERROR                           
         MVC   0(6,R4),DUB+2                                                    
         LA    R3,SAVEREC                                                       
         CLI   0(R3),0                                                          
         BNE    *+14                                                            
         MVC   6(6,R4),0(R4)       COPY RATE FOR SINGLE                         
         B     CLOS020                                                          
         GOTO1 VALNUM,0(R3)                                                     
         BNER  R1                  IF CC NEQ R1=ERROR                           
         MVC   6(6,R4),DUB+2                                                    
*                                                                               
CLOS020  MVC   WORK(12),0(R4)      MOVE RATES TO WORK                           
         B     ADDRATE                                                          
         SPACE 2                                                                
****************************************************                            
*   TRANSLATE FORWARD RATE TYPES                   *                            
****************************************************                            
         SPACE 1                                                                
FORD010  CLC   0(2,R3),=C'1M'                                                   
         BNE   *+12                                                             
         MVI   TYPE,C'1'                                                        
         B     FORD020                                                          
         CLC   0(2,R3),=C'3M'                                                   
         BNE   *+12                                                             
         MVI   TYPE,C'3'                                                        
         B     FORD020                                                          
         CLC   0(2,R3),=C'6M'                                                   
         BNE   *+12                                                             
         MVI   TYPE,C'6'                                                        
         B     FORD020                                                          
         B     REJECT                                                           
*                                                                               
FORD020  CLI   2(R3),C','                                                       
         BNE   REJECT                                                           
         LA    R3,3(R3)                                                         
         MVI   FLAG,C'+'           SET +VE OR -VE RATE                          
         CLI   0(R3),C'-'                                                       
         BNE   *+12                                                             
         MVI   FLAG,C'-'           FORWARD RATE IS NEGATIVE                     
         LA    R3,1(R3)                                                         
         GOTO1 VALNUM,0(R3)                                                     
         BNER  R1                  IF CC NEQ R1=ERROR                           
         CLC   CURT,YEN                                                         
         BE    FORD021             YEN AND LIRA FORWARD RATES                   
         CLC   CURT,ILR                                                         
         BE    FORD021             HAVE NO DECIMAL PLACES                       
         SRP   DUB+2(6),62,0       CONVERT ALL THE OTHERS                       
FORD021  CLI   FLAG,C'-'                                                        
         BNE   *+12                                                             
         NI    DUB+7,X'F0'                                                      
         OI    DUB+7,X'0D'         SET TO NEGATIVE                              
         AP    DUB+2(6),0(6,R4)    ADD TO CLOSING RATE                          
         MVC   WORK(6),DUB+2                                                    
*                                                                               
         LA    R3,SAVEREC                                                       
         CLI   0(R3),0                                                          
         BNE   *+14                                                             
         MVC   WORK+6(6),WORK      COPY RATE FOR SINGLE                         
         B     FORD030                                                          
         MVI   FLAG,C'+'           SET +VE OR -VE RATE                          
         CLI   0(R3),C'-'                                                       
         BNE   *+12                                                             
         MVI   FLAG,C'-'           FORWARD RATE IS NEGATIVE                     
         LA    R3,1(R3)                                                         
         GOTO1 VALNUM,0(R3)                                                     
         BNER  R1                  IF CC NEQ R1=ERROR                           
         CLC   CURT,YEN                                                         
         BE    FORD022             YEN AND LIRA FORWARD RATES                   
         CLC   CURT,ILR                                                         
         BE    FORD022             HAVE NO DECIMAL PLACES                       
         SRP   DUB+2(6),62,0       CONVERT ALL THE OTHERS                       
FORD022  CLI   FLAG,C'-'                                                        
         BNE   *+12                                                             
         NI    DUB+7,X'F0'                                                      
         OI    DUB+7,X'0D'         SET TO NEGATIVE                              
         AP    DUB+2(6),6(6,R4)    ADD TO CLOSING RATE                          
         MVC   WORK+6(6),DUB+2                                                  
FORD030  B     ADDRATE                                                          
         EJECT                                                                  
****************************************************                            
*   BUILD GENDIR KEY                               *                            
****************************************************                            
         SPACE 1                                                                
ADDRATE  AP    WORK(6),WORK+6(6)   ADD BID AND OFFER RATES                      
         ZAP   TEMP(7),WORK(6)                                                  
         DP    TEMP(7),=P'2'       DIVIDE BY 2 FOR AVG RATE                     
         CLC   CURF,POUND                                                       
         BNE   *+16                                                             
         CLC   CURT,ECU                                                         
         BE    ADRT0               $ST TO ECU REVERSE RATE                      
         CLC   CURF,DOLLAR                                                      
         BNE   ADRT1                                                            
         CLC   CURT,POUND          US TO $ST REVERSE RATE                      
         BE    ADRT0                                                            
         CLC   CURT,IRPOUND        US TO IR$ REVERSE RATE                      
         BNE   ADRT1                                                            
*                                                                               
ADRT0    ZAP   WORK(13),INVERT     TAKE RECIPRICAL                              
         DP    WORK(13),TEMP(6)                                                 
         MVC   TEMP(7),WORK+1                                                   
*                                                                               
ADRT1    MVC   XRATE,TEMP                                                       
         GOTO1 GETSHFT             GET SHIFT VALUE                              
         BNE   ERR10               AND CHECK FOR SILLY RATE                     
*                                                                               
         L     R2,AIOAREA1         BUILD GENFIL REC AT IOAREA+2                 
         LA    R2,2(R2)                                                         
*                                                                               
         XC    GEKEY(GEFIRST+GEXELLQ),GEKEY                                     
         MVI   GEKREC,GEKRECQ      KEY VALUES                                   
         MVI   GEKSYS,X'FF'                                                     
         MVC   GEKCURF,CURT        REVERSE RATES FOR DDS                        
         MVC   GEKCURT,CURF        IE PGER  =  DMK TO $ST                       
         MVC   GEKCTYP,TYPE                                                     
         MVC   GEKPSTA,DATE        FROM THIS DATE                               
         MVC   GEKPEND,FFS         UNTIL FURTHER NOTICE                         
*                                                                               
         MVC   GERLEN,RECLEN       RECORD VALUES                                
         MVI   GESTAT,X'01'        RATES ARE REVERSED                           
*                                                                               
         MVI   GEXEL,GEXELQ        ELEMENT VALUES                               
         MVI   GEXELL,GEXELLQ                                                   
         MVC   GEXRATE,XRATE                                                    
         MVC   GEXSHFT,XSHFT                                                    
*                                                                               
         SR    RF,RF               INSERT WKFILE LENGTH                         
         ICM   RF,3,RECLEN                                                      
         LA    RF,2(RF)                                                         
         L     R2,AIOAREA1                                                      
         STCM  RF,3,0(R2)                                                       
         GOTO1 GEUP,GEUPADD        ADD RECORD TO WKFILE                         
*                                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(R1)            UPDATE COUNTER                               
         STH   R1,COUNT                                                         
*                                                                               
***      GOTO1 VHEXOUT,DMCB,(R2),PRTLINE,53                                     
***      GOTO1 VREPORT,REPD                                                     
*                                                                               
         B     CURRY1              GET ANOTHER ONE                              
         EJECT                                                                  
****************************************************                            
*   END OF FILE ON CURRENCY FEED                   *                            
****************************************************                            
         SPACE 1                                                                
EOFCURRY CLI   MODE,1                                                           
         BE    ERR1                CURRENCY ID NOT FOUND                        
         CLOSE (FTCURRY)                                                        
         LA    R1,GEUPBUF                                                       
         GOTO1 GEUP,GEUPCLO                                                     
         OC    ECOUNT,ECOUNT                                                    
         BNZ   EOF01                                                            
*                                                                               
         L     RF,REPAMST                                                       
         USING MASTD,RF            SET WORKER FILE KEY FOR PQ CLOSE             
         OC    MCREMPQK,MCREMPQK   TEST SOON JOB                                
         BZ    *+10                                                             
         MVC   MCREPPQI,GEUPPKEY                                                
         DROP  RF                                                               
*                                                                               
EOF01    MVC   PRTLINE(L'ENDMSG),ENDMSG                                         
         LH    R0,COUNT            NUMBER OF RECORDS UPLOADED                   
         CVD   R0,DUB                                                           
         UNPK  PRTLINE(4),DUB(8)                                                
         OI    PRTLINE+3,X'F0'                                                  
*                                                                               
         OC    ECOUNT,ECOUNT                                                    
         BNZ   *+14                                                             
         MVC   PRTLINE+L'ENDMSG(9),=C'NO ERRORS'                                
         B     EOFX                                                             
         LH    R0,ECOUNT           NUMBER OF ERRORS                             
         CVD   R0,DUB                                                           
         UNPK  PRTLINE+L'ENDMSG(4),DUB(8)                                       
         OI    PRTLINE+L'ENDMSG+3,X'F0'                                         
         MVC   PRTLINE+L'ENDMSG+5(6),=C'ERRORS'                                 
*                                                                               
EOFX     GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         SPACE 2                                                                
GEUP     LR    R0,RE                                                            
         STC   R1,GEUPACTN                                                      
         GOTO1 VGEUP,GEUPBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
GEUPX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
****************************************************                            
*   ROUTINE TO VALIDATE NUMERIC FIELDS             *                            
*   IN THE FORMAT 99999.999999Q    Q=CHKSUM        *                            
****************************************************                            
         SPACE 1                                                                
VALNUM   NTR1                                                                   
         ST    R1,FULL             SAVE A(NUMBER)                               
         B     *+8                                                              
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '          FIND END                                     
         BNE   *-8                                                              
         BCTR  R1,0                SUB 1 FOR CHKSUM                             
         MVI   0(R1),C' '          REMOVE CHKSUM                                
         BCTR  R1,0                SUB 1 FOR '.'                                
         L     RF,FULL                                                          
         SR    R1,RF               R1 = NUMBER OF DIGITS                        
         SR    RE,RE                                                            
VALN01   CLI   0(RF),C'.'          SCAN FOR '.'                                 
         BE    VALN02                                                           
         CLI   0(RF),C' '          OR FIRST SPACE                               
         BE    VALN01A                                                          
         TM    0(RF),X'F0'         ALL ELSE MUST BE NUMERIC                     
         BNO   VALNOTN                                                          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)            COUNT SIGNIFICANT DIGITS                     
         B     VALN01                                                           
*                                                                               
VALN01A  LA    R1,1(R1)            IF NO '.' USE REAL LENGTH                    
*                                                                               
VALN02   STC   R1,BYTE             SAVE LEN IN BYTE                             
         CH    RE,=H'5'            CHECK MAX SIGNIFICANT DIGITS                 
         BH    VALNTOG                                                          
         SR    R1,RE               R1=NUM OF DEC PLACES                         
         STH   R1,HALF                                                          
         BZ    VALN02A                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RF)       SQUASH OUT DEC POINT                         
         LA    R1,1(R1)                                                         
VALN02A  LA    RE,6                                                             
         SR    RE,R1               CALCULATE                                    
         BM    VALNDCP                                                          
         SLL   RE,2                SHIFT VALUE                                  
*                                                                               
         L     RF,FULL                                                          
         IC    R1,BYTE                                                          
         BCTR  R1,0                                                             
         EX    R1,*+16                                                          
         EX    R1,*+18                                                          
         EX    R1,*+20                                                          
         B     *+22                                                             
         MVC   TEMP(0),0(RF)                                                    
         NC    TEMP(0),NUMERIC                                                  
         CLC   TEMP(0),NUMERIC     CHECK NUMERIC                                
         BNE   VALNOTN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),0(0,RF)      PACK NUMBER                                  
         LM    R0,R1,DUB           LOAD INTO R0,R1                              
         SRDL  R0,4                LOSE SIGN BITS                               
         SLDL  R0,4                                                             
         SLDL  R0,0(RE)            SHIFT CORRECT AMOUNT                         
         STM   R0,R1,DUB           AND STORE BACK                               
         OI    DUB+7,X'0C'         ADD SIGN BITS                                
         LH    R1,HALF                                                          
         CR    RB,RB                                                            
         B     VALNX                                                            
*                                                                               
VALNOTN  LA    R1,ERR8                                                          
         B     VALNERR                                                          
VALNTOG  LA    R1,ERR6                                                          
         B     VALNERR                                                          
VALNDCP  LA    R1,ERR7                                                          
*                                                                               
VALNERR  LTR   RB,RB                                                            
*                                                                               
VALNX    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
**********************************************************************          
* CALCULATE SHIFT VALUE FROM DEC PLACES IN CURRENCY RECS             *          
**********************************************************************          
         SPACE 1                                                                
GETSHFT  NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING GCURD,R2                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ      RECORD TYPE CURRENCY                         
         MVC   GCKCURR,SPACES                                                   
         MVC   GCKCURU,CURT                                                     
         LA    R1,IORDD+IOGENDIR+IO2                                            
         GOTO1 AIO                 READ TO CURRENCY RECORD                      
         BE    *+6                                                              
         DC    H'0'                CURRENCY MUST EXIST                          
         LA    R1,IOGET+IOGENFIL+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA2                                                      
         MVC   HALF(1),GCRDECP                                                  
         XC    TEMP(12),TEMP                                                    
         MVC   TEMP+3(5),GCRMNEXC  EXTRACT MIN MAX RATES                        
         MVI   TEMP+11,X'0C'                                                    
         XC    TEMP+18(12),TEMP+18                                              
         MVC   TEMP+21(5),GCRMXEXC                                              
         MVI   TEMP+29,X'0C'                                                    
         LA    R2,IOKEY                                                         
         MVC   GCKCURU,CURF                                                     
         LA    R1,IORDD+IOGENDIR+IO2                                            
         GOTO1 AIO                 READ FROM CURRENCY RECORD                    
         BE    *+6                                                              
         DC    H'0'                CURRENCY MUST EXIST                          
         LA    R1,IOGET+IOGENFIL+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA2                                                      
         MVC   HALF+1(1),GCRDECP                                                
         MVC   WORK(5),GCRMNEXC    EXTRACT MIN MAX RATES                        
         MVI   WORK+5,X'0C'                                                     
         ZAP   TEMP+30(6),WORK(6)                                               
         MVC   WORK(5),GCRMXEXC                                                 
         MVI   WORK+5,X'0C'                                                     
         ZAP   TEMP+12(6),WORK(6)                                               
         SR    RE,RE               CALCULATE SHIFT VALUE                        
         SR    RF,RF                                                            
         IC    RE,HALF                                                          
         IC    RF,HALF+1                                                        
         SR    RF,RE                                                            
         STC   RF,XSHFT            PUT VALUE IN XSHFT                           
         DP    TEMP+0(12),TEMP+12(6)                                            
         DP    TEMP+18(12),TEMP+30(6)                                           
         MVC   WORK(5),XRATE                                                    
         MVI   WORK+5,X'0C'                                                     
         CP    WORK(6),TEMP(6)     CHECK MIN RATE                               
         BL    GETSHFTX                                                         
         CP    WORK(6),TEMP+18(6)  CHECK MAX RATE                               
         BH    GETSHFTX                                                         
         CR    RB,RB                                                            
*                                                                               
GETSHFTX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
****************************************************                            
*   ERROR EXITS                                    *                            
****************************************************                            
         SPACE 1                                                                
ERR1     MVC   PRTERR(L'ERROR1),ERROR1                                          
         B     ERRXX                                                            
ERR2     MVC   PRTERR(L'ERROR2),ERROR2                                          
         B     ERRX                                                             
ERR3     MVC   PRTERR(L'ERROR3),ERROR3                                          
         B     ERRX                                                             
ERR4     MVC   PRTERR(L'ERROR4),ERROR4                                          
         B     ERRX                                                             
ERR5     MVC   PRTERR(L'ERROR5),ERROR5                                          
         B     ERRX                                                             
ERR6     MVC   PRTERR(L'ERROR6),ERROR6                                          
         B     ERRX                                                             
ERR7     MVC   PRTERR(L'ERROR7),ERROR7                                          
         B     ERRX                                                             
ERR8     MVC   PRTERR(L'ERROR8),ERROR8                                          
         B     ERRX                                                             
ERR9     MVC   PRTERR(L'ERROR9),ERROR9                                          
         B     ERRX                                                             
ERR10    MVC   PRTERR(L'ERROR10),ERROR10                                        
         B     ERRX                                                             
REJECT   LH    R1,XCOUNT           UPDATE REJECT COUNT                          
         LA    R1,1(R1)                                                         
         STH   R1,XCOUNT                                                        
         B     CURRY1              CARRY ON                                     
*                                                                               
ERRX     LH    R1,ECOUNT           UPDATE ERROR COUNT                           
         LA    R1,1(R1)                                                         
         STH   R1,ECOUNT                                                        
         MVC   PRTFT,FTDATA                                                     
         GOTO1 VREPORT,REPD                                                     
***      GOTO1 VHEXOUT,DMCB,(R2),PRTLINE,53                                     
***      GOTO1 VREPORT,REPD                                                     
*                                                                               
         B     CURRY1                                                           
*                                                                               
ERRXX    LH    R1,ECOUNT           UPDATE ERROR COUNT                           
         LA    R1,1(R1)                                                         
         STH   R1,ECOUNT                                                        
         MVI   MODE,2                                                           
         B     EOFCURRY            GO STRAIGHT TO EOF                           
         EJECT                                                                  
****************************************************                            
*   ERRORS                                         *                            
****************************************************                            
         SPACE 1                                                                
ERROR1   DC    C'***ERROR***  FINSTAT CURRENCY ID NOT FOUND'                    
ERROR2   DC    C'***ERROR***  INVALID DATE ON CURRENCY FEED'                    
ERROR3   DC    C'***ERROR***  UNKNOWN FROM CURRENCY'                            
ERROR4   DC    C'***ERROR***  UNKNOWN TO CURRENCY'                              
ERROR5   DC    C'***ERROR***  BID PRICE NOT FOLLOWED BY OFFER PRICE'            
ERROR6   DC    C'***ERROR***  RATE VALUE TOO LARGE'                             
ERROR7   DC    C'***ERROR***  TOO MANY DECIMAL PLACES'                          
ERROR8   DC    C'***ERROR***  NON NUMERIC DATA IN RATE'                         
ERROR9   DC    C'***ERROR***  MISSING ","'                                      
ERROR10  DC    C'***ERROR***  RATE IS OUT OF RANGE'                             
ENDMSG   DC    C'XXXX EXCHANGE RATES UPLOADED '                                 
         EJECT                                                                  
****************************************************                            
*   CURRENCY CONVERSION TABLE                      *                            
*   FINSTAT CODE / DDS CODE / CLOSE TABLE INDEX    *                            
****************************************************                            
         SPACE 1                                                                
CURTAB   DS    0CL7                                                             
         DC    C'UKM',C'$ST',X'00'       POUNDS STERLING                        
         DC    C'USA',C'US',X'00'       UNITED STATES DOLLARS                  
         DC    C'CAN',C'CA',X'01'       CANADIAN DOLLARS                       
         DC    C'NTH',C'DFL',X'02'       DUTCH GUILDER                          
         DC    C'BEL',C'BFR',X'03'       BELGIAN FRANC                          
         DC    C'DEN',C'DKR',X'04'       DANISH KRONE                           
         DC    C'IRE',C'IR$',X'05'       IRISH PUNT                             
         DC    C'GER',C'DMK',X'06'       DEUTSCHMARK                            
         DC    C'POR',C'POR',X'07'       PORTUGUESE ESCUDO                      
         DC    C'SPA',C'PTS',X'08'       SPANISH PESETA                         
         DC    C'ITA',C'ILR',X'09'       ITALIAN LIRE                           
         DC    C'NOR',C'NKR',X'0A'       NORWEGIAN KRONE                        
         DC    C'FRA',C'FFR',X'0B'       FRENCH FRANC                           
         DC    C'SWE',C'SKR',X'0C'       SWEDISH KRONE                          
         DC    C'JAP',C'YEN',X'0D'       JAPANESE YEN                           
         DC    C'AUT',C'ASH',X'0E'       AUSTRIAN SCHILLING                     
         DC    C'SWI',C'SFR',X'0F'       SWISS FRANC                            
         DC    C'ARG',C'ARP',X'FF'       ARGENTINIAN AUSTRALS                   
         DC    C'GRE',C'GDR',X'FF'       GREEK DRACHMA                          
         DC    C'LUX',C'LFR',X'FF'       LUXEMBOURG FRANC                       
         DC    C'MLY',C'MRG',X'FF'       MALAYSIAN RINGGIT                      
         DC    C'FIN',C'FMK',X'FF'       FINLAND MARKKA                         
         DC    C'IRN',C'IRR',X'FF'       IRANIAN RIAL                           
         DC    C'KUW',C'KDH',X'FF'       KUWAIT DINAR                           
         DC    C'UAE',C'UAE',X'FF'       UAE DIRHAM                             
         DC    C'BRZ',C'BRZ',X'FF'       BRAZILIAN CRUZEIRO                     
         DC    C'SAR',C'SRL',X'FF'       SAUDI ARABIAN RIYAL                    
         DC    C'SAF',C'SAF',X'FF'       SOUTH AFRICAN RAND FINANCIAL           
         DC    C'KOR',C'WON',X'FF'       SOUTH KOREAN WON                       
         DC    C'MEX',C'MXP',X'FF'       MEXICAN DOLLAR                         
         DC    C'TAI',C'T ',X'FF'       TAIWAN DOLLAR                          
         DC    C'SAC',C'SAR',X'FF'       SOUTH AFRICAN RAND COMMERCIAL          
         DC    C'AUL',C'A ',X'FF'       AUSTRALIAN DOLLAR                      
         DC    C'HKG',C'HK',X'FF'       HONG KONG DOLLAR                       
         DC    C'NZD',C'NZ',X'FF'       NEW ZEALAND DOLLAR                     
         DC    C'SIN',C'S ',X'FF'       SINGAPORE DOLLAR                       
         DC    C'ECU',C'ECU',X'FF'       EUROPEAN CURRENCY UNIT                 
         DC    C'POU',C'   ',X'FF'       POUND RATE (REJECT)                    
         DC    C'PEN',C'   ',X'FF'       PENCE RATE (REJECT)                    
CURTABX  DC    X'00'                                                            
*                                                                               
YEN      DC    CL3'YEN'                  NO DECIMAL PLACES IN FRWD RATE         
ILR      DC    CL3'ILR'                  NO DECIMAL PLACES IN FRWD RATE         
ECU      DC    CL3'ECU'                                                         
IRPOUND  DC    CL3'IR$'                                                         
POUND    DC    CL3'$ST'                                                         
DOLLAR   DC    CL3'US'                                                         
         EJECT                                                                  
CURRID   DC    CL6'FT CUR'               CURRENCY FEED HEADER                   
EOFEED   DC    CL8'PPPPPPPP'             CURRENCY FEED FOOTER                   
DDSNAME  DC    CL30'CON.FT.TEXT(0)                '                             
RECLEN   DC    H'51'                                                            
INVERT   DC    PL7'1000000000000'                                               
NUMERIC  DC    XL8'F0F0F0F0F0F0F0F0'                                            
FFS      DC    XL8'FFFFFFFFFFFFFFFF'                                            
SPACES   DC    CL64' '                                                          
         LTORG                                                                  
         EJECT                                                                  
****************************************************                            
*   DATA CONTROL BLOCKS                            *                            
****************************************************                            
         SPACE 1                                                                
FTCURRY  DCB   DSORG=PS,MACRF=GL,DDNAME=FTCURRY,EODAD=EOFCURRY                  
         SPACE 2                                                                
REPDESCL DC    CL11'FINSTAT    '                                                
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'FINSTAT UPDATE'                                          
         SPEC  H2,57,C'--------------'                                          
         SPEC  M1,1,C'FINSTAT DATA             '                                
         SPEC  M2,1,C'-------------------------'                                
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* GEGENEXC                                                                      
       ++INCLUDE GEGENEXC                                                       
* GEGENCUR                                                                      
       ++INCLUDE GEGENCUR                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENA9D                                                       
         ORG                                                                    
         EJECT                                                                  
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)                                                     
PRTLINE  DS    0CL130                                                           
PRTFT    DS    CL25                                                             
PRTERR   DS    CL107                                                            
         ORG   PRTLIN+L'PRTLIN                                                  
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
MODE     DS    C                                                                
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
VGEUP    DS    A                                                                
ADDR     DS    F                                                                
FLAG     DS    C                                                                
CURF     DS    CL3                 CURRENCY FROM                                
CURT     DS    CL3                 CURRENCY TO                                  
TYPE     DS    C                   RATE TYPE  (1)MTH (3)MTH (C)LOSE             
DATE     DS    H                   DATE ON CURRENCY ID                          
XRATE    DS    CL5                 EXCHANGE RATE 99999.99999 FORMAT             
XSHFT    DS    X                   SHIFT VALUE SRP                              
COUNT    DS    H                   NUMBER OF RECORDS UPLOADED                   
XCOUNT   DS    H                   NUMBER OF RECORDS IGNORED                    
ECOUNT   DS    H                   NUMBER OF ERRORS                             
FTDATA   DS    CL25                COPY RECORD DATA                             
DSNAME   DS    CL30                                                             
WORK     DS    CL64                                                             
TEMP     DS    CL64                                                             
SAVEREC  DS    CL80                                                             
PRINT    DS    CL133                                                            
PCLOSE   DS    16CL12              POUND CLOSE                                  
DCLOSE   DS    16CL12              DOLLAR CLOSE                                 
ECLOSE   DS    16CL12              ECU CLOSE                                    
       ++INCLUDE GEUPBLK                                                        
GEUPBUF  DS    XL4096              4K WKFILE BUFFER                             
LOCALX   EQU   *                                                                
*                                                                               
*CTDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE CTDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTGEN16   08/22/00'                                      
         END                                                                    
