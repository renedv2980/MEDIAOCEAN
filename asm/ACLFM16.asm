*          DATA SET ACLFM16    AT LEVEL 008 AS OF 11/06/96                      
*PHASE T60316A,+0                                                               
*INCLUDE HEXIN                                                                  
         TITLE 'MONTH OF SERVICE LOCK'                                          
T60316   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM16*                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         SPACE 4                                                                
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   MS02                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      SET KEY TO READ COMPANY                      
         CLI   MYANYKEY,C'Y'                                                    
         BE    XIT                                                              
         MVI   MYANYKEY,C'Y'       FIRST TIME SET FOR READ                      
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*          *** D I S P L A Y   A N   E L E M E N T ***                          
*              ***********************************                              
         SPACE 3                                                                
MS02     CLI   MODE,DSPLYREC                                                    
         BNE   MS20                                                             
MS04     TWAXC LOGMONH                                                          
         LA    R2,LOGMONH          SET R2 TO 1ST INPUT FIELD ON SCREEN          
         LA    R5,IO2              FIND MOS LOCK ELEMENT FOR DISPLAY            
         AH    R5,DATADISP                                                      
MS06     CLI   0(R5),0                                                          
         BE    XIT                                                              
         CLI   0(R5),X'10'         IS IT A X'10' EL (COMPANY ELEMENT)           
         BE    MS08                                                             
         CLI   0(R5),X'18'         IS IT A X'18' EL                             
         BE    MS13                                                             
MS07     ZIC   RF,1(R5)            ELEMENT LENGTH                               
         AR    R5,RF               POINT R5 TO NEXT EL                          
         LA    R2,LOGMONH          SET R2 TO 1ST INPUT FIELD ON SCREEN          
         B     MS06                                                             
         SPACE 3                                                                
*                                  FROM THE X'10' ELEMENT                       
*                                  *********************                        
MS08     DS    0H                                                               
         USING ACCOMPD,R5                                                       
         LA    R2,LOGMONH          SET R2 TO DEFAULT MONTH ON SCREEN            
         LA    R3,MONTHS           MONTHS TABLE                                 
         LA    R4,MONLST           MONTHS 1 DIGIT TABLE                         
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   LOGMON,=C'XXX'                                                   
         LA    RF,12                                                            
         SPACE 2                                                                
MS09     CLC   ACMPMOSX,0(R4)      MATCH MONTH IN COMP EL TO TABLE              
         BE    MS10                YES - MOVE MONTH TO SCREEN                   
         LA    R4,1(R4)            BUMP UP MTH 1 DIGIT TABLE                    
         LA    R3,3(R3)            AND MTH TRANSLATION TABLE                    
         BCT   RF,MS09             REDUCE MONTH COUNTER                         
         SPACE 1                                                                
         B     *+10                                                             
MS10     MVC   LOGMON,0(R3)        MOVE 3 DIGIT MONTH CODE TO SCREEN            
         SPACE 1                                                                
         LA    R2,LOGSECH          DEFAULT SEC LEVEL ON SCREEN                  
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   LOGSEC,ACMPBSL      COMPANY ELEMENT SEC LEVEL                    
         OI    8(R2),X'F0'         FROM HEX TO CHARS                            
         B     MS07                                                             
         EJECT                                                                  
*                                  FROM THE X'18' ELEMENT                       
*                                  *********************                        
         SPACE 1                                                                
         USING ACMOSD,R5                                                        
         USING MINID,R2                                                         
         SPACE 2                                                                
MS13     ZIC   RE,ACMOSNUM         NUMBER OF MINI MOS ELEMENTS                  
         LA    R6,ACMOSTYP         SET R6 TO 1ST BATCH TYPE IN EL               
         LA    R2,LOGTYP1H         SET R2 TO 1ST BATCH TYPE ON SCREEN           
         USING ACMOSTYP,R6                                                      
         SPACE 1                                                                
MS14     DS    0H                                                               
         OI    MINITYPH+6,X'80'    TRANSMIT                                     
         EDIT  (1,ACMOSTYP),(2,MINITYP),ALIGN=LEFT,ZERO=NOBLANK                 
         SPACE 1                                                                
         LA    R3,MONTHS           MONTHS TABLE                                 
         LA    R4,MONLST           MONTHS 1 DIGIT TABLE                         
         SR    RF,RF                                                            
         LA    RF,12                                                            
         SPACE 1                                                                
MS16     CLC   ACMOSMTH,0(R4)      MATCH MONTH IN EL TO TABLE                   
         BE    MS18                YES - MOVE MONTH TO SCREEN                   
         LA    R4,1(R4)            BUMP UP MTH 1 DIGIT TABLE                    
         LA    R3,3(R3)            AND MTH TRANSLATION TABLE                    
         BCT   RF,MS16             REDUCE MONTH COUNTER                         
         SPACE 1                                                                
         B     MS19                                                             
MS18     OI    MINIMTHH+6,X'80'    TRANSMIT                                     
         MVC   MINIMTH,0(R3)       MOVE 3 DIGIT MONTH CODE TO SCREEN            
         SPACE 1                                                                
MS19     OI    MINISECH+6,X'80'    TRANSMIT                                     
         MVC   MINISEC,ACMOSSEC    SEC LEVEL FROM ELEMENT TO SCREEN             
         CLI   ACMOSSEC,X'40'                                                   
         BE    *+8                                                              
         OI    MINISEC,X'F0'       FROM HEX TO CHARS                            
         SPACE 1                                                                
         LA    R6,ACMOSNEX(R6)     R6 TO NEXT BATCH TYPE IN EL                  
         LA    R2,MININEX          POINT R2 TO NEXT MINI ON SCREEN              
         BCT   RE,MS14             REDUCE NUMBER ON MINI ELEMENTS               
         LA    R2,LOGMONH          SET R2 TO 1ST INPUT FIELD ON SCREEN          
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
*          *** R E B U I L D   A N   E L E M E N T ***                          
*              ***********************************                              
         SPACE 3                                                                
*                                  THE X'10' ELEMENT                            
*                                  *****************                            
MS20     DS    0H                                                               
         USING ACCOMPD,R5                                                       
         LA    R5,IO2              FIND COMPANY ELEMENT FOR DISPLAY             
         AH    R5,DATADISP                                                      
MS22     CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT -TROUBLE--                
         CLI   0(R5),X'10'         IS IT A X'10' EL (COMPANY ELEMENT)           
         BE    MS24                                                             
         ZIC   RF,1(R5)            ELEMENT LENGTH                               
         AR    R5,RF               POINT R5 TO NEXT EL                          
         B     MS22                                                             
         SPACE 1                                                                
MS24     DS    0H                                                               
         LA    R2,LOGMONH          SET R2 TO DEFAULT MONTH SCREEN               
         LA    R4,MONLST           1 DIGIT MONTH CODE TABLE                     
         LA    R3,MONTHS           3 DIGIT MONTH TABLE                          
         SR    RF,RF                                                            
         LA    RF,12                                                            
         SPACE 1                                                                
MS25     CLC   LOGMON,0(R3)        COMPARE 3 DIGIT MTH CODE TO SCREEN           
         BE    MS26                FOUND A MATCH                                
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   RF,MS25                                                          
         CLC   LOGMON,=C'XXX'      IS THERE NO DEFAULT MONTH?                   
         BE    MS26A                                                            
         CLI   LOGACT,C'N'         IS ACTION NEW                                
         BNE   MTHERROR            NO- ERROR                                    
         B     MS04                YES DISPLAY FIRST                            
         SPACE 1                                                                
MS26     MVC   ACMPMOSX,0(R4)      UPDATE CO ELEMENT WITH MOS MONTH             
MS26A    LA    R2,LOGSECH          R2 TO DEFAULT SEC LEVEL ON SCREEN            
         CLI   5(R2),0                                                          
         BE    MS35                                                             
         CLI   LOGSEC,C'0'         SEC LEVEL MUST BE X'01' X'02'                
         BL    SECERR1             SECURITY LEVEL ERROR                         
         CLI   LOGSEC,C'3'         OR X'03' ELSE IT INVALID                     
         BH    SECERR1             SECURITY LEVEL ERROR                         
         MVC   ACMPBSL,LOGSEC      MOVE SEC LEV INTO ELEMENT                    
         NI    ACMPBSL,X'0F'       FROM CHARS TO HEX                            
         EJECT                                                                  
*                                  THE X'18' ELEMENT                            
*                                  *****************                            
MS35     DS    0H                                                               
         USING ACMOSD,R5                                                        
         SR    R7,R7                                                            
         SR    R8,R8               USE R8 AS MINI ELEMENT COUNTER               
         LA    R7,MININUM          MAX NUMBER OF SCREEN MINI EL'S               
         LA    R5,ELEMENT          ELEMENT WORK AREA                            
         XC    ELEMENT,ELEMENT     CLEAR IT                                     
         LA    R6,ACMOSTYP         R6 TO BUMP THRU MINI EL'S IN X'18'           
         USING ACMOSTYP,R6                                                      
         SPACE 1                                                                
         MVI   ACMOSEL,X'18'       MOVE IN ELEMENT NUMBER                       
         LA    R2,LOGTYP1H         SET R2 TO 1ST BATCH TYPE ON SCREEN           
         SPACE 1                                                                
MS37     CLI   5(R2),0             INPUT TO BATCH TYPE FIELD                    
         BNE   MS37A               YES- CONTINUE                                
         CLI   MINIMTHH+5,X'0'     INPUT TO MONTH FIELD                         
         BNE   BTHERROR            ------------BATCH TYPE MISSING               
         CLI   MINISECH+5,X'0'     INPUT TO SEC LEV FIELD                       
         BNE   BTHERROR            ------------BATCH TYPE MISSING               
         B     MS45A                                                            
         SPACE 1                                                                
MS37A    CLI   MINIMTHH+5,X'0'     IF YOU HAVE A BAT TYPE                       
         BNE   MS37B               ---YOU MUST HAVE INPUT TO EITHER             
         CLI   MINISECH+5,X'0'     ---MTH OR SEC LEVEL TOO                      
         BE    ANERROR             ---ERROR MISSING MTH OR SEC LEV              
         SPACE 1                                                                
MS37B    CLI   MINITYPH+5,X'1'     IS LENGTH OF TYPE INPUT 1                    
         BNE   MS38                NO- LENGTH MUST BE 2                         
         CLI   MINITYP,C'0'     MAKE SURE ITS NUMERIC                           
         BL    BTHERROR                                                         
         CLI   MINITYP,C'9'                                                     
         BH    BTHERROR                                                         
         MVC   DUB(1),MINITYP                                                   
         NI    DUB,X'0F'           FROM CHARS TO HEX                            
         MVC   WORK(1),DUB                                                      
         B     MS39                                                             
MS38     CLC   MINITYP,=C'00'      MAKE SURE ITS NUMERIC                        
         BL    BTHERROR                                                         
         CLC   MINITYP,=C'99'                                                   
         BH    BTHERROR                                                         
         PACK  DUB,MINITYP         FROM CHARACTER BATCH NUMBER                  
         CVB   R1,DUB              TO                                           
         STC   R1,WORK             HEX BATCH NUMBER                             
         EJECT                                                                  
*                                  **CHECK FOR DUPLICATE INPUT***               
         SPACE 1                                                                
MS39     LA    R3,LOGTYP1          R3 - ADDR 1ST BATCH TYPE FIELD               
         LA    R4,MINITYP          R4 - ADDR OF CURRENT BAT TYPE FIELD          
         SPACE 1                                                                
MS39A    CR    R3,R4               POINTING AT THE SAME FIELD?                  
         BE    MS40                YES- THIS BAT TYPE IS NOT A DUP              
         CLC   0(2,R3),MINITYP     THE SAME BATCH TYPE?                         
         BE    DUPERROR            DUPLICATE BATCH TYPE ERROR                   
         LA    R3,MINILNQ(R3)      R3 TO NEXT BAT TYP ON SCREEN                 
         B     MS39A                                                            
         SPACE 1                                                                
MS40     LA    R4,TYPETAB          TABLE OF VALID BATCH TYPES                   
MS40A    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    BTHERROR                                                         
         SPACE 1                                                                
MS40B    CLC   0(1,R4),WORK        IS IT A VALID BATCH TYPE                     
         BE    MS40C                                                            
         LA    R4,1(R4)            BUMP UP BATCH TYPE TABLE                     
         B     MS40A               GO CHECK FOR VALID BATCH TYPE                
         SPACE 1                                                                
MS40C    MVC   ACMOSTYP,WORK       BATCH TYPE INTO ELEMENT                      
         AH    R8,=H'1'            ADD 1 TO MINI ELEMENT COUNTER                
         CLI   MINIMTHH+5,X'0'     IS THERE INPUT TO MONTH ON SCREEN            
         BNE   MS40D               YES- GO PUT IT INTO ELEMENT                  
         MVI   ACMOSMTH,X'40'      SPACES INTO ELEMENT                          
         B     MS44A                                                            
         SPACE 1                                                                
MS40D    LA    R4,MONLST           1 DIGIT MONTH CODE TABLE                     
         LA    R3,MONTHS           3 DIGIT MONTH TABLE                          
         LA    RF,12                                                            
         SPACE 1                                                                
MS42     CLC   MINIMTH,0(R3)       COMPARE 3 DIGIT MTH CODE TO SCREEN           
         BE    MS44                FOUND A MATCH                                
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   RF,MS42                                                          
         LA    R2,MINIMTHH                                                      
         B     MTHERROR            INVALID MONTH ERROR                          
         SPACE 2                                                                
MS44     MVC   ACMOSMTH,0(R4)      MOVE MONTH INTO ELEMENT                      
MS44A    CLI   MINISECH+5,X'0'     IS THERE INPUT TO SECURITY LEVEL             
         BNE   MS44B               YES- PUT IT IN ELEMENT                       
         MVI   ACMOSSEC,X'40'                                                   
         B     MS45                                                             
         SPACE 1                                                                
MS44B    CLI   MINISEC,C'0'        SEC LEVEL MUST BE X'01' X'02'                
         BL    SECERROR            SECURITY LEVEL ERROR                         
         CLI   MINISEC,C'3'        OR X'03' ELSE IT INVALID                     
         BH    SECERROR            SECURITY LEVEL ERROR                         
         MVC   ACMOSSEC,MINISEC    MOVE SEC LEV INTO ELEMENT                    
         NI    ACMOSSEC,X'0F'      FROM CHARS TO HEX                            
         SPACE 1                                                                
MS45     LA    R6,ACMOSNEX(R6)     R6 TO NEXT BATCH TYPE IN EL                  
MS45A    LA    R2,MININEX          POINT R2 TO NEXT MINI ON SCREEN              
         BCT   R7,MS37                                                          
         SPACE 1                                                                
         STC   R8,ACMOSNUM         NUMBER OF MINI ELEMENT ENTRIES               
         MH    R8,=H'3'                X 3                                      
         LA    R8,3(R8)                + 3 = LENGTH OF ELEMENT                  
         STC   R8,ACMOSLEN                                                      
         SPACE 1                                                                
         GOTO1 REMANEL,DMCB,(X'18',0)                                           
         CLI   ACMOSNUM,X'0'       DID WE BUILD A 18 ELEMENT                    
         BE    XIT                                                              
         GOTO1 ADDANEL                                                          
         MVI   ERROR,X'FF'                                                      
         B     MS04                RE-DISPLAY NEW OR AMENDED INPUT              
         EJECT                                                                  
************** ERROR MESSAGES********************                               
*************************************************                               
         SPACE 3                                                                
BTHERROR MVI   ERROR,X'FE'         I'LL SET THE ERROR MESSAGE                   
         MVC   LOGHEAD(22),=C'**INVALID BATCH TYPE**'                           
         B     XIT                                                              
         SPACE 2                                                                
MTHERROR MVI   ERROR,X'FE'         I'LL SET THE ERROR MESSAGE                   
         MVC   LOGHEAD(34),=C'**INVALID MONTH - USE FORMAT MMM**'               
         B     XIT                                                              
         SPACE 2                                                                
SECERROR LA    R2,MINISECH                                                      
SECERR1  MVI   ERROR,X'FE'         I'LL SET THE ERROR MESSAGE                   
         MVC   LOGHEAD(39),=C'**INVALID SECURITY LEVEL- USE 0,1,2,3**'          
         B     XIT                                                              
         SPACE 2                                                                
DUPERROR MVI   ERROR,X'FE'         I'LL SET THE ERROR MESSAGE                   
         MVC   LOGHEAD(40),=C'**ERROR-DUPLICATE BATCH TYPE ON SCREEN**'         
         B     XIT                                                              
         SPACE 2                                                                
MISERROR MVI   ERROR,X'FE'         I'LL SET THE ERROR MESSAGE                   
         MVC   LOGHEAD(36),=C'**ERROR - MISSING BATCH TYPE INPUT**'             
         B     XIT                                                              
         SPACE 2                                                                
ANERROR  LA    R2,MINIMTHH                                                      
         MVI   ERROR,X'FE'         I'LL SET THE ERROR MESSAGE                   
         MVC   LOGHEAD(39),=C'**ERROR - MONTH OR SEC LEVEL REQUIRED**'          
         B     XIT                                                              
         SPACE 2                                                                
MOSINV   MVI   ERROR,INVALID                                                    
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
***************TABLES******************                                         
***************************************                                         
         SPACE 2                                                                
MONTHS   DC    C'JANFEBMARAPRMAYJUN'                                            
         DC    C'JULAUGSEPOCTNOVDEC'                                            
         SPACE 2                                                                
MONLST   DC    C'123456789ABC'                                                  
         SPACE 3                                                                
*                                **** TABLE OF BATCH TYPES****                  
TYPETAB  DS    0C                                                               
         DC    X'01'             'INVOICE'                                      
         DC    X'02'             'STANDARD JRNL ENTRY'                          
         DC    X'03'             'CHECK'                                        
*&&UK*&& DC    X'04'             'PETTY CASH'                                   
         DC    X'05'             'MULTIPLE JRNL ENTRY'                          
         DC    X'06'             'MANUAL BILLING'                               
         DC    X'08'             'INTERNAL INVOICE'                             
         DC    X'0B'             'ANALYSED TRANSFER'                            
         DC    X'0E'             'WRITE OFF'                                    
*&&US*&& DC    X'0F'             'BILLABLE EXPENSE'                             
         DC    X'10'             'CASH RECEIPT'                                 
*&&UK*&& DC    X'11'             'TIME SHEETS'                                  
         DC    X'12'             'SPECIAL TRANSFER'                             
         DC    X'13'             'ONE-SIDED POSTING'                            
*&&US*&& DC    X'14'             'SPECIAL JRNL ENTRY'                           
         DC    X'15'             'INVOICE (NON-BILL)'                           
         DC    X'16'             'CHECK (NON-BILLABLE)'                         
         DC    X'17'             'GENERAL VOUCHER' (FCB)                        
*&&US*&& DC    X'18'             'TURNAROUND CHECK'                             
*&&US*&& DC    X'19'             'RETAIL BILLING' KINNEY                        
         DC    X'1A'             'MEDIA BILLING'                                
*&&US*&& DC    X'1B'             'TIME SHEETS'                                  
         DC    X'1E'             'CASH ALLOCATION'                              
*&&US*&& DC    X'1F'             'SCHLITZ BILLING'                              
         DC    X'21'             'BILLABLE TIME'                                
         DC    X'22'             'JOB-TO-JOB TRANSFER'                          
*&&US*&& DC    X'23'             'RETAIL BILLING' GENL                          
         DC    X'24'             'ADVANCE PAYMENT'                              
         DC    X'25'             'VOID CHECK'                                   
*&&US*&& DC    X'26'             'DDS BILLING'                                  
         DC    X'27'             'ACCRUAL'                                      
*&&US*&& DC    X'29'             'TIME-PROJECT CONTROL'                         
*&&US*&& DC    X'2A'             'MEDIA BILLING' CANAD                          
         DC    X'2B'             'ANALYSED ACCRUAL'                             
         DC    X'2D'             'MULTIPLE ANALYSED J/E'                        
*&&US*&& DC    X'2E'             'MULTIPLE INVOICE'                             
*&&US*&& DC    X'2F'             'ESTIMATED PRODUCTION'                         
* TYPE 48      X'30'             AUTO REVERSE OF ESTIMATED PRODUCTION           
*&&US*&& DC    X'31'             'CLIENT TIMESHEETS'                            
*&&US*&& DC    X'33'             'INTER-AGENCY'                                 
*&&US*&& DC    X'34'             'TAX ADJUSTMENTS'                              
         DC    X'35'             'MULTI  ANALYS ACCRUAL'                        
         DC    X'37'             'INCOME ACCRUAL'                               
*&&US*&& DC    X'3A'             'INTER-AGENCY'                                 
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        DSECT TO COVER   *** ON THE SCREEN ***  TYPE-MON-SEC                   
MINID    DSECT                                                                  
MINITYPH DS    CL8                   TYPE HEADER                                
MINITYP  DS    CL2                   BATCH TYPE                                 
MINIMTHH DS    CL8                   MONTH HEADER                               
MINIMTH  DS    CL3                   LATEST LOCKED MONTH                        
MINISECH DS    CL8                   SECURITY LEVEL HEADER                      
MINISEC  DS    CL1                   SECURITY LEVEL '01' '02' '03'              
MININEX  EQU   *                     NEXT MINI ELEMENT                          
MINILNQ  EQU   *-MINID               LENGTH OF A MINI ELEMENT                   
MININUM  EQU   (LOGENDH-LOGTYP1H)/MINILNQ   MAX NUMBER OF ENTRIES               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFME9D                                                       
         SPACE 2                                                                
MYANYKEY DS    CL1                                                              
*        ACLFMWORK                                                              
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACLFMEQU                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACLFM16   11/06/96'                                      
         END                                                                    
