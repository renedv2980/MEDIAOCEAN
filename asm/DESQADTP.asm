*          DATA SET DESQADTP   AT LEVEL 009 AS OF 12/18/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESQADTA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
SQADTP   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SQADTP,=V(REGSAVE),R6                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         OPEN  (IN,(INPUT))                                                     
         MVI   GETSW,1                                                          
         OPEN  (OUT,(OUTPUT))                                                   
         LA    RE,OUTDATA                                                       
         XCEF  (RE),1000                                                        
         MVI   FIRSTREC,C'Y'                                                    
*                                                                               
GET      GET   IN,INREC                                                         
         ZICM  R3,INREC,3          GET LENGTH                                   
         CH    R3,=H'10'           SKIP RECORDS SHORTER THAN 10?                
         BL    GET                                                              
         SH    R3,=H'4'            VARIABLE LENGTH RECS                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),INREC+4                                                     
*                                                                               
***********   KEY OF THE OUPUT FILE RECORD *****************                    
OUT10    LA    R9,OUTDATA       R2 POINTS TO THE OUTDATA                        
         USING DRKEY,R9                                                         
         MVI   DRCODE,DRCODEQU                                                  
         MVI   DRMEDIA,C'T'                                                     
         MVI   DRSRC,C'N'                                                       
         XC    DUB,DUB                                                          
***************************************************************                 
*        MARKET                                               *                 
***************************************************************                 
         LA    R2,P                                                             
         LA    R0,6             6TH FIELD IS MARKET ID                          
         BAS   RE,PARSER        IS THE MARKET                                   
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
*                                                                               
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         LA    R1,SQADMKT                                                       
MKT10    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CH    RE,0(R1)                                                         
         BE    MKT20                                                            
         LA    R1,L'SQADMKT(R1)                                                 
         B     MKT10                                                            
*                                                                               
MKT20    LH    RE,2(R1)                                                         
         STCM  RE,3,DRKMKT      CONVERT MARKET #                                
         EDIT  (B2,DRKMKT),(4,DRSTAT),ALIGN=RIGHT,FLOAT=0,FILL=0                
         MVI   DRSTAT+L'DRSTAT-1,C'T'                                           
*                                                                               
***************************************************************                 
*        BOOKTYPE                                                               
***************************************************************                 
         MVI   BOOKTYPE,C'S'    DEFAULT BOOKTYPE IS 'S'                         
*                                                                               
         LA    R2,P             WE, NOT SQAD, ADDED A FILE ID                   
         LA    R0,10            FIELD IN THE 10TH FIELD                         
         BAS   RE,PARSER        DERIVED FROM THE FIRST 2 CHARS                  
*                                                                               
* FILE NAME WAS PREVIOUS TF###_CSV.ZIP.  SPEC-30716 - FILE NAME IS              
* NOW TV###_CSV.ZIP FOR TVQ (QUARTERLY).                                        
*                                                                               
*        CLC   =C'TF',WORK+1    OF THE FILE NAME.                               
         CLC   =C'TV',WORK+1    OF THE FILE NAME.                               
         BNE   *+8                                                              
         MVI   BOOKTYPE,C'Q'                                                    
         MVC   DRBTYP,BOOKTYPE                                                  
*                                                                               
***************************************************************                 
*        YY/QTR DATA YEAR                                                       
***************************************************************                 
*                                                                               
*    MOVE YEAR/QTR TO DRKMKT                                                    
         LA    R2,P           POINT TO input                                    
         LA    R0,4           DATA YEAR IS 4TH FIELD IN INPUT                   
OUT15    BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         XC    WORK+1(2),WORK+1  LETS GET RID OF CENTURY FOR NOW                
         XC    DUB,DUB                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RF,DUB                                                           
         STC   RF,DRKMKT      STORE BOOK YEAR                                   
*                                                                               
*   R2 POINTS TO MONTH GET MONTH BY CALLING PARSER                              
*                                                                               
         LA    R2,P           QTR IS 3RD FIELD                                  
         LA    R0,3                                                             
         BAS   RE,PARSER      MOVE TO NEXT FIELD (QTR)                          
         XC    DUB,DUB                                                          
         ZIC   RE,WORK        LENGTH OF MONTH                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RF,DUB                                                           
         STC   RF,DRKMKT+1    STORE BOOK MONTH                                  
*  Y2K CODE                                                                     
*                                                                               
         CLI   DRKMKT,27                                                        
         BH    NOTY2K                                                           
         LA    RE,100                                                           
         ZIC   R0,DRKMKT                                                        
         AR    RE,R0                                                            
         LA    R0,99                                                            
         SR    RE,R0                                                            
         LA    R0,99                                                            
         AR    RE,R0          YEAR Y2K                                          
         STC   RE,DRKMKT                                                        
*                                                                               
NOTY2K   DS    0H                                                               
******************************************************************              
*  MOVE ISSUE YR/MM TO DRBOOK                                                   
******************************************************************              
         LA    R2,P           POINT TO input                                    
         LA    R0,2           BOOK YEAR IS 2ND FIELD IN INPUT                   
OUT15BK  BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
*                                                                               
         XC    WORK+1(2),WORK+1  LETS GET RID OF CENTURY FOR NOW                
         XC    DUB,DUB                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RF,DUB                                                           
         STC   RF,DRBOOK      STORE BOOK YEAR                                   
*                                                                               
*   R2 POINTS TO MONTH GET MONTH BY CALLING PARSER                              
*                                                                               
         LA    R2,P                                                             
         LA    R0,1                                                             
         BAS   RE,PARSER      MOVE TO NEXT FIELD (QTR)                          
         XC    DUB,DUB                                                          
         ZIC   RE,WORK        LENGTH OF MONTH                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RF,DUB                                                           
         STC   RF,DRBOOK+1    STORE BOOK MONTH                                  
*                                                                               
*  Y2K CODE                                                                     
*                                                                               
         CLI   DRBOOK,27                                                        
         BH    NOTY2KBK                                                         
         LA    RE,100                                                           
         ZIC   R0,DRBOOK                                                        
         AR    RE,R0                                                            
         LA    R0,99                                                            
         SR    RE,R0                                                            
         LA    R0,99                                                            
         AR    RE,R0          YEAR Y2K                                          
         STC   RE,DRBOOK                                                        
*                                                                               
NOTY2KBK DS    0H                                                               
         XC    DRBOOK,=X'FFFF'                                                  
*                                                                               
**********************************************************                      
*      DAYPART                                                                  
**********************************************************                      
         LA    R2,P                                                             
         LA    R0,8                                                             
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
*                                                                               
DYPRTLP  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         LA    R1,DYPRTTAB        THAN NEW FORMAT DAYPART CODE                  
DYP03    ZIC   R2,4(R1)                                                         
         CR    RE,R2                                                            
         BE    DYP05                                                            
         LA    R1,L'DYPRTTAB(R1)                                                
         CLC   =C'ZZ',0(R1)                                                     
         BNE   DYP03                                                            
         DC    H'0'                                                             
*                                                                               
DYP05    MVC   SVDPRTCD(4),0(R1)                                                
         MVI   DRHIGHD,X'95'                                                    
         MVC   SVDYQH(2),DRHIGHD                                                
         MVC   DRHIQHR,5(R1)                                                    
*                                                                               
DYPX     DS    0H                                                               
*                                                                               
         GOTO1 MKTTYPE                                                          
         GOTO1 QTRHOUR                                                          
*                                                                               
         CLI   FIRSTREC,C'Y'                                                    
         BE    OUT50                NO SVODATA TO COMPARE YET                   
*                                                                               
OUT25    CLC   OUTDATA(L'DRKMAJOR+L'DRKMINOR),SVODATA                           
         BE    OUT50                SAME RECORD, DIFFERENT DEMO                 
*                                                                               
         BAS   RE,HEADFILL          FILL IN ELEMENT HEADER INFO                 
         PUT   OUT,SVOUTREC         PUT OUT OLD AND DO NEW INREC                
         CLC   OUTDATA+3(L'DRSTAT+L'DRBOOK),SVODATA+3                           
         BE    OUT50                NEED TO BLD MKEYS                           
         MVC   SVMKT,SVODATA+3      THE MKT BEFORE RECORD CHANGE                
         GOTO1 BLDBSKEY                                                         
         GOTO1 BLDMLKEY                                                         
         GOTO1 BLDSBKEY                                                         
*                                                                               
OUT50    LA    R9,OUTDATA           R2 POINTS TO THE OUTDATA                    
         USING DRKEY,R9                                                         
         LA    R4,SQADTGT           LIST OF DEMO CATEGORIES                     
         LA    R8,0                                                             
*                                                                               
**********************************************************                      
*      DEMO TARGET ID                                                           
**********************************************************                      
*                                                                               
         LA    R0,7                 DEMO TARGET ID IS 8TH FIELD                 
         LA    R2,P                                                             
OUT55    BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
*                                                                               
OUT57    CH    RE,0(R4)                                                         
         BE    OUT60                                                            
         LA    R4,10(R4)         BUMPING THROUGH TARGET ID TAB                  
         B     OUT57                                                            
*                                                                               
OUT60    MVI   WORK,8                                                           
         MVC   WORK+1(8),2(R4)                                                  
*                                                                               
         LA    R4,DEMOTAB                                                       
OUT70    CLC   =X'0000',0(R4)                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),WORK+1    COMPARE DEMOS ON OUTPUT LINE TO TABLE          
         BE    OUT80                                                            
         LA    R4,L'DEMOTAB(R4)                                                 
         LA    R8,1(R8)                                                         
         B     OUT70                                                            
*                                                                               
**********************************************************                      
*      DEMO VALUE                                                               
**********************************************************                      
OUT80    LA    R0,9                                                             
         LA    R2,P                                                             
         BAS   RE,PARSER      GET DEMO VALUE                                    
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
**********************************************************                      
*      CPP LEVEL                                                                
**********************************************************                      
         LA    R2,P                                                             
         LA    R0,5           CPP LEVEL IS THE 5TH FIELD                        
OUT85    BAS   RE,PARSER                                                        
*                                                                               
         CLI   WORK+1,C'1'            TYPE L                                    
         BNE   OUT90                                                            
         LA    R5,DRFRSTEL    1st demo elem is past mkt elem                    
         LA    R5,MARLNEQ(R5)  past qtr hour elem                               
         LA    R5,QHPNMAX+(QHPNAME-QHELEM)(R5)                                  
         USING DEMOELD,R5                                                       
         LA    R7,DEMOSL                                                        
         MVI   DELTYPL,X'41'                                                    
         LR    RE,R5                                                            
         LA    RE,DEMOLLN(RE)                                                   
         ST    RE,DMCB                                                          
*                                                                               
OUT90    CLI   WORK+1,C'0'             TYPE A                                   
         BNE   OUT95                                                            
         LA    R5,DRFRSTEL    2st demo elem is past mkt elem                    
         LA    R5,MARLNEQ(R5)  past qtr hour elem                               
         LA    R5,QHPNMAX+(QHPNAME-QHELEM)(R5)                                  
         LA    R5,DEMOLLN(R5)    PAST FIRSTDEMO                                 
         USING DEMOELD,R5                                                       
         LA    R7,DEMOSL                                                        
         MVI   DELTYPL,X'43'                                                    
         LR    RE,R5                                                            
         LA    RE,DEMOLLN(RE)                                                   
         ST    RE,DMCB                                                          
*                                                                               
OUT95    CLI   WORK+1,C'2'            TYPE H                                    
         BNE   OUT98                                                            
         LA    R5,DRFRSTEL    2st demo elem is past mkt elem                    
         LA    R5,MARLNEQ(R5)  past qtr hour elem                               
         LA    R5,QHPNMAX+(QHPNAME-QHELEM)(R5)                                  
         LA    R5,DEMOLLN(R5)    PAST FIRSTDEMO                                 
         LA    R5,DEMOLLN(R5)    PAST 2ND  DEMO                                 
         USING DEMOELD,R5                                                       
         LA    R7,DEMOSL                                                        
         MVI   DELTYPL,X'45'                                                    
         LR    RE,R5                                                            
         LA    RE,DEMOLLN(RE)                                                   
         ST    RE,DMCB                                                          
*                                                                               
OUT98    MH    R8,=H'2'       HOW FAR FROM THE DEMOS ELEMENT                    
         AR    R7,R8          R8 IS THE NUMBER OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF   MOVE IN THE DEMO VALUE TO THE DEMO ELEM           
*                                                                               
         LA    RF,L'OUTDATA                                                     
         LR    R1,RF                                                            
         LA    R0,SVODATA     VARIABLE LENGTH                                   
         LA    RE,OUTDATA                                                       
         MVCL  R0,RE                                                            
*                                                                               
         MVI   FIRSTREC,C'N'                                                    
         B     GET                                                              
OUT100   LA    RF,L'SVODATA                                                     
         LR    R1,RF                                                            
         LA    R0,OUTDATA     VARIABLE LENGTH                                   
         LA    RE,SVODATA                                                       
         MVCL  R0,RE                                                            
*                                                                               
OUT130   LA    RE,OUTDATA                                                       
         XCEF  (RE),1000                                                        
         MVI   FRSTFLG,C'N'                                                     
         B     GET                                                              
ENDJOB   DS    0C                                                               
         BAS   RE,HEADFILL                                                      
         PUT   OUT,SVOUTREC                                                     
         MVC   SVMKT,OUTDATA+3     THE LAST RECORD TYPE MKT                     
         GOTO1 BLDBSKEY                                                         
         GOTO1 BLDMLKEY                                                         
         GOTO1 BLDSBKEY                                                         
         B     CLSE                                                             
CLSE     CLI   GETSW,5                                                          
*                                                                               
CLSE2    CLOSE (IN)                                                             
*                                                                               
         EJECT                                                                  
OUTPROGX DS    0C                                                               
         CLOSE (OUT)                                                            
         B     EXIT                                                             
*                                                                               
************************************************                                
*          DATA SET DECNVCOR   AT LEVEL 030 AS OF 01/25/99                      
************************************************                                
*  PARSER                                                                       
* ON ENTRY R2-POINTS TO BEGINNING OF WHERE TO START PARSING                     
* ON EXIT  WORK  HAS THE LENGTH OF FIELD                                        
*          WORK+1  HAS THE PARSED FIELD                                         
*                                                                               
*                                                                               
PARSER   NTR1                                                                   
         LA    RE,WORK+1                                                        
         XC    WORK,WORK                                                        
         LA    R5,1                STARTING WITH FIRST FIELD                    
         LA    R7,0                FIELD LENGTH                                 
*                                                                               
PAR05    CR    R5,R0               THIS THE FIELD WE LOOKING FOR?               
         BL    PAR10                                                            
PAR06    LA    R2,1(R2)            MOVE PAST FIRST "                            
*                                                                               
PAR07    MVC   0(1,RE),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    R7,1(R7)                                                         
         STC   R7,WORK             KEEP TRACK OF LENGTH                         
*                                                                               
PAR08    CLI   0(R2),C'"'          END OF FIELD YET?                            
         BNE   PAR07                                                            
         B     PARXX                                                            
*                                                                               
PAR10    CLI   0(R2),C','          START OF NEXT FIELD?                         
         BE    PAR15                                                            
PAR12    LA    R2,1(R2)            BUMP THROUGH TILL NEXT FIELD                 
         CLI   0(R2),C','                                                       
         BNE   PAR12                                                            
         LA    R5,1(R5)                                                         
         LA    R2,1(R2)                                                         
         B     PAR05                                                            
*                                                                               
PAR15    LA    R2,1(R2)            POINT TO NEXT STARTING "                     
         LA    R5,1(R5)            BUMP COUNTER                                 
         B     PAR05                                                            
PARXX    XIT1                                                                   
************************************************                                
************************************************                                
*  counting element size routine                                                
*  DMCB =  A(START OF FIELD TO COUNT FROM)                                      
*  DMCB+4= FULL WORD LENGTH OF THE FIELD TO BE COUNTED                          
*  DMCB+8  ONE BYTE LENGTH (TOTAL LENGTH OF DEMOS ELEMENT)                      
*                                                                               
************************************************                                
SIZER    NTR1                                                                   
         L     R2,DMCB                                                          
         L     R4,DMCB+4                                                        
         AR    R4,R2                                                            
         SH    R4,=H'2'     MOVE R4 TO END OF FIELD                             
         LA    R3,0         ACCUMULATER                                         
SIZER10  CR    R4,R2        IF PROCESS ALL FROM END TO START                    
         BL    SIZER40                                                          
         OC    0(2,R4),0(R4)                                                    
         BNZ   SIZER40                                                          
         LA    R3,2(R3)     INCREASE ACCUMLATER                                 
         SH    R4,=H'2'     MOVE BACK TWO BYTES AT A TIME                       
         B     SIZER10                                                          
SIZER40  L     R5,DMCB+4                                                        
         SR    R5,R3                                                            
         AH    R5,=H'3'     LENGTH OF STRINGS OF DEMOS + 3BYTE HEADER           
         STC   R5,DMCB+8                                                        
         XIT1                                                                   
******************************************************                          
* FILL IN RECORD LENGTH INFO AND ELEMENT HEADER INFO                            
******************************************************                          
HEADFILL NTR1                                                                   
         LA    R3,3    THERES THREE DEMO HEADER TO FILL                         
         LA    R9,SVODATA                                                       
         USING DRKEY,R9                                                         
         LA    R4,ELEMTAB          TABLE OF ELEMENT CODES                       
         LA    R5,DRFRSTEL                                                      
         LA    R5,MARLNEQ+QHPNMAX+(QHPNAME-QHELEM)(R5)                          
         USING DEMOELD,R5                                                       
HEAD10   LA    R2,DEMOSL                                                        
         XC    DMCB,DMCB                                                        
         ST    R2,DMCB                                                          
         LA    RE,L'DEMOSL                                                      
         ST    RE,DMCB+4                                                        
         GOTO1 SIZER,DMCB                                                       
         LA    RE,DEMOLLN          FORCE THE LENGTH FOR NOW                     
         STC   RE,DELTYPLQ                                                      
         MVI   DELTYPLN,X'42'             USE 42 FOR DECIMALS FOR NOW           
*                                                                               
         MVC   DELTYPL,0(R4)                                                    
         AHI   R4,1                                                             
*                                                                               
         LA    R5,DEMOLLN(R5)                                                   
         BCT   R3,HEAD10                                                        
         ST    R5,DMCB                                                          
         BAS   RE,BLD5E                                                         
*                                                                               
         LR    RE,R5                                                            
         LA    RE,7(RE)                 7 BYTES 5E ELEMENT                      
         MVI   0(RE),0                  TERMINATE RECORD WITH A NULL            
         LA    RF,SVODATA                                                       
         SR    RE,RF                                                            
         LA    RE,1(RE)                   LENGTH OF RECORD                      
         STCM  RE,3,DRRLEN                                                      
         LA    RE,4(RE)                   + 4 BYTE HEADER                       
         STCM  RE,3,SVOUTLEN                                                    
         XIT1                                                                   
******************************************************                          
*  MARKET TYPE ELEMENT                                                          
******************************************************                          
MKTTYPE  NTR1                                                                   
         LA    R5,DRFRSTEL                                                      
         USING MARELEM,R5                                                       
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,DRKMKT                                                     
         XIT1                                                                   
******************************************************                          
*  QUARTER HOUR ELEMENT                                                         
* ON EXIT: RETURNS THE ADDRESS THE QTR ELEMENT                                  
*          SO DEMO KNOWS WHERE TO MOVE INTO                                     
*          DMCB HAS THE ADDRESS                                                 
* 0=AVG 1=LOW 2=HIGH                                                            
******************************************************                          
QTRHOUR  NTR1                                                                   
*                                                                               
         LA    R5,DRFRSTEL                                                      
         LA    R5,MARLNEQ(R5)     ELEMENT AFTER MARKET ELEMENT                  
         USING QHELEM,R5                                                        
         MVI   QHCODE,QHCODEQ                                                   
         MVI   QHELN,QHPNMAX+(QHPNAME-QHELEM)                                   
         MVC   QHPNAME(L'SVDPRTCD-2),SVDPRTCD                                   
         OC    QHPNAME(QHPNMAX),SPACES                                          
         CLI   WORK+1,C'2'                                                      
         BNE   QTR10                                                            
         MVC   QHDAY(1),SVDYQH                                                  
QTR10    CLI   WORK+1,C'0'                                                      
         BNE   QTR20                                                            
         MVC   QHDAY(1),SVDYQH                                                  
         B     QTR30                                                            
QTR20    CLI   WORK+1,C'1'                                                      
         BNE   QTR30                                                            
         MVC   QHDAY(1),SVDYQH                                                  
QTR30    MVC   QHSQH(1),SVDYQH+1                                                
         MVC   QHEQH(1),SVDYQH+1                                                
         MVI   QHWKS,X'2F'                                                      
*                                                                               
         XIT1                                                                   
******************************************************                          
* BUILD 5E ELEMENT  *DMCB HAD ADDRESS TO MOVE 5E ELEM                           
*                    INTO AFTER THE DEMO ELEMENT                                
******************************************************                          
BLD5E    NTR1                                                                   
         L     R2,DMCB                                                          
         MVC   0(2,R2),=X'5E07'                                                 
         MVC   2(3,R2),=C'TTS'                                                  
         MVC   5(2,R2),=X'6201'                                                 
         XIT1                                                                   
******************************************************                          
BLDBSKEY NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         LA    R2,4(R2)                                                         
         USING BSKEY,R2                                                         
         LA    RE,L'BSNDXDA+L'BSKSTAT+L'BSKMAJOR                                
         LA    RE,4(RE)      4 BYTES RECORD LEN                                 
         STCM  RE,3,KEY                                                         
         MVI   BSCODE,BSCODEQU                                                  
         MVI   BSMEDIA,C'T'                                                     
         MVI   BSSRC,C'N'                                                       
         MVC   BSBOOK,DRBOOK                                                    
         MVC   BSBTYP,BOOKTYPE                                                  
         MVI   BSIND,BSINDEQU                                                   
         MVC   BSSTAT,SVMKT                                                     
         MVI   BSSTAT+L'BSSTAT-1,C'T'                                           
         MVC   BSNDXDA+1(2),=X'FFFF'   PASSIVE                                  
         PUT   OUT,KEY                                                          
         XIT1                                                                   
******************************************************                          
BLDMLKEY NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         LA    R2,4(R2)                                                         
         USING MLKEY,R2                                                         
         LA    RE,L'MLNDXDA+L'MLKSTAT+L'MLKMAJOR                                
         LA    RE,4(RE)      4 BYTES RECORD LEN                                 
         STCM  RE,3,KEY                                                         
         MVI   MLCODE,MLCODEQU                                                  
         MVI   MLMEDIA,C'T'                                                     
         MVI   MLSRC,C'N'                                                       
         MVC   MLBOOK,DRBOOK                                                    
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,DRKMKT                                                    
         MVC   MLSTAT,SVMKT                                                     
         MVC   MLBTYP,BOOKTYPE                                                  
         MVI   MLSTAT+L'MLSTAT-1,C'T'                                           
         MVC   MLNDXDA+1(2),=X'FFFF'   PASSIVE                                  
         PUT   OUT,KEY                                                          
         XIT1                                                                   
******************************************************                          
BLDSBKEY NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         LA    R2,4(R2)                                                         
         USING SBKEY,R2                                                         
         LA    RE,L'SBNDXDA+L'SBKSTAT+L'SBKMAJOR                                
         LA    RE,4(RE)      4 BYTES RECORD LEN                                 
         STCM  RE,3,KEY                                                         
         MVI   SBCODE,SBCODEQU                                                  
         MVI   SBMEDIA,C'T'                                                     
         MVI   SBSRC,C'N'                                                       
         MVC   SBBTYP,BOOKTYPE                                                  
         MVC   SBBOOK,DRBOOK                                                    
         XC    SBBOOK,=X'FFFF'                                                  
         MVC   SBRMKT,DRKMKT                                                    
         MVC   SBSTAT,SVMKT                                                     
         MVI   SBSTAT+L'SBSTAT-1,C'T'                                           
         MVC   SBNDXDA+1(2),=X'FFFF'   PASSIVE                                  
         PUT   OUT,KEY                                                          
         XIT1                                                                   
******************************************************                          
*                                                                               
EXIT     XBASE                                                                  
*                                                                               
         EJECT                                                                  
         DC    CL8'**WORK**'                                                    
BOOKTYPE DS    CL1                                                              
DMCB     DS    6F                                                               
GETSW    DS    C                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
FIRSTREC DS    CL1                                                              
WORK     DS    CL20                                                             
SVMKT    DS    XL4                                                              
SVDPRTCD DS    CL4                                                              
SVDYQH   DS    CL2                                                              
ELEMADDR DS    A                                                                
KEY      DS    CL50                                                             
         DC    CL8'**IREC**'                                                    
INREC    DS    CL800                                                            
       ++INCLUDE DESQADTGT                                                      
         LTORG                                                                  
*                                                                               
************************************************                                
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
IN       DCB   DDNAME=FILEIN,                                          X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=00424,                                            X        
               MACRF=GM                                                         
*                                                                               
OUT      DCB   DDNAME=OTAPE,                                           X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               BLKSIZE=05000,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
ELEMTAB  DS    0XL1                                                             
         DC    XL1'41'             LOW                                          
         DC    XL1'43'             AVG                                          
         DC    XL1'45'             HIGH                                         
         DC    XL1'FF'                                                          
*                                                                               
QTRTAB   DS    0CL2                                                             
         DC    CL1'1',AL1(2)                                                    
         DC    CL1'2',AL1(5)                                                    
         DC    CL1'3',AL1(7)                                                    
         DC    CL1'4',AL1(11)                                                   
         DC    AL1(0)                                                           
*                                                                               
DYPRTTAB DS    0CL6                                                             
         DC    CL4'PR  ',AL1(0),AL1(5)                                          
         DC    CL4'LF  ',AL1(1),AL1(6)                                          
         DC    CL4'EF  ',AL1(2),AL1(3)                                          
         DC    CL4'EN  ',AL1(3),AL1(7)                                          
         DC    CL4'LN  ',AL1(4),AL1(8)                                          
         DC    CL4'DA  ',AL1(5),AL1(2)                                          
         DC    CL4'PA  ',AL1(6),AL1(4)                                          
         DC    CL4'EM  ',AL1(8),AL1(1)                                          
         DC    CL2'ZZ'                                                          
*                                                                               
DEMOTAB  DS    0CL9                                                             
         DC    CL8'M18+',AL1(6)                                                 
         DC    CL8'M1834',AL1(8)                                                
         DC    CL8'M1849',AL1(8)                                                
         DC    CL8'M2549',AL1(8)                                                
         DC    CL8'M2554',AL1(8)                                                
         DC    CL8'M35+',AL1(6)                                                 
         DC    CL8'M50+',AL1(6)                                                 
         DC    CL8'M1824',AL1(8)                                                
         DC    CL8'M2534',AL1(8)                                                
         DC    CL8'M3554',AL1(8)                                                
         DC    CL8'W18+',AL1(6)                                                 
         DC    CL8'W1834',AL1(8)                                                
         DC    CL8'W1849',AL1(8)                                                
         DC    CL8'W2549',AL1(8)                                                
         DC    CL8'W2554',AL1(8)                                                
         DC    CL8'W35+',AL1(6)                                                 
         DC    CL8'W50+',AL1(6)                                                 
         DC    CL8'W1824',AL1(8)                                                
         DC    CL8'W2534',AL1(8)                                                
         DC    CL8'W3554',AL1(8)                                                
         DC    CL8'A18+',AL1(6)                                                 
         DC    CL8'A1834',AL1(8)                                                
         DC    CL8'A1849',AL1(8)                                                
         DC    CL8'A2549',AL1(8)                                                
         DC    CL8'A2554',AL1(8)                                                
         DC    CL8'A35+',AL1(6)                                                 
         DC    CL8'A50+',AL1(6)                                                 
         DC    CL8'A1824',AL1(8)                                                
         DC    CL8'A25+',AL1(6)                                                 
         DC    CL8'A3554',AL1(8)                                                
         DC    CL8'A3564',AL1(8)                                                
         DC    CL8'P1224',AL1(8)                                                
         DC    CL8'P1234',AL1(8)                                                
         DC    CL8'P1249',AL1(8)                                                
         DC    CL8'K211',AL1(7)                                                 
         DC    CL8'TEENS',AL1(7)                                                
         DC    CL8'TVHH',AL1(7)                                                 
         DC    CL8'W3549',AL1(8)                                                
         DC    CL8'M3549',AL1(8)                                                
         DC    CL8'A3549',AL1(8)                                                
         DC    CL8'W1850',AL1(8)                                                
         DC    CL8'M1850',AL1(8)                                                
         DC    CL8'A1850',AL1(8)                                                
         DC    CL8'W1854',AL1(8)                                                
         DC    CL8'M1854',AL1(8)                                                
         DC    CL8'A1854',AL1(8)                                                
         DC    CL8'A2534',AL1(8)                                                
         DC    CL8'W5054',AL1(8)                                                
         DC    CL8'M5054',AL1(8)                                                
         DC    CL8'A5054',AL1(8)                                                
         DC    CL8'W55+',AL1(6)                                                 
         DC    CL8'M55+',AL1(6)                                                 
         DC    CL8'A55+',AL1(6)                                                 
         DC    CL8'A65+',AL1(6)                                                 
         DC    CL8'W3564',AL1(8)                                                
         DC    CL8'M3564',AL1(8)                                                
         DC    CL8'W25+',AL1(6)                                                 
         DC    CL8'M25+',AL1(6)                                                 
         DC    CL8'M2134',AL1(8)                                                
         DC    CL8'P12+',AL1(6)                                                 
         DC    CL8'P1254',AL1(8)                                                
         DC    CL8'W1224',AL1(8)                                                
         DC    CL8'M1224',AL1(8)                                                
         DC    CL8'WKGWM',AL1(8)                                                
         DC    CL8'A21+',AL1(6)                                                 
         DC    CL8'A2134',AL1(8)                                                
         DC    CL8'A2149',AL1(8)                                                
         DC    CL8'A2154',AL1(8)                                                
         DC    CL8'M21+',AL1(6)                                                 
         DC    CL8'M2149',AL1(8)                                                
         DC    CL8'M2154',AL1(8)                                                
         DC    CL8'W21+',AL1(6)                                                 
         DC    CL8'W2134',AL1(8)                                                
         DC    CL8'W2149',AL1(8)                                                
         DC    CL8'W2154',AL1(8)                                                
DEMTABEN EQU   *                                                                
         DC    XL2'0000'                                                        
FRSTFLG  DS    CL1                                                              
         DS    0F                                                               
         DC    CL8'*OUTREC*'                                                    
SVOUTREC DS    0XL1004                                                          
SVOUTLEN DS    XL4                                                              
SVODATA  DS    XL(L'OUTDATA)                                                    
OUTKEY   DS    CL(L'DRKMAJOR+L'DRKMINOR)                                        
OUTDATA  DS    XL1000                                                           
         EJECT                                                                  
** ++INCLUDE DDDPRINT                                                           
** ++INCLUDE DEDEMFILE                                                          
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
*                                                                               
*                                                                               
DEMOELD  DSECT                                                                  
DELTYPL  DS    XL1           ELEMENT TYPE L,A,H                                 
DELTYPLQ DS    XL1           TOTAL ELEMENT LENGTH                               
DELTYPLN DS    XL1           PRECISION                                          
DEMOLHDQ EQU   *-DEMOELD                                                        
DEMOSL   DS    CL((DEMTABEN-DEMOTAB)/9*2)                                       
DEMOLLN  EQU   *-DEMOELD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DESQADTP  12/18/18'                                      
         END                                                                    
