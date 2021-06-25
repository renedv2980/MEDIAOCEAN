*          DATA SET DESQAQN    AT LEVEL 003 AS OF 01/02/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESQAQNA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE LOADER                                                                 
*INCLUDE DATCON                                                                 
SQADTP   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SQADTP,=V(REGSAVE),R6                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         OPEN  (IN,(INPUT))                                                     
         MVI   GETSW,1                                                          
         OPEN  (OUT,(OUTPUT))                                                   
         LARL  RE,OUTDATA                                                       
         XCEF  (RE),1500                                                        
         MVI   FIRSTREC,C'Y'                                                    
*                                                                               
GET      GET   IN,INREC                                                         
         CLC   =C'DATA',INREC+4                                                 
         BE    GET                                                              
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
*                                                                               
         LARL  R9,OUTDATA       R2 POINTS TO THE OUTDATA                        
         USING DRKEY,R9                                                         
         MVI   DRCODE,DRCODEQU                                                  
         MVI   DRMEDIA,C'T'                                                     
         MVI   DRSRC,C'N'                                                       
         XC    DUB,DUB                                                          
***************************************************************                 
* MARKET NAME                                                                   
*   ex. "ABILENE-SWEETWATER"                                                    
*   FIND MARKET NUMBER FROM NIELSEN MARKET TABLE                                
*   THEN LOOK UP CORRESPONDING SQAD MARKET NUMBER                               
***************************************************************                 
M_N      LA    R2,P                                                             
         LA    R0,M_NQ             MARKET NAME                                  
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         OC    WORK+1(L'SRTM_N),SPACES  SPACE PAD & CAPITALIZE                  
*                                                                               
         LARL  R1,SQADMKT          A(SQAD MARKET TABLE)                         
M_N10    CLI   0(R1),X'FF'         EOT?                                         
         BNZ   *+6                                                              
         DC    H'00'                                                            
         CLC   WORK+1(L'SRTM_N),2(R1)                                           
         BE    *+12                                                             
         AHI   R1,L'SQADMKT                                                     
         B     M_N10                                                            
*                                                                               
         MVC   DRKMKT,0(R1)        SET MARKET NUMBER                            
         EDIT  (B2,DRKMKT),(4,DRSTAT),ALIGN=RIGHT,FLOAT=0,FILL=0                
         MVI   DRSTAT+L'DRSTAT-1,C'T'                                           
*                                                                               
***************************************************************                 
* BOOKTYPE                                                                      
***************************************************************                 
         MVI   BOOKTYPE,C'Q'    DEFAULT BOOKTYPE IS 'Q'                         
         MVC   DRBTYP,BOOKTYPE                                                  
***************************************************************                 
* QUARTER NAME                                                                  
*   BYTES 0-3 = YEAR                                                            
*   BYTES 5-15 = QUARTER  (BYTE 5 IS ALWAYS QUARTER NUMBER)                     
*   ex. "2015 4th Quarter"                                                      
***************************************************************                 
Q_N      LA    R2,P                POINT TO input                               
         LA    R0,Q_NQ             QUARTER NAME                                 
         BAS   RE,PARSER                                                        
         LA    RE,4                YEAR IS 4 BYTES                              
         XC    WORK+1(2),WORK+1    LETS GET RID OF CENTURY FOR NOW              
         XC    DUB,DUB                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RF,DUB                                                           
         STC   RF,DRKMKT           STORE BOOK YEAR                              
*                                                                               
         XC    DUB,DUB                                                          
         LA    RE,1                MONTH (QTR NUM) IS 1 BYTE                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+6(0)       A(QTR NUM)                                   
         CVB   RF,DUB                                                           
         STC   RF,DRKMKT+1         STORE BOOK MONTH                             
*                                                                               
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
         AR    RE,R0               YEAR Y2K                                     
         STC   RE,DRKMKT                                                        
*                                                                               
NOTY2K   DS    0H                                                               
******************************************************************              
* DATA RELEASE                                                                  
*   BYTES 0-3 = YEAR                                                            
*   BYTES 4-5 = MONTH                                                           
*   BYTES 6-7 = DAY                                                             
*   ex. "20151201"                                                              
******************************************************************              
D_R      LA    R2,P                POINT TO input                               
         LA    R0,D_RQ             DATA RELEASE                                 
         BAS   RE,PARSER                                                        
         LA    RE,4                YEAR IS 4 BYTES                              
         XC    WORK+1(2),WORK+1    LETS GET RID OF CENTURY FOR NOW              
         XC    DUB,DUB                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RF,DUB                                                           
         STC   RF,DRBOOK           STORE BOOK YEAR                              
*                                                                               
         XC    DUB,DUB                                                          
         LA    RE,2                MONTH IS 2 BYTES                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+5(0)       A(MONTH)                                     
         CVB   RF,DUB                                                           
         STC   RF,DRBOOK+1         STORE BOOK MONTH                             
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
         AR    RE,R0               YEAR Y2K                                     
         STC   RE,DRBOOK                                                        
*                                                                               
NOTY2KBK DS    0H                                                               
         XC    DRBOOK,=X'FFFF'                                                  
**********************************************************                      
* DAYPART NAME                                                                  
*   ex. "Prime Time"                                                            
**********************************************************                      
D_N      LA    R2,P                                                             
         LA    R0,D_NQ             DAYPART NAME                                 
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         OC    WORK+1(L'SRTD_N),SPACES  SPACE PAD & CAPITALIZE                  
         LA    R4,DYPTAB           DAYPART TABLE                                
D_N10    CLC   =C'ZZ',0(R4)        FOUND IT?                                    
         JE    *+2                 NO - DUMP                                    
         CLC   6(L'SRTD_N,R4),WORK+1                                            
         BE    *+12                                                             
         AHI   R4,DYPTABQ                                                       
         B     D_N10                                                            
*                                                                               
         MVC   SVDPRTCD(4),0(R4)   TWO CHAR DAYPART                             
         MVI   DRHIGHD,X'95'                                                    
         MVC   SVDYQH(2),DRHIGHD                                                
         MVC   DRHIQHR,5(R4)       DAYPART EQUATE                               
**********************************************************                      
*                                                                               
**********************************************************                      
         GOTO1 MKTTYPE                                                          
         GOTO1 QTRHOUR                                                          
*                                                                               
         CLI   FIRSTREC,C'Y'                                                    
         BE    OUT50               NO SVODATA TO COMPARE YET                    
*                                                                               
         LARL  RF,OUTDATA                                                       
         CLC   0(L'DRKMAJOR+L'DRKMINOR,RF),SVODATA                              
         BE    OUT50               SAME RECORD, DIFFERENT DEMO                  
*                                                                               
         BAS   RE,HEADFILL         FILL IN ELEMENT HEADER INFO                  
         PUT   OUT,SVOUTREC        PUT OUT OLD AND DO NEW INREC                 
         LARL  RF,OUTDATA                                                       
         CLC   3(L'DRSTAT+L'DRBOOK,RF),SVODATA+3                                
         BE    OUT50               NEED TO BLD MKEYS                            
         MVC   SVMKT,SVODATA+3     THE MKT BEFORE RECORD CHANGE                 
         GOTO1 BLDBSKEY                                                         
         GOTO1 BLDMLKEY                                                         
         GOTO1 BLDSBKEY                                                         
*                                                                               
OUT50    LARL  R9,OUTDATA          R2 POINTS TO THE OUTDATA                     
         USING DRKEY,R9                                                         
         LA    R8,0                                                             
**********************************************************                      
* DEMO CODE                                                                     
*   BYTES 0-7 = DEMO CODE                                                       
*   ex. "AD18+"                                                                 
**********************************************************                      
D_C      LA    R2,P                                                             
         LA    R0,D_CQ             DEMO CODE                                    
         BAS   RE,PARSER                                                        
         MVI   WORK,L'SRTD_C                                                    
         OC    WORK+1(L'SRTD_C),SPACES                                          
*                                                                               
         LA    R4,DEMOTAB                                                       
D_C10    CLC   =X'0000',0(R4)      FOUND IT?                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),WORK+1     COMPARE DEMOS ON OUTPUT LINE TO TABLE         
         BE    SETELEM                                                          
         LA    R4,L'DEMOTAB(R4)                                                 
         LA    R8,1(R8)                                                         
         B     D_C10                                                            
**********************************************************                      
* SET R5 TO FIRST AVAILABLE ELEMENT TO ADD                                      
**********************************************************                      
SETELEM  MH    R8,=H'2'            HOW FAR FROM THE DEMOS ELEMENT               
         LA    R5,DRFRSTEL         1st demo elem is past mkt elem               
         LA    R5,MARLNEQ(R5)      past qtr hour elem                           
         LA    R5,QHPNMAX+(QHPNAME-QHELEM)(R5)                                  
         USING DEMOELD,R5                                                       
**********************************************************                      
* DEMO VALUE: LOW CPP                                                           
**********************************************************                      
L_C      LA    R2,P                                                             
         LA    R0,L_CQ             LOW CPP                                      
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,L_CELQ                                                   
         LR    RE,R5                                                            
         LA    RE,DEMOLLN(RE)                                                   
         ST    RE,DMCB                                                          
*                                                                               
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: AVG CPP                                                           
**********************************************************                      
A_C      LA    R2,P                                                             
         LA    R0,A_CQ             AVG CPP                                      
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,A_CELQ                                                   
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: HIGH CPP                                                          
**********************************************************                      
H_C      LA    R2,P                                                             
         LA    R0,H_CQ             HIGH CPP                                     
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,H_CELQ                                                   
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: LOW DMA CPM                                                       
**********************************************************                      
L_D_C    LA    R2,P                                                             
         LA    R0,L_D_CQ           LOW DMA CPM                                  
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,L_D_CELQ                                                 
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: LOW TSA CPM                                                       
**********************************************************                      
L_T_C    LA    R2,P                                                             
         LA    R0,L_T_CQ           LOW TSA CPM                                  
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,L_T_CELQ                                                 
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: AVG DMA CPM                                                       
**********************************************************                      
A_D_C    LA    R2,P                                                             
         LA    R0,A_D_CQ           AVG DMA CPM                                  
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,A_D_CELQ                                                 
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: AVG TSA CPM                                                       
**********************************************************                      
A_T_C    LA    R2,P                                                             
         LA    R0,A_T_CQ           AVG TSA CPM                                  
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,A_T_CELQ                                                 
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: HIGH DMA CPM                                                      
**********************************************************                      
H_D_C    LA    R2,P                                                             
         LA    R0,H_D_CQ           HIGH DMA CPM                                 
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,H_D_CELQ                                                 
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
*                                                                               
         LA    R5,DEMOLLN(R5)      POINT TO NEXT ELEM SLOT                      
**********************************************************                      
* DEMO VALUE: HIGH TSA CPM                                                      
**********************************************************                      
H_T_C    LA    R2,P                                                             
         LA    R0,H_T_CQ           HIGH TSA CPM                                 
         BAS   RE,PARSER                                                        
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
*                                                                               
         MVI   DELTYPL,H_T_CELQ                                                 
         LA    R7,DEMOSL                                                        
         AR    R7,R8               R8 IS THE # OF DEMOS FROM BEGINNING          
         MVC   0(2,R7),HALF        MOVE IN DEMO VALUE TO THE DEMO ELEM          
**********************************************************                      
* SAVE RECORD AND GET NEXT RECORD                                               
**********************************************************                      
         LA    RF,L'OUTDATA                                                     
         LR    R1,RF                                                            
         LA    R0,SVODATA     VARIABLE LENGTH                                   
         LARL  RE,OUTDATA                                                       
         MVCL  R0,RE                                                            
*                                                                               
         MVI   FIRSTREC,C'N'                                                    
         B     GET                                                              
OUT200   LA    RF,L'SVODATA                                                     
         LR    R1,RF                                                            
         LARL  R0,OUTDATA     VARIABLE LENGTH                                   
         LA    RE,SVODATA                                                       
         MVCL  R0,RE                                                            
*                                                                               
OUT230   LARL  RE,OUTDATA                                                       
         XCEF  (RE),1500                                                        
         MVI   FRSTFLG,C'N'                                                     
         B     GET                                                              
*                                                                               
ENDJOB   BAS   RE,HEADFILL                                                      
         PUT   OUT,SVOUTREC                                                     
         LARL  RF,OUTDATA                                                       
         MVC   SVMKT,3(RF)         THE LAST RECORD TYPE MKT                     
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
*****************************************************************               
*                                                                               
*****************************************************************               
* PARSER                                                                        
* ON ENTRY R2-POINTS TO BEGINNING OF WHERE TO START PARSING                     
* ON EXIT  WORK  HAS THE LENGTH OF FIELD                                        
*          WORK+1  HAS THE PARSED FIELD                                         
*                                                                               
PARSER   NTR1                                                                   
         LA    RE,WORK+1                                                        
         XC    WORK,WORK                                                        
         LA    R5,1                STARTING WITH FIRST FIELD                    
         LA    R7,0                FIELD LENGTH                                 
         MVI   PARSEFLG,0                                                       
*                                                                               
         CHI   R0,H_D_CQ           HIGH DMA CPM                                 
         BE    PAR02                                                            
         CHI   R0,H_T_CQ           HIGH TSA CPM                                 
         BE    PAR02                                                            
         CHI   R0,A_D_CQ           AVG DMA CPM                                  
         BE    PAR02                                                            
         CHI   R0,A_T_CQ           AVG TSA CPM                                  
         BE    PAR02                                                            
         CHI   R0,L_D_CQ           LOW DMA CPM                                  
         BE    PAR02                                                            
         CHI   R0,L_T_CQ           LOW TSA CPM                                  
         BNE   *+8                                                              
PAR02    OI    PARSEFLG,PFSKIPDQ   SKIP DECIMAL POINT                           
*                                                                               
PAR05    CR    R5,R0               THIS THE FIELD WE LOOKING FOR?               
         BL    PAR10                                                            
*                                                                               
         CLI   0(R2),C'"'          CURRENT FIELD " DELIMITED?                   
         BNE   *+12                                                             
         OI    PARSEFLG,PFQUOTEQ                                                
         LA    R2,1(R2)                                                         
*                                                                               
PAR06    TM    PARSEFLG,PFSKIPDQ   SKIP DECIMAL POINT?                          
         BZ    PAR08                                                            
*                                                                               
         CLI   0(R2),C'.'          DO NOT INCLUDE DECIMALS                      
         BNE   PAR08                                                            
         LA    R2,1(R2)                                                         
         MVC   0(1,RE),0(R2)                                                    
*                                                                               
         CLI   1(R2),C','          ONLY 1 DEC PRECISION (EX. 10.4)?             
         BNE   PAR08                                                            
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'0'          MAKE IT 2 DEC PRECISION (EX. 10.40)          
         LA    R7,1(R7)                                                         
         B     *+10                                                             
PAR08    MVC   0(1,RE),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    R7,1(R7)                                                         
         STC   R7,WORK             KEEP TRACK OF LENGTH                         
*                                                                               
         CHI   R0,R_TQ             LAST FIELD?                                  
         BNE   *+12                                                             
         CLI   0(R2),0             END OF FIELD YET?                            
         BE    PARXX                                                            
*                                                                               
         MVI   BYTE,C','                                                        
         TM    PARSEFLG,PFQUOTEQ                                                
         BZ    *+8                                                              
         MVI   BYTE,C'"'           CURRENT FIELD IS " DELIMITED                 
*                                                                               
         CLC   BYTE,0(R2)          END OF FIELD YET?                            
         BNE   PAR06                                                            
         B     PARXX                                                            
*                                                                               
PAR10    CLI   0(R2),C'"'          THIS FIELD " DELIMITED?                      
         BNE   PAR12                                                            
PAR11    LA    R2,1(R2)                                                         
         CLI   0(R2),C'"'                                                       
         BNE   PAR11                                                            
         LA    R2,2(R2)            BUMP PAST " AND ,                            
         LA    R5,1(R5)                                                         
         B     PAR05                                                            
*                                                                               
PAR12    LA    R2,1(R2)            BUMP THROUGH TILL NEXT FIELD                 
         CLI   0(R2),C','                                                       
         BNE   PAR10                                                            
         LA    R5,1(R5)                                                         
         LA    R2,1(R2)                                                         
         B     PAR05                                                            
*                                                                               
PARXX    XIT1                                                                   
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
         LA    R3,NUMDEMS          9 DEMO HEADERS TO FILL                       
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
         GOTO1 =V(DATCON),DMCB,(5,0),(2,MARDATE)                                
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
         MVC   QHDAY(1),SVDYQH                                                  
         MVC   QHSQH(1),SVDYQH+1                                                
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
WORK     DS    CL(SRTKDQ)                                                       
BYTE     DS    X                                                                
SVMKT    DS    XL4                                                              
SVDPRTCD DS    CL4                                                              
SVDYQH   DS    CL2                                                              
ELEMADDR DS    A                                                                
*                                                                               
PARSEFLG DS    X                                                                
PFQUOTEQ EQU   X'80'                                                            
PFSKIPDQ EQU   X'40'               SKIP DECIMAL POINT                           
*                                                                               
KEY      DS    CL50                                                             
         DC    CL8'**IREC**'                                                    
INREC    DS    CL800                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
IN       DCB   DDNAME=FILEIN,DSORG=PS,EODAD=ENDJOB,RECFM=VB,MACRF=GM,  X        
               LRECL=00424                                                      
*                                                                               
OUT      DCB   DDNAME=OTAPE,DSORG=PS,RECFM=VB,LRECL=01504,MACRF=PM,    X        
               BLKSIZE=05000                                                    
*                                                                               
ELEMTAB  DS    0XL1                                                             
         DC    AL1(L_CELQ)         LOW_CPP                                      
         DC    AL1(A_CELQ)         AVG_CPP                                      
         DC    AL1(H_CELQ)         HIGH_CPP                                     
         DC    AL1(L_D_CELQ)       LOW_DMA_CPM                                  
         DC    AL1(L_T_CELQ)       LOW_TSA_CPM                                  
         DC    AL1(A_D_CELQ)       AVG_DMA_CPM                                  
         DC    AL1(A_T_CELQ)       AVG_TSA_CPM                                  
         DC    AL1(H_D_CELQ)       HIGH_DMA_CPM                                 
         DC    AL1(H_T_CELQ)       HIGH_TSA_CPM                                 
         DC    XL1'FF'                                                          
NUMDEMS  EQU   9                                                                
*                                                                               
       ++INCLUDE DESQADTABS                                                     
*                                                                               
FRSTFLG  DS    CL1                                                              
         DS    0F                                                               
         DC    CL8'*OUTREC*'                                                    
SVOUTREC DS    0XL1504                                                          
SVOUTLEN DS    XL4                                                              
SVODATA  DS    XL(L'OUTDATA)                                                    
OUTKEY   DS    CL(L'DRKMAJOR+L'DRKMINOR)                                        
OUTDATA  DS    XL1500                                                           
       ++INCLUDE DESQADMKTT                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
*                                                                               
*  FIELD # EQUATES                                                              
*                                                                               
D_RQ     EQU   1                   DATA RELEASE                                 
M_NQ     EQU   2                   MARKET NAME                                  
D_NQ     EQU   3                   DAYPART NAME                                 
D_CQ     EQU   4                   DEMO CD                                      
Q_NQ     EQU   5                   QUARTER NAME                                 
H_CQ     EQU   6                   HIGH CPP                                     
H_D_CQ   EQU   7                   HIGH DMA CPM                                 
H_T_CQ   EQU   8                   HIGH TSA CPA                                 
A_CQ     EQU   9                   AVH CPP                                      
A_D_CQ   EQU   10                  AVG DMA CPM                                  
A_T_CQ   EQU   11                  AVG TSA CPA                                  
L_CQ     EQU   12                  LOW CPP                                      
L_D_CQ   EQU   13                  LOW DMA CPM                                  
L_T_CQ   EQU   14                  LOW TSA CPA                                  
P_DQ     EQU   15                  POP DMA                                      
R_DQ     EQU   16                  RANK DMA                                     
P_TQ     EQU   17                  POP TSA                                      
R_TQ     EQU   18                  RANK TSA                                     
*                                                                               
SRTKD    DSECT                                                                  
SRTKY    DS    CL1                 USED FOR INPUT LENGTH                        
SRTM_N   DS    CL40                MARKET NAME                                  
         DS    CL1                                                              
SRTQ_N   DS    CL17                QUARTER NAME                                 
         DS    CL1                                                              
SRTD_N   DS    CL15                DAYPART NAME                                 
         DS    CL1                                                              
SRTD_C   DS    CL8                 DEMO CD                                      
         DS    CL1                                                              
SRTD_R   DS    CL8                 DATA RELEASE                                 
         DS    CL1                                                              
SRTH_C   DS    CL8                 HIGH CPP                                     
         DS    CL1                                                              
SRTH_D_C DS    CL8                 HIGH DMA CPM                                 
         DS    CL1                                                              
SRTH_T_C DS    CL8                 HIGH TSA CPA                                 
         DS    CL1                                                              
SRTA_C   DS    CL8                 AVG CPP                                      
         DS    CL1                                                              
SRTA_D_C DS    CL8                 AVG DMA CPA                                  
         DS    CL1                                                              
SRTA_T_C DS    CL8                 AVG TSA CPM                                  
         DS    CL1                                                              
SRTL_C   DS    CL8                 LOW CPP                                      
         DS    CL1                                                              
SRTL_D_C DS    CL8                 LOW DMA CPA                                  
         DS    CL1                                                              
SRTL_T_C DS    CL8                 LOW TSA CPM                                  
         DS    CL1                                                              
SRTP_D   DS    CL8                 POP DMA                                      
         DS    CL1                                                              
SRTR_D   DS    CL8                 RANK DMA                                     
         DS    CL1                                                              
SRTP_T   DS    CL8                 POP TSA                                      
         DS    CL1                                                              
SRTR_T   DS    CL8                 RANK TSA                                     
SRTKDQ   EQU   *-SRTKY                                                          
*                                                                               
*                                                                               
DEMOELD  DSECT                                                                  
DELTYPL  DS    XL1                 ELEMENT TYPE A,B,C,H,I,J,L,M,N               
DELTYPLQ DS    XL1           TOTAL ELEMENT LENGTH                               
DELTYPLN DS    XL1           PRECISION                                          
DEMOLHDQ EQU   *-DEMOELD                                                        
DEMOSL   DS    CL((DEMTABEN-DEMOTAB)/9*2)                                       
DEMOLLN  EQU   *-DEMOELD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DESQAQN   01/02/19'                                      
         END                                                                    
