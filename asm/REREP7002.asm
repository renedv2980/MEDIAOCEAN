*          DATA SET REREP7002  AT LEVEL 175 AS OF 02/11/15                      
*PHASE RE7002A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE REGENSTC                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'MODULE TO PRINT STATION LISTING'                                
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP7002 (RE7002) --- STATION LISTING REPORT            *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* MAR19/97 (BU ) --- FIX AFFILIATE FILTER BUG                     *             
*                                                                 *             
* SEP09/97 (BU ) --- TEST VERSION:  DEC/97 STATIONS FILTERING     *             
*                                                                 *             
* JAN28/98 (JRD) --- 4K CONTRACTS                                 *             
*                                                                 *             
* JAN08/01 (ABOB)--- STATION ID NUMBER                            *             
*                                                                 *             
* MAY20/02 (HQ ) --- STATION EMAIL ADDRESS                        *             
*                                                                 *             
* APR01/03 (BU ) --- ADD UID TO PRINTOUT                          *             
*                                                                 *             
* AUG08/06 (BU ) --- FIX STATION ACTIVITY FILTER BUG              *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                    ***  END TOMBSTONE  ***                      *             
*******************************************************************             
*                                                                               
RE7002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7002,R8,R9,RR=R5                                           
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         XC    MYFLAG,MYFLAG                                                    
*                                                                               
         EJECT                                                                  
*              SET UP HEADLINES                                                 
         CLI   MODE,REQFRST                                                     
         BNE   ST2                                                              
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',RSTAREC,0                                          
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
         L     R2,VXRQNUM                                                       
         L     R4,VXRQCARD         POINT TO THE CARDS AREA                      
         MVC   QCARD2(80),SPACES   CLEAR 2ND CARD                               
         CLI   0(R2),2             TWO CARDS?                                   
         BL    CARD0010                                                         
         MVC   QCARD2(80),80(R4)   SAVE SECOND CARD                             
CARD0010 EQU   *                                                                
         CLI   SORTSET,C'Y'        HAS SORT ALREADY BEEN SET?                   
         BE    CARD0015            YES                                          
         BAS   RE,SETSORT                                                       
CARD0015 EQU   *                                                                
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         L     R2,ASPACND4         START OF TABLE                               
ELEM     CLI   0(R2),X'07'         GROUP/SUB-GROUP ELEMENT                      
         BE    FND7                                                             
         SR    R4,R4                                                            
         IC    R4,1(R2)            FIND 07 ELEMENT                              
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R4                                                            
         B     ELEM                                                             
FND7     ST    R2,BGLK                                                          
         B     EXIT                                                             
         EJECT                                                                  
ST2      CLI   MODE,PROCSTAT                                                    
         BNE   STA40                                                            
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0010            NO                                           
         MVC   P+1(14),=C'STATION INPUT:'                                       
         MVC   P+20(27),RSTAREC                                                 
         GOTO1 REPORT                                                           
TEST0010 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   QOPTION3,C'X'       CLOSED OUT STATION LISTING?                  
         BNE   ST20020             NO                                           
         MVI   RCSUBPRG,4          YES - SET UP BY 4                            
ST20020  EQU   *                                                                
         SPACE 1                                                                
*              PROCESS STATION RECORDS                                          
*  QOPTION1 - A=PRINT ACTIVE STATIONS ONLY  (DEFAULT)                           
*             I=PRINT INACTIVE STATIONS ONLY                                    
*             B=PRINT BOTH ACTIVE AND INACTIVE STATIONS                         
*                                                                               
*  QOPTION2 - S=PRINT STATION SEQUENCE ONLY                                     
*             M=PRINT MARKET SEQUENCE ONLY                                      
*             B=PRINT BOTH STATION AND MARKET SEQUENCE  (DEFAULT)               
*             D=PRINT FULL DETAIL                                               
*             A=PRINT FULL DETAIL WITH ACTIVITY                                 
*             C=PRINT COMBO STATIONS LISTING                                    
*             N=PRINT STATION SEQUENCE WITH MARKET CODE/NAME                    
*             #=PRINT STATION ID NUMBER SEQUENCE                                
*             E=PRINT STATION EMAIL ADDRESS                                     
*                                                                               
*  QOPTION3 - C=CREATE DATE FILTER                                              
*             L=LAST UPDATE FILTER                                              
*             X=CLOSED THRU DATE FILTER                                         
*                                                                               
ST3      EQU   *                                                                
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0015            NO                                           
         MVC   P+1(14),=C'GROUP CHECK:  '                                       
         MVC   P+20(02),QGROUP                                                  
         MVI   P+22,C'/'                                                        
         MVC   P+23(02),RSTAGRUP                                                
         GOTO1 REPORT                                                           
TEST0015 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         CLC   QGROUP(2),SPACES         ALL GROUPS                              
         BE    ALLQSB                                                           
         CLC   RSTAGRUP(1),QGROUP                                               
         BNE   EXIT                                                             
         CLI   QSBGROUP,X'40'                                                   
         BE    ALLQSB                                                           
         CLC   RSTAGRUP+1(1),QSBGROUP                                           
         BNE   EXIT                                                             
ALLQSB   EQU   *                                                                
         LA    R6,QCARD2                                                        
         USING QREC2,R6                                                         
         CLC   Q2AFFIL,SPACES      FILTER ON AFFILIATE?                         
         BE    ALLQSB08            NO                                           
         CLC   RSTAAFFL,Q2AFFIL    YES - AFFILIATE MATCH                        
         BNE   EXIT                NO  - SKIP IT                                
         DROP  R6                                                               
         SPACE 1                                                                
*                                                                               
**** FILTER ON CREATE DATE OR LAST UPDATE DATE?                                 
*                                                                               
ALLQSB08 EQU   *                                                                
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TESTA015            NO                                           
         MVC   P+1(14),=C'OTP3  CHECK:  '                                       
         MVC   P+20(01),QOPTION3                                                
         MVI   P+22,C'/'                                                        
         GOTO1 REPORT                                                           
TESTA015 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         CLI   QOPTION3,C' '       CREATE/LAST UPDTE/CLOSED THRU?               
         BE    ALLQSB50            NO CONTINUE                                  
         CLI   QOPTION3,C'C'       CREATE DATE FLTR?                            
         BNE   ALLQSB09            NO, CHECK LAST UPDTE                         
*                                                                               
**** FILTER ON CREATE DATE, FLTR MUST BE ON DATE ADDED                          
*                                                                               
         BAS   RE,DAT2FILT         YES, FILTER ON CREAT DTE FLD                 
         BNE   EXIT                NO  - SKIP IT                                
         B     ALLQSB50                                                         
*                                                                               
**** FILTER ON LAST UPDATE DATE                                                 
*                                                                               
ALLQSB09 EQU   *                                                                
         CLI   QOPTION3,C'L'       LAST UPDATE FLTR?                            
         BNE   ALLQSB15            NO, CHECK CLOSED THRU                        
         BAS   RE,DATEFILT         YES, FILTER ON ACTIV DATE FLD                
         BNE   EXIT                NO  - SKIP IT                                
         B     ALLQSB50                                                         
*                                                                               
ALLQSB15 EQU   *                                                                
         CLI   QOPTION3,C'X'       CLOSED THRU FLTR?                            
         BNE   ALLQSB50            NO, IGNORE OPTION - UNRECOGNIZED             
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0020            NO                                           
         MVC   P+1(14),=C'CLOSED THRU:  '                                       
         MVC   P+20(27),RSTAREC                                                 
         GOTO1 REPORT                                                           
TEST0020 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         BAS   RE,CLOSFILT         YES, FILTER ON CLOSED THRU DATE              
         BNE   EXIT                NO  - SKIP IT                                
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0030            NO                                           
         MVC   P+1(14),=C'ACCEPTED   :  '                                       
         MVC   P+20(27),RSTAREC                                                 
         GOTO1 REPORT                                                           
TEST0030 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         B     ALLQSB60                                                         
*                                                                               
ALLQSB50 EQU   *                                                                
         BAS   RE,STACLOSD         RETRIEVE DATE STATION CLOSED                 
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TESTB015            NO                                           
         MVC   P+1(14),=C'CLOSE DATE :  '                                       
         GOTO1 DATCON,DMCB,(3,CLOSDATE),(8,P+22)                                
         GOTO1 REPORT                                                           
TESTB015 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
ALLQSB60 EQU   *                                                                
         CLI   QOPTION1,C'A'       ACTIVE ONLY                                  
         BE    ST3A                YES                                          
         CLI   QOPTION1,C' '       ACTIVE ONLY (DEFAULT)                        
         BE    ST3A                YES                                          
         CLI   QOPTION1,C'I'       INACTIVE ONLY?                               
         BE    ST3D                YES                                          
         CLI   QOPTION1,C'B'       PRINT BOTH ACTIVE/INACTIVE STATIONS          
         BE    ST3H                                                             
         DC    H'0'                UNRECOGNIZED OPTION                          
ST3A     EQU   *                                                                
         OC    RSTAEND,RSTAEND     ACTIVE: PROCESS                              
         BZ    ST3H                                                             
         B     EXIT                                                             
ST3D     OC    RSTAEND,RSTAEND     INACTIVE ONLY (QOPTION1=I)                   
         BZ    EXIT                                                             
**       CLI   RSTAEND,X'60'       LEFT DURING 96?                              
**       BNE   EXIT                NO  - SKIP IT                                
ST3H     DS    0H                                                               
         CLI   QOPTION2,C'D'       DETAIL REPORT?                               
         BE    DETAIL                                                           
         CLI   QOPTION2,C'A'       DETAIL REPORT W/ ACTIVITY                    
         BE    DETAIL                                                           
         CLI   QOPTION2,C'S'       STATION SEQUENCE ONLY                        
         BE    ST3K                                                             
         CLI   QOPTION2,C'N'       STATION SEQUENCE W/MARKET CODE/NAME          
         BE    ST3J                                                             
         CLI   QOPTION2,C'C'       COMBO STATIONS ONLY                          
         BE    COMBO                                                            
         CLI   QOPTION2,C'E'       PRINT EMAIL ADDRESS REPORT                   
         BE    EMAIL                                                            
         BAS   RE,PUTSORT                                                       
         CLI   QOPTION2,C'M'       MARKET SEQUENCE ONLY                         
         BE    EXIT                DO PRINTING AT REQLAST                       
         CLI   QOPTION2,C'#'       STATION ID NUMBER SEQUENCE ONLY              
         BE    EXIT                DO PRINTING AT REQLAST                       
         B     ST3K                                                             
                                                                                
ST3J     DS    0H                                                               
         MVI   RCSUBPRG,3          USE MARKET CODE/NAME HEADER                  
         CLI   QOPTION3,C'X'       CLOSED OUT STATION LISTING?                  
         BNE   ST3J0020            NO                                           
         MVI   RCSUBPRG,7          YES - SET UP BY 4                            
ST3J0020 EQU   *                                                                
                                                                                
ST3K     SR    R6,R6                                                            
         SR    R7,R7                                                            
         SR    R3,R3                                                            
         LA    R4,RSTAELEM                                                      
ST4      CLI   0(R4),2                                                          
         BNE   *+8                                                              
         AH    R7,=H'1'            ADD UP NUMBER OF COMPETING STATIONS          
         CLI   0(R4),0                                                          
         BE    ST5                                                              
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     ST4                                                              
         SPACE 1                                                                
ST5      LTR   R7,R7               IF NO COMPETING STATIONS                     
         BZ    ST6                                                              
         D     R6,=F'3'            ELSE, PUT 3 COMP STATIONS PER LINE           
         LTR   R6,R6                                                            
         BZ    ST6                                                              
         AH    R7,=H'1'            PLUS A LINE FOR THE REMAINDER                
         SPACE 1                                                                
ST6      CH    R7,=H'2'                                                         
         BNL   *+8                                                              
         LA    R7,2                ALLOW MIN OF 2 PRINT LINES                   
         SPACE 1                                                                
         CLI   FOOTSW,C'Y'                                                      
         BNE   ST6A                                                             
         AH    R7,=H'2'            ALLOW FOR 2 FOOTLINES                        
ST6A     EQU   *                                                                
         SR    R2,R2                                                            
         IC    R2,LINE                                                          
         AR    R2,R7                                                            
         SR    R7,R7                                                            
         IC    R7,MAXLINES                                                      
         CR    R2,R7                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
         GOTO1 REPORT                                                           
PROCESS  EQU   *                                                                
         MVC   P+1(4),RSTAKSTA                                                  
         MVI   P+5,C'-'                                                         
         MVC   P+6(1),RSTAKSTA+4                                                
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    BRNCH                                                            
         MVI   P+7,C'M'                                                         
BRNCH    EQU   *                                                                
         CLI   P+6,C' '                                                         
         BNE   *+10                                                             
         MVC   P+6(2),=C'TV'                                                    
         MVC   P+9(20),RSTAMKT                                                  
         SPACE 1                                                                
         CLI   RCSUBPRG,0                                                       
         BE    PROCESS5                                                         
         CLI   RCSUBPRG,4                                                       
         BE    PROCESS5                                                         
         CLI   RCSUBPRG,3                                                       
         BE    PROCESS5                                                         
         CLI   RCSUBPRG,7                                                       
         BE    PROCESS5                                                         
         MVC   P+22(7),P+1                                                      
         MVC   P+1(20),RSTAMKT                                                  
         MVI   P+21,C' '                                                        
                                                                                
PROCESS5 DS    0H                                                               
         EDIT  RSTACHAN,(4,P+30),ALIGN=LEFT                                     
         CLI   P+6,C'F'                                                         
         BNE   FFIL                                                             
         LA    R1,P+33                                                          
         CLI   0(R1),X'40'                                                      
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVC   1(1,R1),0(R1)                                                    
         MVI   0(R1),C'.'                                                       
         SPACE 1                                                                
FFIL     MVC   P+36(3),RSTAAFFL                                                 
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(8,P+40)                                
         OC    RSTAEND,RSTAEND                                                  
         BZ    GRPSB                                                            
         GOTO1 DATCON,DMCB,(3,RSTAEND),(8,PSECOND+40)                           
         SPACE 1                                                                
GRPSB    EQU   *                                                                
         OC    RSTACLDT,RSTACLDT                                                
         BZ    CLDT                                                             
         MVC   WORK(2),RSTACLDT                                                 
         MVC   WORK+2(1),=X'01'                                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,PSECOND+64)                              
         GOTO1 DATCON,DMCB,(3,CLOSDATE),(8,PTHIRD+40)                           
*                                                                               
         TM    CLOSMECH,X'80'                                                   
         BNO   GRPSB020                                                         
         MVC   PSECOND+72(15),=C'TAPE ACTUALIZER'                               
         B     CLDT                                                             
GRPSB020 EQU   *                                                                
         TM    CLOSMECH,X'20'                                                   
         BNO   GRPSB040                                                         
         MVC   PSECOND+72(10),=C'R16 REPORT'                                    
         B     CLDT                                                             
GRPSB040 EQU   *                                                                
         SPACE 1                                                                
CLDT     EQU   *                                                                
         LA    R5,RSTAGRUP                                                      
         BAS   RE,GRPELEM                                                       
         MVC   P+49(1),RSTAGRUP                                                 
         MVI   P+50,C'-'                                                        
         MVC   P+51(10),4(R2)                                                   
         CLI   RSTAGRUP+1,X'40'                                                 
         BE    RANK                                                             
         MVC   PSECOND+49(1),RSTAGRUP+1                                         
         MVI   PSECOND+50,C'-'                                                  
         MVC   PSECOND+51(10),14(R2)                                            
         SPACE 1                                                                
RANK     MVC   P+63(1),RSTARANK                                                 
         SPACE 1                                                                
         BAS   RE,PRTFONE          PRINT FORMER REP/NEW REP                     
         SPACE 1                                                                
*                                                                               
* PRINT STATION ID NUMBER FROM X'0F' ELEMENT       ABOB ADDED                   
         BAS   RE,PRTSIDN                                                       
*                                                                               
STACON   MVI   P+67,C'Y'                                                        
         TM    RSTASTAT,X'08'                                                   
         BZ    TRAFSYS                                                          
         MVI   P+67,C'N'                                                        
         SPACE 1                                                                
TRAFSYS  MVC   P+70(1),RSTATRAF                                                 
         SPACE 1                                                                
TVB      EQU   *                                                                
         CLI   QOPTION3,C'X'       CLOSED THRU REPORT?                          
         BE    DOPR2               YES - SKIP TVB REGION/OWNER DISPLAY          
*                                     THESE USE SAME PRINT SPACE                
         LA    R3,TVBLST                                                        
TVB2     CLI   0(R3),X'FF'                                                      
         BE    OWNER                                                            
         CLC   RSTATVB,0(R3)                                                    
         BE    TVB4                                                             
         LA    R3,20(R3)                                                        
         B     TVB2                                                             
TVB4     GOTO1 CHOPPER,DMCB,(18,2(R3)),(12,P+72),(C'P',2)                       
         SPACE 1                                                                
OWNER    OC    RSTAOWN,RSTAOWN                                                  
         BZ    DOPR2                                                            
         LA    R3,ROWNNAME                                                      
OWNER4   GOTO1 CHOPPER,DMCB,(20,0(R3)),(13,P+85),(C'P',2)                       
*                                                                               
*              COMPETING STATIONS                                               
* JAN16/01 ABOB CHANGED                                                         
DOPR2    EQU   *                                                                
*                                                                               
*   TEST CBS-A DUMP                                                             
***      CLC   =C'CBS',RSTAKSTA                                                 
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   TEST CBS-A DUMP END                                                         
*                                                                               
         CLI   QOPTION2,C'N'       STATION SEQUENCE W/MARKET CODE/NAME          
         BE    MARKET                                                           
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'02'        GET OTHER STATIONS ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   STA25               IF NOT FOUND PRINT                           
* ELSE                                                                          
         USING RSTAMKEL,R6                                                      
         LA    R2,P+99             POINT TO P LINE                              
STA8     LR    R7,R2               POINT TO SINGLE ENTRY ON P LINE              
*                                                                               
*LOAD 3 TO LIMIT ONLY TO THREE ENTRIES PER PLINE                                
         LA    R5,3                                                             
*                                                                               
STA10    MVC   0(4,R7),RSTAMKST    MOVE STATION CODE ON PLINE                   
         LA    R7,3(R7)            BUMP P LINE 3 CHAR                           
         CLI   0(R7),C' '          CHECK IF STATION CODE IS 3 CHAR              
         BE    *+8                 IF NOT                                       
         LA    R7,1(R7)            BUMP P LINE 1 CHAR                           
         MVI   0(R7),C'-'                                                       
         MVC   1(1,R7),RSTAMKST+4  MOVE BROADCAST BAND                          
         CLI   1(R7),C' '          IF BAND = SPACE                              
         BNE   *+8                 NOT = SPACE                                  
         MVI   1(R7),C'T'          ELSE MOVE T ON P LINE                        
         CLI   7(R6),X'41'                                                      
         BL    *+14                                                             
         MVI   2(R7),C'='                                                       
         MVC   3(3,R7),RSTAMKAF    MOVE AFFILIATE                               
         LA    R7,7(R7)          BUMP TO NEXT ENTRY POSITION ON P LINE          
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   STA25               NOT THERE PRINT                              
         BCT   R5,STA10                                                         
*                                                                               
         LA    R2,132(R2)          BUMP TO NEXT P LINE                          
         B     STA8                                                             
*                                                                               
*PLINES COMPRESSION                                                             
*                                                                               
STA25    CLC   PSECOND,SPACES      CHECK IF SOMETHING ON IT                     
         BNE   STA11               IF YES                                       
         MVC   PSECOND,PTHIRD      ELSE BUMP PLINES UP                          
         MVC   PTHIRD,PFOURTH                                                   
         MVC   PFOURTH,SPACES      CLEAR LAST                                   
*                                                                               
STA11    CLC   PTHIRD,SPACES       CHECK IF SOMETHING ON IT                     
         BNE   STA25PRT            IF YES                                       
         MVC   PTHIRD,PFOURTH      ELSE BUMP PLINE UP                           
         MVC   PFOURTH,SPACES      CLEAR LAST                                   
*                                                                               
STA25PRT GOTO1 REPORT              PRINT ALL PLINES                             
*                                                                               
STA26    EQU   *                                                                
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0080            NO                                           
         MVC   P+1(14),=C'SORTSW=       '                                       
         MVC   P+08(01),SORTSW                                                  
         GOTO1 REPORT                                                           
TEST0080 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         CLI   SORTSW,C'N'                                                      
         BE    EXIT                                                             
         BAS   RE,GETSORT                                                       
         CLI   SORTSW,C'N'                                                      
         BE    EXIT                                                             
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0090            NO                                           
         MVC   P+1(14),=C'SORT RETURNED:'                                       
         GOTO1 REPORT                                                           
TEST0090 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         B     ST3K                                                             
*                                                                               
MARKET   DS    0H                  MARKET CODE/NAME                             
         LA    R6,RSTAREC                                                       
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,X'08'        GET MARKET CODE                              
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
                                                                                
         OC    RSTAMKTC,RSTAMKTC                                                
         BZ    MARKET20                                                         
         MVC   P+99(L'RSTAMKTC),RSTAMKTC                                        
         GOTO1 GETMARK,DMCB,RSTAMKTC                                            
         BNE   MARKET10                                                         
         DROP  R6                                                               
                                                                                
         LA    R6,IOAREA                                                        
         USING RMKTREC,R6                                                       
         MVC   P+104(L'RMKTNAME),RMKTNAME                                       
         DROP  R6                                                               
                                                                                
* RE-ESTABLISH STATION SEQUENCE                                                 
MARKET10 DS    0H                                                               
         XC    KEY(27),KEY                                                      
         MVC   KEY(27),RSTAREC                                                  
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY                           
                                                                                
MARKET20 DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XMOD1 1                                                                
*                                                                               
*                                                                               
*   GET GROUP/SB-GROUP  ELEMENT                                                 
GRPELEM  L     R2,BGLK             BEGIN OF 07 ELEMENTS                         
         SR    R4,R4                                                            
LOOP7    IC    R4,1(R2)                                                         
         CLC   0(2,R5),4(R2)                                                    
         BE    0(RE)                                                            
         CLI   1(R5),X'40'                                                      
         BNE   *+14                                                             
         CLC   0(1,R5),4(R2)                                                    
         BE    0(RE)                                                            
         AR    R2,R4                                                            
         CLI   0(R2),7                                                          
         JNE   *+2                                                              
         B     LOOP7                                                            
*                                                                               
         SPACE 1                                                                
STA40    CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         MVI   SORTSW,C'Y'                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,1                                                       
         CLI   QOPTION3,C'X'       CLOSED OUT STATION LISTING?                  
         BNE   STA40020            NO                                           
         MVI   RCSUBPRG,5          YES - SET UP BY 4                            
STA40020 EQU   *                                                                
         MVI   FOOTSW,C'Y'         PRINTING FOOTLINES                           
         B     STA26                                                            
         EJECT                                                                  
*                                                                               
* PRINT REPORT USING DETAIL OPTION                                              
DETAIL   DS    0H                                                               
         CLI   QOPTION2,C'A'       ACTIVITY REPORT?                             
         BNE   DET00                                                            
         BAS   RE,DATEFILT         YES, FILTER ON DATE FIELD                    
         BNE   DETX                EXIT IF NOT IN DATE RANGE                    
*                                                                               
DET00    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         CLI   QOPTION3,C'X'       CLOSED OUT STATION LISTING?                  
         BNE   DET00020            NO                                           
         MVI   RCSUBPRG,6          YES - SET UP BY 4                            
DET00020 EQU   *                                                                
         GOTO1 REPORT              BLANK LINE                                   
*                                                                               
* PRINT STATION                                                                 
*                                                                               
         CLI   QOPTION2,C'A'       ACTIVITY REPORT?                             
         BNE   DET03                                                            
         MVC   P(13),=C'(NEW) STATION'                                          
         B     DET02                                                            
*                                                                               
DET01    DS    0H                                                               
         MVC   P(13),=C'(OLD) STATION'                                          
DET02    MVC   P+19(4),RSTAKSTA    STATION NAME                                 
         LA    R2,P+23                                                          
         B     DET04                                                            
*                                                                               
DET03    DS    0H                                                               
         MVC   P(07),=C'STATION'                                                
         MVC   P+13(4),RSTAKSTA    STATION NAME                                 
         LA    R2,P+17                                                          
*                                                                               
DET04    DS    0H                                                               
         CLI   0(R2),C' '          CALL LETTERS CAN BE LT 4 CHARS               
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVI   1(R2),C'-'          PRINT BAND                                   
         MVI   2(R2),C'L'          LOW POWER TV                                 
         CLI   RSTAKSTA+4,C'L'     LOW POWER TV                                 
         BE    DET04A                                                           
         MVI   3(R2),C'M'                                                       
         MVC   2(1,R2),RSTAKSTA+4                                               
         CLI   RSTAKSTA+4,C' '                                                  
         BNE   *+10                                                             
         MVC   2(2,R2),=C'TV'                                                   
DET04A   EQU   *                                                                
*                                                                               
         CLI   QOPTION2,C'A'       FOR OPTION2=ACTIVITY                         
         BNE   DET04B              PRINT DATE OF LAST CHANGE                    
         LA    R6,RSTAREC                                                       
         USING RSTACTEL,R6                                                      
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DET04B                                                           
         MVC   10(16,R2),=C'LAST CHANGE DATE'                                   
         GOTO1 DATCON,DMCB,(3,RSTACTCD),(11,27(R2))                             
         MVC   41(8,R2),=C'ADD DATE'                                            
         GOTO1 DATCON,DMCB,(3,RSTACTAD),(11,50(R2))                             
         DROP  R6                                                               
*                                                                               
DET04B   DS    0H                                                               
         GOTO1 REPORT                                                           
DET05    GOTO1 REPORT              BLANK LINE                                   
         MVC   P,SPACES            MAKE SURE P IS SPACES, NOT NULLS             
*                                                                               
* ONLY GET X'08' ONCE...                                                        
         XC    SAVX08,SAVX08                                                    
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         ST    R6,SAVX08                                                        
*                                                                               
* LINE 1 - MARKET, CON TO STA?, RECAP?, CHANNEL                                 
         MVC   P+2(6),=C'MARKET'                                                
         MVC   P+14(L'RSTAMKT),RSTAMKT                                          
*                                                                               
         MVC   P+40(11),=C'CON TO STA?'                                         
         MVC   P+52(3),=C'YES'                                                  
         TM    RSTASTAT,X'08'                                                   
         BZ    *+10                                                             
         MVC   P+52(3),=C'NO '                                                  
*                                                                               
         MVC   P+56(6),=C'RECAP?'                                               
         MVC   P+63(3),=C'YES'                                                  
         TM    RSTASTAT,X'04'                                                   
         BZ    *+10                                                             
         MVC   P+63(3),=C'NO '                                                  
*                                                                               
         MVC   P+67(7),=C'CHANNEL'                                              
         EDIT  RSTACHAN,(4,P+75),ALIGN=LEFT                                     
         CLI   RSTAKSTA+4,C'F'                                                  
         BNE   DET10                                                            
         LA    R2,P+78                                                          
         CLI   0(R2),X'40'                                                      
         BNE   *+6                                                              
         BCTR  R2,0                                                             
         MVC   1(1,R2),0(R2)                                                    
         MVI   0(R2),C'.'                                                       
DET10    GOTO1 REPORT                                                           
*                                                                               
* LINE 2 - GRP/SUBGRP, TRAFFIC FORMAT, DESTINATION FORMAT                       
         MVC   P+2(10),=C'GRP/SUBGRP'                                           
         MVC   P+14(L'RSTAGRUP),RSTAGRUP                                        
*                                                                               
         MVC   P+40(14),=C'TRAFFIC FORMAT'                                      
         MVC   P+55(1),RSTATRAF                                                 
*                                                                               
         MVC   P+58(18),=C'DESTINATION FORMAT'                                  
         L     R6,SAVX08                                                        
         OR    R6,R6               IS THERE AN X'08'?                           
         BZ    *+10                                                             
         USING RSTAXXEL,R6                                                      
         MVC   P+77(2),RSTARWS                                                  
         DROP  R6                                                               
         GOTO1 REPORT                                                           
*                                                                               
* LINE 3 - TVB REGION, JOIN DATE, LEAVE DATE                                    
         MVC   P+2(10),=C'TVB REGION'                                           
         LA    R3,TVBLST                                                        
DET20    CLI   0(R3),X'FF'                                                      
         BE    DET30                                                            
         CLC   RSTATVB,0(R3)                                                    
         BE    *+12                                                             
         LA    R3,20(R3)                                                        
         B     DET20                                                            
         GOTO1 CHOPPER,DMCB,(18,2(R3)),(12,P+14),(C'P',2)                       
*                                                                               
DET30    MVC   P+40(9),=C'JOIN DATE'                                            
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(8,P+51)                                
*                                                                               
         MVC   P+60(10),=C'LEAVE DATE'                                          
         OC    RSTAEND,RSTAEND                                                  
         BZ    DET40                                                            
         GOTO1 DATCON,DMCB,(3,RSTAEND),(8,P+71)                                 
DET40    EQU   *                                                                
         MVC   P+82(10),=C'CLOSE DATE'                                          
         OC    RSTACLDT,RSTACLDT                                                
         BZ    DET45                                                            
         MVC   WORK(2),RSTACLDT                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(6,P+93)                                    
DET45    EQU   *                                                                
         GOTO1 REPORT                                                           
*                                                                               
* LINE 4 - OWNER                                                                
         MVC   P+2(5),=C'OWNER'                                                 
         OC    RSTAOWN,RSTAOWN                                                  
         BZ    *+14                                                             
         BAS   RE,GETOWN                                                        
         MVC   P+14(L'ROWNNAME),ROWNNAME                                        
*                                                                               
         MVC   P+40(10),=C'FORMER REP'                                          
         MVC   P+60(7),=C'NEW REP'                                              
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0C'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+16                NO EXTRA ELEM                                
         USING RSTAFNEL,R6                                                      
         MVC   P+51(L'RSTAFNFO),RSTAFNFO                                        
         MVC   P+71(L'RSTAFNNE),RSTAFNNE                                        
         DROP  R6                                                               
         TM    CLOSMECH,X'80'                                                   
         BNO   DET47                                                            
         MVC   P+82(15),=C'TAPE ACTUALIZER'                                     
         B     DET49                                                            
DET47    EQU   *                                                                
         TM    CLOSMECH,X'20'                                                   
         BNO   DET49                                                            
         MVC   P+82(10),=C'R16 REPORT'                                          
         B     DET49                                                            
DET49    EQU   *                                                                
         GOTO1 REPORT                                                           
*                                                                               
* LINE 5 - RANK, STATUS                                                         
         MVC   P+2(4),=C'RANK'                                                  
         MVC   P+14(L'RSTARANK),RSTARANK                                        
*                                                                               
         MVC   P+40(6),=C'STATUS'                                               
         TM    RSTASTAT,X'F1'                                                   
         BZ    DET50               NO STATUS'S SET                              
         LA    R2,P+51                                                          
         TM    RSTASTAT,X'80'                                                   
         BZ    *+14                                                             
         MVC   0(06,R2),=C'BOP=N,'                                              
         LA    R2,06(R2)                                                        
         TM    RSTASTAT,X'40'                                                   
         BZ    *+14                                                             
         MVC   0(11,R2),=C'CONTRACT=N,'                                         
         LA    R2,11(R2)                                                        
         TM    RSTASTAT,X'20'                                                   
         BZ    *+14                                                             
         MVC   0(11,R2),=C'AVAILS=NEW,'                                         
         LA    R2,11(R2)                                                        
         TM    RSTASTAT,X'10'                                                   
         BZ    *+14                                                             
         MVC   0(11,R2),=C'BUDGET=YES,'                                         
         LA    R2,11(R2)                                                        
         BCTR  R2,0                                                             
         MVI   0(R2),C' '                                                       
DET50    GOTO1 REPORT                                                           
*                                                                               
* LINE 6 - OPTIONS, MKT CODE                                                    
         MVC   P+2(7),=C'OPTIONS'                                               
         MVC   P+14(9),=C'NNNNNNNNN'                                            
         OC    SAVX08,SAVX08       IS THERE AN X'08'?                           
         BZ    *+14                                                             
         L     R6,SAVX08                                                        
         USING RSTAXXEL,R6                                                      
         MVC   P+14(L'RSTAOPTS),RSTAOPTS                                        
         DROP  R6                                                               
*                                                                               
         MVC   P+40(8),=C'MKT CODE'                                             
         OC    SAVX08,SAVX08       IS THERE AN X'08'?                           
         BZ    DET75                                                            
         L     R6,SAVX08                                                        
         USING RSTAXXEL,R6                                                      
         MVC   P+51(L'RSTAMKTC),RSTAMKTC                                        
*                                                                               
         GOTO1 GETMARK,DMCB,RSTAMKTC                                            
         BNE   DET75                                                            
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKTREC,R6                                                       
         MVC   P+56(L'RMKTNAME),RMKTNAME                                        
*                                                                               
DET75    DS    0H                                                               
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
* LINE 7 - SYSTEM ID'S: RECEIVING, SIGN ON                                      
         MVC   P+2(11),=C'SYSTEM IDS:'                                          
         MVC   P+16(08),=C'RECEIVNG'                                            
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         USING RSTAXEL,R6                                                       
         BNE   *+10                NO EXTRA ELEM                                
         MVC   P+28(L'RSTARSO),RSTARSO                                          
         DROP  R6                                                               
*                                                                               
         MVC   P+40(07),=C'SIGN ON'                                             
         LA    R2,P+51                                                          
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         USING RSTASOEL,R6                                                      
         BNE   DET90               NO SIGN ON ID                                
DET80    MVC   0(L'RSTASO,R2),RSTASO                                            
         BAS   RE,NEXTEL                                                        
         BNE   DET90                                                            
         LA    R2,8(R2)                                                         
*                                                                               
         CLI   0(R2),C' '          FIND FIRST BLANK                             
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         B     DET80                                                            
         DROP  R6                                                               
DET90    GOTO1 REPORT                                                           
*                                                                               
* LINE 8 - DESTINATION                                                          
         MVC   P+16(11),=C'DESTINATION'                                         
         L     R6,SAVX08                                                        
         OR    R6,R6               IS THERE AN X'08'?                           
         BZ    *+10                                                             
         USING RSTAXXEL,R6                                                      
         MVC   P+28(L'RSTAORDS),RSTAORDS                                        
         DROP  R6                                                               
         GOTO1 REPORT                                                           
*                                                                               
* LINE 9 - PRIMARY NETWORK AFFL, SECONDARY AFFL, TIME ZONE                      
         MVC   P+2(20),=C'PRIMARY NETWORK AFFL'                                 
         MVC   P+24(L'RSTAAFFL),RSTAAFFL                                        
*                                                                               
         MVC   P+28(14),=C'SECONDARY AFFL'                                      
         MVC   P+47(09),=C'TIME ZONE'                                           
         L     R6,SAVX08                                                        
         OR    R6,R6               IS THERE AN X'08'?                           
         BZ    *+16                                                             
         USING RSTAXXEL,R6                                                      
         MVC   P+43(L'RSTAAFL2),RSTAAFL2                                        
         MVC   P+57(1),RSTATZ                                                   
         DROP  R6                                                               
*                                                                               
*STATION ID NUMBER FROM ELEMENT X'0F'           ABOB ADDED                      
         MVC   P+60(12),=C'STATION ID #'       PRINT FIELD NAME                 
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                       GET STATION ID ELEMENT            
         BNE   DET91                          IF NOT FOUND                      
         USING RSTAINEL,R6                    ELSE                              
         EDIT RSTAINID,(8,P+73),ALIGN=LEFT    PUT STAION ID NUMBER ON P         
*                                                                               
DET91    DS    0H                                                               
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
* LINE 10- CON COMM                                                             
         MVC   P+2(8),=C'CON COMM'                                              
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DET110                                                           
         USING RSTACEL,R6                                                       
         CLI   RSTACTYP,C'M'                                                    
         BE    DET100                                                           
         MVC   P+14(1),RSTACTYP                                                 
         MVI   P+15,C'='                                                        
         EDIT  RSTACNUM,(4,P+16),ALIGN=LEFT                                     
         MVC   P+14(L'RSTARANK),RSTARANK                                        
         B     DET110                                                           
DET100   DS    0H                                                               
         ZIC   R1,RSTACLEN                                                      
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+14(0),RSTACCMT                                                 
         DROP  R6                                                               
*************************                                                       
*                                                                               
         LA    R6,RSTAREC                                                       
         USING RSTAKEY,R6                                                       
         CLC   =C'C=',P+14         DISPLAY STORED FILE COMMENT                  
         BNE   DET110                                                           
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING RCMTD,RE                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,RSTAKREP   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE,P+16       COMMENT CODE                                 
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R6,RE                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   DET110                                                           
*                                                                               
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,IOAREA,DMWORK                    
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         LA    R6,IOAREA                                                        
         USING RCMTELM2,R6         COMMENT TEXT ELEMENT                         
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   DET110                                                           
*                                                                               
DISPFC20 DS    0H                                                               
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    DISPFC30                                                         
         CLI   RCMT2TXT,C' '                                                    
         BNE   DISPFC30                                                         
         BAS   RE,NEXTEL                                                        
         BE    DISPFC20                                                         
         B     DET110                                                           
*                                                                               
DISPFC30 DS    0H                                                               
         CLI   RCMT2LEN,46         COMMT FIELD HAS THIS MUCH ROOM               
         BH    DISPFC40                                                         
         ZIC   R1,RCMT2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DET110                                                           
         MVC   P+28(0),RCMT2TXT                                                 
*                                                                               
DISPFC40 MVC   P+28(46),RCMT2TXT                                                
*                                                                               
DET110   GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
* LINE 11- ELECTRONIC CON?, LUID, INVOICE?, A/R INTERFACE CODE                  
         MVC   P+2(15),=C'ELECTRONIC CON?'                                      
         MVC   P+22(4),=C'LUID'                                                 
         MVC   P+36(08),=C'INVOICE?'                                            
         MVC   P+49(18),=C'A/R INTERFACE CODE'                                  
         L     R6,SAVX08                                                        
         OR    R6,R6               IS THERE AN X'08'?                           
         BZ    DET120                                                           
         USING RSTAXXEL,R6                                                      
*                                                                               
         MVC   P+18(2),=C'NO'      ELECTRONIC CON                               
         TM    RSTAXOPT,X'80'                                                   
         BZ    *+10                                                             
         MVC   P+18(3),=C'YES'                                                  
*                                                                               
         MVC   P+27(L'RSTALUID),RSTALUID    LUID                                
*                                                                               
         MVC   P+45(2),=C'NO'      INVOICE                                      
         TM    RSTAXOPT,X'40'                                                   
         BZ    *+10                                                             
         MVC   P+45(3),=C'YES'                                                  
*                                                                               
         MVC   P+68(L'RSTAOSI),RSTAOSI    A/R INTERFACE CODE                    
DET120   GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
* LINE 12- RADIO COMBOS: COMBO, 1, 2, 3, 4                                      
         MVC   P+2(21),=C'RADIO COMBOS:  COMBO)'                                
         MVC   P+33(2),=C'1)'                                                   
         MVC   P+45(2),=C'2)'                                                   
         MVC   P+57(2),=C'3)'                                                   
         MVC   P+69(2),=C'4)'                                                   
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   DET140                                                           
         USING RSTACSEL,R6                                                      
*                                                                               
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    *+20                                                             
*                                                                               
         MVC   P+24(4),RSTACS                                                   
         MVC   P+28(2),=C'-C'                                                   
         B     DET140                                                           
*                                                                               
         LA    R2,P+36                                                          
DET130   MVC   0(4,R2),RSTACS                                                   
         MVI   4(R2),C'-'                                                       
*                                                                               
* GET LENGTH AND PRINT PREFERRED STATUS FOR NEW COMBINED ELEMENT                
*                                                                               
         MVC   5(1,R2),RSTACS+4    BAND                                         
*                                                                               
         CLI   RSTACSLN,7          NEW ELEMENTS HAVE LENGTH > 7                 
         BNH   DET135                                                           
         MVC   6(1,R2),RSTACS+5    PREFERRED STATION (*), IF SELECTED           
                                                                                
DET135   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DET140                                                           
         LA    R2,12(R2)                                                        
         B     DET130                                                           
*                                                                               
DET140   GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
* LINE 13- FAX NUMBER                                                           
         MVC   P+2(10),=C'FAX Number'                                           
         OC    SAVX08,SAVX08       IS THERE AN X'08'                            
         BZ    DET142                                                           
         L     R6,SAVX08                                                        
         USING RSTAXXEL,R6                                                      
         CLI   RSTAOFAX,0                                                       
         BE    DETINTL                                                          
         MVC   P+14(L'RSTAOFAX),RSTAOFAX                                        
         B     DET142                                                           
DETINTL  EQU   *                                                                
****TEST                                                                        
*        MVC   P+2(8),=C'INFAX=  '                                              
*        MVC   P+10(13),RSTAOFAX                                                
*        GOTO1 REPORT                                                           
****END TEST                                                                    
         UNPK  INTLNUM(16),RSTAOFAX+3(8)                                        
         MVC   P+14(16),INTLNUM+1                                               
         EDIT  (1,RSTAOFAX+1),(2,P+15)                                          
         MVC   P+29(1),SPACES                                                   
         CLC   RSTAOFAX+3(5),=X'0000000000'                                     
         BNE   DET142                                                           
         MVC   P+14(16),SPACES                                                  
DET142   EQU   *                                                                
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
* LINE 14- TWX NUMBER, LAST UPDATE, UNIQUE ID                                   
         MVC   P+2(10),=C'TWX Number'                                           
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   DET14210            NO TWX ELEMENT                               
         USING RSTATWXL,R6                                                      
         MVC   P+14(20),RSTATWX    INSERT TWX NUMBER                            
         DROP  R6                                                               
DET14210 EQU   *                                                                
         MVC   P+34(12),=C'Last Updated'                                        
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'F1'                                                     
         BAS   RE,GETEL                                                         
         BNE   DET14220            NO DATE ELEMENT                              
         USING RSTACTEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,RSTACTCD),(11,P+48)                               
         DROP  R6                                                               
DET14220 EQU   *                                                                
         MVC   P+57(09),=C'Unique ID'                                           
         LA    R6,RSTAREC                                                       
         XC    DUB,DUB                                                          
         MVI   ELCODE,X'2A'                                                     
         BAS   RE,GETEL                                                         
         BNE   DET14230            NO DATE ELEMENT                              
         USING RSTAUIEL,R6                                                      
         MVC   P+68(6),RSTAUIST                                                 
         MVC   DUB(6),RSTAUIST     SET FOR MARKET READ                          
         DROP  R6                                                               
DET14230 EQU   *                                                                
         GOTO1 REPORT                                                           
         OC    DUB,DUB             WAS UNIQUE ID FOUND?                         
         BZ    DET14240            NO                                           
*                                                                               
         LA    R6,RBUYREC                                                       
         USING CT99RECD,R6                                                      
         XC    0(25,R6),0(R6)      BUILD CONTROL FILE KEY                       
         MVI   CT99KTYP,X'99'                                                   
         MVC   CT99KUID,DUB                                                     
         MVC   P+34(08),=C'UID MKT='                                            
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R6),(R6),0                   
         CLI   DMCB+8,0                                                         
         BE    DET14232                                                         
         MVC   P+44(17),=C'NO LONGER ON FILE'                                   
         B     DET14235                                                         
DET14232 EQU   *                                                                
         LA    RF,CT99DATA                                                      
         USING CRCLD,RF                                                         
         MVC   P+44(26),CRCLCTY                                                 
DET14235 EQU   *                                                                
         DROP  R6,RF                                                            
         GOTO1 REPORT                                                           
*                                                                               
DET14240 EQU   *                                                                
*                                                                               
* LINE 15- LIABILITY COMMENT                                                    
         MVC   P+2(9),=C'LIABILITY'                                             
         OC    RSTALIAB,RSTALIAB   ANY LIABILITY ENTERED?                       
         BZ    DET144Z                                                          
         EDIT  (B1,RSTALIAB),(2,P+14),FILL=0                                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTREC,R6                                                       
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,QREP                                                    
         MVC   RCMTKOFF,=X'FFFF'                                                
         MVC   RCMTKCDE,SPACES                                                  
         MVC   RCMTKCDE(4),=C'LIAB'                                             
         LA    R7,RCMTKCDE+4                                                    
         EDIT  (B1,RSTALIAB),(2,0(R7)),FILL=0                                   
         DROP  R6                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   DET110                                                           
*                                                                               
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,IOAREA,DMWORK                    
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         LA    R6,IOAREA                                                        
         USING RCMTELM2,R6         COMMENT TEXT ELEMENT                         
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   DET144Z                                                          
*                                                                               
DET144B  DS    0H                                                               
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    DET144D                                                          
         CLI   RCMT2TXT,C' '                                                    
         BNE   DET144D                                                          
         BAS   RE,NEXTEL                                                        
         BE    DET144B                                                          
         B     DET144Z                                                          
*                                                                               
DET144D  DS    0H                                                               
         CLI   RCMT2LEN,46         COMMT FIELD HAS THIS MUCH ROOM               
         BH    DET144F                                                          
         ZIC   R1,RCMT2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DET144Z                                                          
         MVC   P+28(0),RCMT2TXT                                                 
*                                                                               
DET144F  MVC   P+28(46),RCMT2TXT                                                
*                                                                               
DET144Z  EQU   *                                                                
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
         MVC   P+2(09),=C'COMMENTS:'                                            
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0B'        STATION COMMENT ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    DET146              ONE COMMENT FOUND                            
         GOTO1 REPORT              NO COMMENTS - PRINT TITLE LINE               
         B     DET148                                                           
DET146   EQU   *                                                                
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         LA    RE,3                SET UP FOR LENGTH MOVE                       
         SR    RF,RE                                                            
         EX    RF,DET146A          MOVE BY LENGTH                               
         GOTO1 REPORT              PRINT OUT COMMENT                            
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    DET146              FOUND - GO PRINT IT                          
         B     DET148              NOT FOUND -                                  
*                                                                               
                                                                                
DET146A  MVC   P+13(0),2(R6)                                                    
*                                                                               
DET148   EQU   *                                                                
*                                                                               
*                                                                               
* LINE 16 - TEAM, MKT CODE                                                      
*                                                                               
         LA    R2,P+14                                                          
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   DET149                                                           
         GOTO1 REPORT              BLANK LINE                                   
         MVC   P+2(4),=C'TEAM'     DATA FOUND:  INSERT LINE TITLE               
         USING RSTAOTEL,R6                                                      
DET148A  EQU   *                                                                
         MVC   0(L'RSTAOTOF,R2),RSTAOTOF                                        
         LA    R2,L'RSTAOTOF(R2)                                                
         MVI   0(R2),C'='                                                       
         MVC   1(L'RSTAOTTM,R2),RSTAOTTM                                        
         LA    R2,L'RSTAOTTM+1(R2)                                              
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,NEXTEL                                                        
         BE    DET148A                                                          
         BCTR  R2,0                                                             
         MVI   0(R2),C' '                                                       
         GOTO1 REPORT              OUTPUT THE DATA                              
         DROP  R6                                                               
*                                                                               
* LINE 17-NN - COMPETITIVE DATA                                                 
DET149   EQU   *                                                                
         GOTO1 REPORT              BLANK LINE                                   
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'02'        ELEM FOR OTHER STA'S IN MKT                  
         BAS   RE,GETEL                                                         
         BNE   DET165              NO COMP STA'S                                
         MVC   P(28),=C'COMPETING STATIONS=AFFILIATE'                           
         GOTO1 REPORT                                                           
         LA    R2,P                                                             
         LA    R3,4                4 MARKETS PER LINE                           
         USING RSTAMKEL,R6                                                      
DET150   DS    0H                                                               
         MVC   0(4,R2),RSTAMKST                                                 
         LA    R5,4(R2)                                                         
*                                                                               
         CLI   0(R5),C' '          CALL LETTERS CAN BE LT 4 CHARS               
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         MVI   1(R5),C'-'          PRINT BAND                                   
         MVC   2(1,R5),RSTAMKST+4                                               
         CLI   RSTAMKST+4,C'L'     LOW POWER TV                                 
         BE    DET150A                                                          
         MVI   3(R5),C'M'                                                       
DET150A  EQU   *                                                                
         CLI   RSTAMKST+4,C'L'                                                  
         BE    DET151                                                           
         CLI   RSTAMKST+4,C' '                                                  
         BNE   *+10                                                             
         MVC   2(2,R5),=C'TV'                                                   
         B     DET152                                                           
DET151   EQU   *                                                                
         MVC   2(2,R5),=C'L '                                                   
*                                                                               
DET152   EQU   *                                                                
         CLC   RSTAMKAF,SPACES                                                  
         BZ    *+14                                                             
         MVI   4(R5),C'='                                                       
         MVC   5(L'RSTAMKAF,R5),RSTAMKAF                                        
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DET160                                                           
*                                                                               
         LA    R2,6+11(R2)                                                      
         BCT   R3,DET150                                                        
*                                                                               
         GOTO1 REPORT                                                           
         LA    R2,P                                                             
         LA    R3,4                                                             
         B     DET150                                                           
*                                                                               
DET160   GOTO1 REPORT                                                           
*                                                                               
DET165   EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   DET165X                                                          
*                                                                               
         MVC   P+2(7),=C'EMAIL: '                                               
         USING RSTAEML,R6                                                       
         LA    R4,P+13                                                          
DET165A  EQU   *                                                                
         ZIC   R5,RSTAEMLN                                                      
         SHI   R5,RSTAADD-RSTAEMC                                               
         CHI   R5,1                                                             
         BL    DET165C            SKIP EMPTY ELT FROM PREVIOUS BUG              
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RSTAADD                                                  
DET165B  BAS   RE,NEXTEL                                                        
         BNE   DET165X                                                          
         AR    R4,R5                                                            
         MVI   1(R4),C','                                                       
         AHI   R4,3                                                             
         B     DET165A                                                          
DET165C  SHI   R4,2               GET RID OFF COMMA FOR EMPTY ELT               
         MVI   0(R4),C' '                                                       
         B     DET165B                                                          
DET165X  DS    0H                                                               
         DROP  R6                                                               
         GOTO1 REPORT                                                           
*                                                                               
* RE-READ STA REC TO RESTORE SEQUENCE                                           
DET170   DS    0H                                                               
         CLI   QOPTION2,C'A'       ACTIVITY REPORT?                             
         BNE   DET300                                                           
         CLI   RSTAREC,X'42'                                                    
         BE    DET300                                                           
*                                  GET THE X'42' RECORD AND PRINT IT            
         XC    KEY(27),KEY                                                      
         MVC   KEY(27),RSTAREC                                                  
         MVI   KEY,X'42'                                                        
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY                           
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DET300                                                           
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RSTAREC,DMWORK                
         CLI   DMCB+8,0                                                         
         BE    DET01                                                            
         DC    H'0'                                                             
*                                                                               
DET300   DS    0H                                                               
         XC    KEY(27),KEY                                                      
         MVC   KEY(27),RSTAREC                                                  
         MVI   KEY,X'02'           IF WE'RE COMING BACK FROM ACTIVITY           
         MVC   KEYSAVE(27),KEY     PRINTING, PUT X'02' AS TYPE                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY                           
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RSTAREC,DMWORK                
         CLI   DMCB+8,X'00'                                                     
         BE    DETX                                                             
         DC    H'0'                                                             
*                                                                               
DETX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PRINT FORMER REP/NEW REP                                           
**********************************************************************          
PRTFONE  NTR1                                                                   
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0C'        NEW/FORMER REP ELEMENT?                      
         BAS   RE,GETEL                                                         
         BNE   PFMT0080            NOT FOUND                                    
         USING RSTAFNEL,R6                                                      
         CLC   RSTAFNFO,SPACES                                                  
         BE    *+16                                                             
         MVC   PSECOND+2(11),=C'FORMER REP='                                    
         MVC   PSECOND+13(L'RSTAFNFO),RSTAFNFO                                  
         CLC   RSTAFNNE,SPACES                                                  
         BE    PFMT0080                                                         
         MVC   PSECOND+17(08),=C'NEW REP='                                      
         MVC   PSECOND+25(L'RSTAFNNE),RSTAFNNE                                  
*                                                                               
PFMT0080 EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'2A'        GET UNIQUE ID ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   PFMT0100            NOT FOUND                                    
*                                                                               
         USING RSTAUIEL,R6                                                      
         MVC   PSECOND+30(6),RSTAUIST                                           
         B     PFMT0120                                                         
PFMT0100 EQU   *                                                                
****     MVC   PSECOND+30(06),=C'NO UID'                                        
PFMT0120 EQU   *                                                                
         DROP  R6                                                               
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* PRTSIDN - SUBROUTINE TO FIND AND PRINT STATION ID NUMBER FROM   *             
*           X'0F' ELEMENT.                                        *             
*           THE STATION ID NUMBER IS PRINTED ON PTHIRD.           *             
*           ABOB ADDED                                            *             
*******************************************************************             
PRTSIDN  NTR1                                                                   
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                       GET STATION ID ELEMENT            
         BNE   PSID001                        IF NOT FOUND EXIT                 
         USING RSTAINEL,R6                    ELSE                              
         MVC   PTHIRD+3(13),=C'STATION ID #:'  PRINT FIELD NAME                 
*                                                                               
*PUT STAION ID NUMBER ON PTHIRD                                                 
         EDIT  RSTAINID,(8,PTHIRD+16),ALIGN=LEFT                                
*                                                                               
PSID001  DS    0H                                                               
         B     EXIT                                                             
*PRTSIDN SUBROUTINE END                                                         
***********************************************************************         
         EJECT                                                                  
**********************************************************************          
* FOR OPTION2=ACTIVITY REPORT, PASS RECORD WITHIN DATE RANGE                    
* IF NO START DATE, DEFAULT DATE RANGE TO CURRENT MON-SUN WEEK                  
**********************************************************************          
DATEFILT NTR1                                                                   
         CLC   QSTART,SPACES                                                    
         BNE   DATEF100                                                         
*                                                                               
* NO START DATE, USE CURRENT MON-SUN WEEK                                       
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,QSTART)                                
         GOTO1 GETDAY,DMCB,QSTART,DMCB+8                                        
         CLC   DMCB+4(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                INVALID DATE FORMAT                          
*                                                                               
         CLI   DMCB,MONDAY                                                      
         BE    DATEF010            DATE IS A MONDAY                             
         ZIC   R2,DMCB             BACK UP DATE TO A                            
         BCTR  R2,0                MONDAY                                       
         LNR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,QSTART,QSTART,(R2)                                    
*                                                                               
DATEF010 GOTO1 ADDAY,DMCB,QSTART,QEND,6                                         
*                                  QEND IS NOW SUNDAY                           
DATEF100 DS    0H                                                               
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         LA    R6,RSTAREC                                                       
         USING RSTACTEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   NO                  DID NOT FIND ACTIVITY ELEMENT, OMIT          
         GOTO1 DATCON,DMCB,(0,QSTART),(3,ACTSTRDT)                              
         GOTO1 DATCON,DMCB,(0,QEND),(3,ACTENDDT)                                
         CLC   RSTACTCD,ACTSTRDT                                                
         BL    NO                                                               
         CLC   RSTACTCD,ACTENDDT                                                
         BH    NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* FOR OPTION3='C' FOR CREATE DATE FILTER                                        
* IF NO START DATE, DEFAULT DATE RANGE TO CURRENT MON-SUN WEEK                  
**********************************************************************          
DAT2FILT NTR1                                                                   
         CLC   QSTART,SPACES                                                    
         BNE   DAT2F100                                                         
*                                                                               
* NO START DATE, USE CURRENT MON-SUN WEEK                                       
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,QSTART)                                
         GOTO1 GETDAY,DMCB,QSTART,DMCB+8                                        
         CLC   DMCB+4(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                INVALID DATE FORMAT                          
*                                                                               
         CLI   DMCB,MONDAY                                                      
         BE    DAT2F010            DATE IS A MONDAY                             
         ZIC   R2,DMCB             BACK UP DATE TO A                            
         BCTR  R2,0                MONDAY                                       
         LNR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,QSTART,QSTART,(R2)                                    
*                                                                               
DAT2F010 GOTO1 ADDAY,DMCB,QSTART,QEND,6                                         
*                                  QEND IS NOW SUNDAY                           
DAT2F100 DS    0H                                                               
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         LA    R6,RSTAREC                                                       
         USING RSTACTEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   NO                  DID NOT FIND ACTIVITY ELEMENT, OMIT          
         GOTO1 DATCON,DMCB,(0,QSTART),(3,ACTSTRDT)                              
         GOTO1 DATCON,DMCB,(0,QEND),(3,ACTENDDT)                                
         CLC   RSTACTAD,ACTSTRDT                                                
         BL    NO                                                               
         CLC   RSTACTAD,ACTENDDT                                                
         BH    NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* FOR OPTION3='X' FOR CLOSED THRU DATE FILTER                        *          
*        STATION CLOSED DATE MUST NOT BE EARLIER THAN R2CLOSMN,      *          
*              THE REQUESTED CLOSED THRU DATE                        *          
*        STATION MUST HAVE BEEN CLOSED VIA R16 OR ACTUALIZER         *          
*                                                                    *          
**********************************************************************          
CLOSFILT NTR1                                                                   
*                                                                               
         LA    R6,QCARD2                                                        
         USING QREC2,R6                                                         
         GOTO1 DATCON,DMCB,(0,QR70CLOS),(3,WORK)                                
*                                  CONVERT CLOSE DATE TO YYMM BINARY            
         DROP  R6                                                               
*                                                                               
         LA    R6,RSTAREC                                                       
         USING RSTACTEL,R6                                                      
         CLC   RSTACLDT,WORK       CLOSED DATE VS REQUEST                       
         BL    NO                  CLOSED EARLIER - SKIP IT                     
         MVI   ELCODE,X'23'        GET CLOSEOUT ELEMENT                         
         LA    R6,RSTAREC                                                       
         USING RSTACTEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   NO                  DID NOT FIND CLOSEOUT ELEMENT, OMIT          
         TM    RSTACOFL-RSTACOEL(R6),X'80'                                      
*                                  CLOSED VIA ACTUALIZER?                       
         BO    CLFI0100            YES - ACCEPT IT                              
         TM    RSTACOFL-RSTACOEL(R6),X'20'                                      
*                                  CLOSED VIA R16 REPORT?                       
         BO    CLFI0100            YES - ACCEPT IT                              
         B     NO                                                               
CLFI0100 EQU   *                                                                
         MVC   CLOSMECH,RSTACOFL-RSTACOEL(R6)                                   
*                                  SAVE TYPE OF CLOSE INDICATOR                 
         MVC   CLOSDATE,RSTACODT-RSTACOEL(R6)                                   
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
* FOR OPTION3 NOT= 'X' FOR CLOSED THRU DATE FILTER                   *          
*        RETRIEVE THE DATE STATION CLOSED VALUE FOR DISPLAY          *          
*                                                                    *          
**********************************************************************          
STACLOSD NTR1                                                                   
*                                                                               
*                                                                               
         XC    CLOSDATE,CLOSDATE   CLEAR DATE CLOSED                            
         LA    R6,RSTAREC                                                       
         USING RSTACTEL,R6                                                      
         MVI   ELCODE,X'23'        GET CLOSEOUT ELEMENT                         
         LA    R6,RSTAREC                                                       
         USING RSTACTEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   NO                  DID NOT FIND CLOSEOUT ELEMENT, OMIT          
         MVC   CLOSDATE,RSTACODT-RSTACOEL(R6)                                   
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
SETSORT  NTR1                                                                   
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0001            NO                                           
         MVC   P+1(14),=C'STATION SET  :'                                       
         MVC   P+20(27),RSTAREC                                                 
         GOTO1 REPORT                                                           
TEST0001 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         CLI   QOPTION2,C'#'    IF OPTION2 = SORT ON STATION ID                 
         BE    SRTSID                                                           
*                                                                               
         GOTO1 =V(SORTER),DMCB,SRTFLD,RECTYP,0,RR=RELO                          
         MVI   SORTSW,C'N'                                                      
         MVI   SORTSET,C'Y'                                                     
         B     EXIT                                                             
*                                                                               
SRTSID   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,STFDSID,RCTPSID,0,RR=RELO                        
         MVI   SORTSW,C'N'                                                      
         MVI   SORTSET,C'Y'                                                     
         B     EXIT                                                             
         SPACE 1                                                                
SRTFLD   DC    CL80'SORT FIELDS=(41,20,A,27,5,A),FORMAT=BI,WORK=1'              
RECTYP   DC    CL80'RECORD TYPE=V,LENGTH=1004   '                               
*                                                                               
*SORT CARDS TO SORT IN STATION ID NUMBER                                        
*                                                                               
STFDSID  DC    CL80'SORT FIELDS=(5,4,A),FORMAT=BI,WORK=1'                       
RCTPSID  DC    CL80'RECORD TYPE=V,LENGTH=1008   '                               
         SPACE 1                                                                
         EJECT                                                                  
*--------------------------------------------------------                       
*'PUT' SORT SUBROUTINE                                                          
*---------------------------------------------------------                      
*                                                                               
PUTSORT  NTR1                                                                   
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0060            NO                                           
         MVC   P+1(14),=C'STATION SORT :'                                       
         MVC   P+20(27),RSTAREC                                                 
         GOTO1 REPORT                                                           
TEST0060 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         CLI   QOPTION2,C'#'           IF OPTION2 = SORT IN STATION ID          
         BE    PUTSTSID                                                         
*                                                                               
         MVC   HALF,RSTALEN                                                     
         XC    SRTREC(4),SRTREC                                                 
         LH    R1,HALF                                                          
         LA    RF,SRTREC+4                                                      
         LA    RE,RSTAREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         LH    R1,HALF                                                          
         AH    R1,=H'4'                                                         
         STH   R1,SRTREC                                                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC,RR=RELO                           
         B     TEST0101                                                         
*                                                                               
PUTSTSID DS    0H                                                               
*                                                                               
         MVC   HALF,RSTALEN         GET RECORD LENGTH                           
         XC    SRTREC(8),SRTREC     CLEAR FIRST 8 BYTES                         
         LH    R1,HALF                                                          
         LA    RF,SRTREC+8                                                      
         LA    RE,RSTAREC                                                       
         MOVE  ((RF),(R1)),(RE)     MOVE RECORD TO SORT BUFFER                  
         LH    R1,HALF              RELOAD LENGTH                               
         AH    R1,=H'8'                                                         
         STH   R1,SRTREC                                                        
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL               GET STATION ID ELEMENT                    
         BNE   PSSID100               IF NOT FOUND                              
         USING RSTAINEL,R6            ELSE                                      
         MVC   SRTREC+4(4),RSTAINID   PUT STATION ID NUMBER TO SORT             
         DROP  R6                                                               
         B     PSSID101                                                         
*                                                                               
PSSID100 DS    0H                                                               
         MVC   SRTREC+4(4),=4X'FF'    SET ID TO HIGHEST NUMBER                  
PSSID101 DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC,RR=RELO                           
*   TEST                                                                        
TEST0101 DS    0H                                                               
         CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0100            NO                                           
         LH    RF,SRTREC           GET LENGTH OF ENTRY                          
         LA    R4,SRTREC                                                        
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TEST0100 EQU   *                                                                
*   TEST END                                                                    
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
*--------------------------------------------------------                       
*'GET' SORT SUBROUTINE                                                          
*--------------------------------------------------------                       
GETSORT  NTR1                                                                   
         MVI   SORTSW,C'N'                                                      
         GOTO1 =V(SORTER),DMCB,=C'GET',RR=RELO                                  
         L     RE,DMCB+4                                                        
         LTR   RE,RE                                                            
         BZ    EXIT                                                             
         MVI   SORTSW,C'Y'                                                      
         LH    R1,0(RE)                                                         
*                                                                               
         CLI   QOPTION2,C'#'           IF OPTION2 = SORT IN STATION ID          
         BE    GETSTSID                                                         
*                                                                               
         SH    R1,=H'4'           GET LENGTH OF RECORD                          
         LA    RF,RSTAREC                                                       
         LA    RE,4(RE)           POINT TO BEGINNING OF RECORD                  
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   HALF,RSTALEN                                                     
         LH    RE,HALF                                                          
         LA    RE,RSTAREC(RE)                                                   
         MVI   0(RE),0                                                          
         BAS   RE,GETOWN                                                        
         B     TEST0071                                                         
*                                                                               
GETSTSID DS    0H                                                               
         SH    R1,=H'8'                                                         
         LA    RF,RSTAREC                                                       
         LA    RE,8(RE)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   HALF,RSTALEN                                                     
         LH    RE,HALF                                                          
         LA    RE,RSTAREC(RE)                                                   
         MVI   0(RE),0                                                          
         BAS   RE,GETOWN                                                        
*                                                                               
*   TEST                                                                        
TEST0071 CLC   =C'PRINTIT',QUESTOR DISPLAY INPUT?                               
         BNE   TEST0070            NO                                           
         MVC   P+1(14),=C'STATION BACK :'                                       
         MVC   P+20(27),RSTAREC                                                 
         GOTO1 REPORT                                                           
TEST0070 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* READ OWNERSHIP RECORDS FOR MARKET SEQUENCE *                                  
GETOWN   NTR1                                                                   
         OC    RSTAOWN,RSTAOWN    IS THERE A CODE                               
         BZ    EXIT               NO CODE - NO OWNERSHIP RECORD                 
         XC    KEY(27),KEY                                                      
         MVI   KEY,X'2A'          OWNERSHIP RECORD ID                           
         MVC   KEY+22(2),RSTAKREP   REP CODE                                    
         MVC   KEY+24(3),RSTAOWN                                                
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY                           
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETO0060                                                         
         MVC   ROWNNAME,=C'OWNER NOT ON FILE   '                                
         B     EXIT                                                             
GETO0060 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,OWNREC,DMWORK                 
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ROWNNAME,OWNREC+36                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET MARKET RECORD                                                             
* P1=MARKET CODE                                                                
* MARKET RECORD WILL BE READ INTO IOAREA                                        
**********************************************************************          
GETMARK  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RMKTKEY,R6                                                       
         MVI   RMKTKTYP,X'2B'      RECORD TYPE                                  
         MVC   RMKTKREP,QREP       REP CODE                                     
                                                                                
         L     RF,0(R1)                                                         
         MVC   RMKTKMKT,0(RF)      MARKET CODE                                  
         DROP  R6                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(L'RMKTKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,IOAREA,DMWORK                    
         TM    DMCB+8,X'FD'                                                     
         BZ    YES                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         EJECT                                                                  
**********************************************************************          
* COMBO OPTION TO PRINT COMBO LISTING                                           
**********************************************************************          
COMBO    DS    0H                                                               
         MVI   RCSUBPRG,3                                                       
         CLI   QOPTION3,C'X'       CLOSED OUT STATION LISTING?                  
         BNE   COMBO020            NO                                           
         MVI   RCSUBPRG,7          YES - SET UP BY 4                            
COMBO020 EQU   *                                                                
*                                                                               
         CLI   RSTAKSTA+4,C'C'     PRINT ONLY COMBO STATIONS                    
         BNE   COMBOX                                                           
*                                                                               
         LA    R2,P                                                             
         USING PCOMBO,R2                                                        
*                                  MARKET                                       
         MVC   PCOMSTA(4),RSTAKSTA                                              
         MVC   PCOMSTA+4(3),=C'-CM'                                             
         MVC   PCOMMKT,RSTAMKT                                                  
*                                                                               
         LA    R6,RSTAREC          COMPONENT STATIONS                           
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BE    COMBO05                                                          
         MVC   PCOMSTA1(35),=C'*** NO COMPONENT STATIONS FOUND ***'             
         B     COMBO30                                                          
*                                                                               
COMBO05  DS    0H                                                               
         LA    R3,PCOMSTA1                                                      
*                                                                               
COMBO10  DS    0H                                                               
         MVC   0(4,R3),RSTACS                                                   
         MVI   4(R3),C'-'                                                       
*                                                                               
* GET LENGTH AND PRINT PREFERRED STATUS FOR NEW COMBINED ELEMENT                
*                                                                               
         MVC   5(1,R3),RSTACS+4    BAND                                         
*                                                                               
         CLI   RSTACSLN,7          NEW ELEMENTS HAVE LENGTH > 7                 
         BNH   COMBO20                                                          
         MVC   6(1,R3),RSTACS+5    PREFERRED STATION (*), IF SELECTED           
                                                                                
COMBO20  DS    0H                                                               
         BAS   RE,NEXTEL           GET NEXT COMPONENT STATION                   
         BNE   COMBO30                                                          
         LA    R3,9(R3)                                                         
         B     COMBO10                                                          
*                                                                               
COMBO30  DS    0H                                                               
         BAS   RE,PRTFONE          PRINT FORMER REP/NEW REP                     
         GOTO1 REPORT              DOUBLE SPACE                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
COMBOX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
**********************************************************************          
* EMAIL OPTION TO PRINT EMAIL LISTING                                           
**********************************************************************          
EMAIL    DS    0H                                                               
         MVI   RCSUBPRG,3                                                       
         CLI   QOPTION3,C'X'       CLOSED OUT STATION LISTING?                  
         BNE   EMAIL020            NO                                           
         MVI   RCSUBPRG,7          YES - SET UP BY 4                            
EMAIL020 EQU   *                                                                
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   EMAILX                                                           
*                                                                               
         USING RSTAEML,R6                                                       
         LA    R4,P+30                                                          
EMAIL050 EQU   *                                                                
         ZIC   R5,RSTAEMLN                                                      
         SHI   R5,RSTAADD-RSTAEMC                                               
         CHI   R5,1                                                             
         BL    C                  SKIP EMPTY ELT FROM PREVIOUS BUG              
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RSTAADD                                                  
B        BAS   RE,NEXTEL                                                        
         BNE   EMAIL051                                                         
         AR    R4,R5                                                            
         MVI   1(R4),C','                                                       
         AHI   R4,3                                                             
         B     EMAIL050                                                         
C        SHI   R4,2               GET RID OFF COMMA FOR EMPTY ELT               
         MVI   0(R4),C' '                                                       
         B     B                                                                
EMAIL051 DS    0H                                                               
         DROP  R6                                                               
*                                  MARKET                                       
         MVC   P+1(4),RSTAKSTA                                                  
         MVI   P+5,C'-'                                                         
         MVC   P+6(1),RSTAKSTA+4                                                
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    EMAIL060                                                         
         MVI   P+7,C'M'                                                         
EMAIL060 EQU   *                                                                
         CLI   P+6,C' '                                                         
         BNE   *+10                                                             
         MVC   P+6(2),=C'TV'                                                    
         MVC   P+9(20),RSTAMKT                                                  
*                                                                               
EMAIL070 DS    0H                                                               
         BAS   RE,PRTFONE          PRINT FORMER REP/NEW REP                     
         GOTO1 REPORT              DOUBLE SPACE                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
EMAILX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**************************************************************                  
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*  INCLUDE RETVBTAB                                                             
         SPACE 2                                                                
       ++INCLUDE RETVBTAB                                                       
         SPACE 2                                                                
MONDAY   EQU   1                                                                
BGLK     DC    F'0'                                                             
RELO     DS    A                                                                
SORTSW   DS    CL1                                                              
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
SORTSET  DS    CL1                                                              
ELCODE   DS    X                                                                
SAVX08   DS    F                   SAVE ADDR OF X'08' ELEM                      
INTLNUM  DS    CL16                AREA TO UNPACK INTERNATIONAL FAX #           
PATRN    DS    CL2                 PATTERN FOR EDITITNG                         
MASK     DC    X'2020'             MASK FOR EDITING                             
MYFLAG   DS    XL1                                                              
FIRSTQ   EQU   X'80'                                                            
                                                                                
ACTSTRDT DS    CL6                 ACTIVITY START DATE                          
ACTENDDT DS    CL6                 ACTIVITY END DATE                            
QCARD2   DS    CL80                                                             
CLOSMECH DS    CL1                                                              
CLOSDATE DS    XL3                 CLOSEOUT DATE (BINARY)                       
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
OWNREC   DS    CL100              AREA TO READ OWNERSHIP RECORD INTO            
IOAREA   DS    CL1000                                                           
SRTREC   DS    0F                                                               
         DS    CL1004                                                           
         DS    F                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*********PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
RCMTD    DSECT                                                                  
       ++INCLUDE REGENCMT                                                       
       ++INCLUDE REGENREQ2                                                      
       ++INCLUDE CTGENRAD                                                       
         PRINT ON                                                               
*                                                                               
* DSECT FOR OPTION COMBO PRINT LINE                                             
*                                                                               
PCOMBO   DSECT                                                                  
         DS    C                                                                
PCOMSTA  DS    CL7                                                              
         DS    CL4                                                              
PCOMMKT  DS    CL20                                                             
         DS    CL4                                                              
PCOMSTA1 DS    CL7                                                              
         DS    CL4                                                              
PCOMSTA2 DS    CL7                                                              
         DS    CL4                                                              
PCOMSTA3 DS    CL7                                                              
         DS    CL4                                                              
PCOMSTA4 DS    CL7                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'175REREP7002 02/11/15'                                      
         END                                                                    
