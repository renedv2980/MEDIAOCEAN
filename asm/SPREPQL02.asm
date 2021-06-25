*          DATA SET SPREPQL02  AT LEVEL 120 AS OF 05/01/02                      
*PHASE SPQL02A                                                                  
*INCLUDE SXBUYC                                                                 
*INCLUDE SXBUYX                                                                 
*INCLUDE SXDTLC                                                                 
*INCLUDE SXDTLX                                                                 
*INCLUDE SXCNVX                                                                 
*                                                                               
         TITLE 'SPQL02 - SPOT XTRACT'                                           
**********************************************************                      
*                                                        *                      
* SPOT SQL SUB SYSTEM EXTRACT CONTROL MODULE             *                      
*                                                        *                      
* QOPT1 - Y - TRACE                                      *                      
* QOPT2 - Y - 4 CLIENTS/4 PRDS/**ESTS/100 BUYS           *                      
**********************************************************                      
SPQL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPQL02                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING SPQL02+4096,R9                                                   
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         EJECT                                                                  
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,PRDFRST                                                     
         BE    PRDF                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,REQLAST                                                     
         BE    XLAST                                                            
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         XC    TOTCLTS,TOTCLTS                                                  
         XC    TOTPRDS,TOTPRDS                                                  
         XC    TOTESTS,TOTESTS                                                  
         XC    TOTBUYS,TOTBUYS                                                  
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
*                                                                               
GENINIT  NTR1                                                                   
         XC    CNTRECS,CNTRECS                                                  
         L     RF,=A(SQLBUFF)                                                   
         ST    RF,ASQLBUFF                                                      
*                                                                               
         OPEN  (EXFILE1,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (EXFILE2,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLIENT FIRST PROCESSING                                             *         
***********************************************************************         
*                                                                               
CLTF     DS    0H                                                               
         CLI   QOPT2,C'Y'          LIMIT NUMBER OF CLIENTS                      
         BNE   CLTF20                                                           
         L     R1,TOTCLTS                                                       
         CH    R1,=H'8'                                                         
         BNH   CLTF10                                                           
         MVI   MODE,CLTLAST                                                     
         B     CLTFX                                                            
*                                                                               
CLTF10   LA    R1,1(R1)                                                         
         ST    R1,TOTCLTS                                                       
         XC    TOTPRDS,TOTPRDS                                                  
         XC    TOTESTS,TOTESTS                                                  
         XC    TOTBUYS,TOTBUYS                                                  
*                                                                               
CLTF20   BAS   RE,BLDCML           BLD TABLE OF COMMERCIALS                     
*                                                                               
CLTFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRODUCT FIRST PROCESSING                                            *         
***********************************************************************         
*                                                                               
PRDF     DS    0H                                                               
         CLI   QOPT2,C'Y'          LIMIT NUMBER OF PRODUCTS                     
         BNE   PRDFX                                                            
         L     R1,TOTPRDS                                                       
         CH    R1,=H'6'                                                         
         BH    PRDF10                                                           
         LA    R1,1(R1)                                                         
         ST    R1,TOTPRDS                                                       
         XC    TOTESTS,TOTESTS                                                  
         XC    TOTBUYS,TOTBUYS                                                  
         B     PRDFX                                                            
*                                                                               
PRDF10   MVI   MODE,PRDLAST                                                     
*                                                                               
PRDFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ESTIMATE FIRST PROCESSING                                           *         
***********************************************************************         
*                                                                               
ESTF     DS    0H                                                               
         CLI   QOPT2,C'Y'          LIMIT NUMBER OF ESTIMATES                    
         BNE   ESTFX                                                            
         B     ESTFX                                                            
         L     R1,TOTESTS                                                       
         CH    R1,=H'4'                                                         
         BH    ESTF10                                                           
         LA    R1,1(R1)                                                         
         ST    R1,TOTESTS                                                       
         XC    TOTBUYS,TOTBUYS                                                  
         B     ESTFX                                                            
*                                                                               
ESTF10   MVI   MODE,ESTLAST                                                     
*                                                                               
ESTFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS BUYS                                                                  
***********************************************************************         
*                                                                               
PROCESS  DS    0H                                                               
         CLI   QOPT2,C'Y'          LIMIT NUMBER OF BUYS                         
         BNE   PROC07                                                           
         L     R1,TOTBUYS                                                       
         CH    R1,=H'100'                                                       
         BH    PROCX                                                            
         LA    R1,1(R1)                                                         
         ST    R1,TOTBUYS                                                       
*                                                                               
PROC07   MVI   SPTLSW,C'D'                                                      
         XC    CURDATE,CURDATE                                                  
         XC    TOTGRS,TOTGRS       CLEAR TOTAL FOR EACH NEW BUY                 
         XC    TOTNET,TOTNET                                                    
         L     R2,ADBUY                                                         
         SR    R4,R4                                                            
         LA    R1,EXFILE2                                                       
         ST    R1,AEXFILE                                                       
         LA    R4,24(R2)                                                        
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
PROC10   BAS   RE,NXTEL                                                         
         BNE   PROC20                                                           
*                                                                               
         GOTO1 SPTLOAD,DMCB,VSXDTLC,VSXDTLX,INITDTL,0,                          
         B     PROC10                                                           
*                                                                               
PROC20   MVI   SPTLSW,C'B'                                                      
         L     R2,ADBUY                                                         
         LA    R1,EXFILE1                                                       
         ST    R1,AEXFILE                                                       
         SR    R4,R4                                                            
         GOTO1 SPTLOAD,DMCB,VSXBUYC,VSXBUYX,INITBUY,0                           
*                                                                               
         ICM   RF,15,CNTRECS                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,15,CNTRECS                                                    
*                                                                               
         BAS   RE,PRTBUY                                                        
*                                                                               
PROCX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS - R1=RECORD LENGTH                   *         
***********************************************************************         
*                                                                               
INITALL  NTR1                                                                   
         LA    R3,MYAREA           R3=A(EXTRACT RECORD AREA)                    
         USING SXBUYD,R3                                                        
         LR    R0,R3                                                            
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
         LA    RF,SXBUYHL                                                       
         SLL   RF,16                                                            
         ST    RF,SXBUYLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   SXBUYSTA-1,SXTRTQ                                                
         MVI   SXBUYSTA,C'A'       STATUS                                       
         MVI   SXBUYACT-1,SXTRTQ                                                
         MVI   SXBUYACT,C'L'       SET ACTION AS LOAD FOR NOW                   
*&&DO                                                                           
         CLI   DXMODE,DXLOADQ      TEST IF LOAD MODE                            
         BE    IALL100                                                          
*                                  HERE IF UPDATE MODE                          
*                                  FORMAT DATE AND TIME FROM RCVHDR             
         GOTO1 VDATCON,DMCB,(3,RDATE),(0,SXBUYCDT+2)                            
         MVC   SXBUYDAT(2),=C'19'                                               
         CLI   TODAY,C'8'          IF THE YEAR IS LESS THAN 8X                  
         BNL   *+10                                                             
         MVC   SXBUYDAT(2),=C'20'  SET CENTURY TO 20                            
*******  MVI   SXBUYTIM-1,SXTRTQ                                                
         ICM   RF,15,RTIME                                                      
         TM    RTIME,X'80'                                                      
         BZ    *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  SXBUYTIM(6),DUB+4(4)                                             
         OI    SXBUYTIM+5,X'F0'                                                 
         B     YES                                                              
*&&                                                                             
*                                  HERE IF LOAD MODE                            
IALL100  MVI   SXBUYDAT-1,SXTRTQ                                                
         MVC   SXBUYDAT+2,TODAY    SET TODAYS DATE                              
         MVC   SXBUYDAT(2),=C'19'                                               
         CLI   TODAY,C'9'          IF THE YEAR IS NOT 9X                        
         BE    *+10                                                             
         MVC   SXBUYDAT(2),=C'20'  SET CENTURY TO 20                            
         TIME                                                                   
         LTR   R0,R0                                                            
         BZ    IALL110                                                          
         ST    R0,MVSTIME                                                       
         L     R1,MVSTIME                                                       
*                                  CONVERT TIME NOW TO EBCDIC                   
         SRL   R1,28                                                            
         STC   R1,TIMEN                                                         
         OI    TIMEN,X'F0'                                                      
         L     R1,MVSTIME                                                       
         SRL   R1,24                                                            
         STC   R1,TIMEN+1                                                       
         OI    TIMEN+1,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,20                                                            
         STC   R1,TIMEN+2                                                       
         OI    TIMEN+2,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,16                                                            
         STC   R1,TIMEN+3                                                       
         OI    TIMEN+3,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,12                                                            
         STC   R1,TIMEN+4                                                       
         OI    TIMEN+4,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,8                                                             
         STC   R1,TIMEN+5                                                       
         OI    TIMEN+5,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,4                                                             
         STC   R1,TIMEN+6                                                       
         OI    TIMEN+6,X'F0'                                                    
         L     R1,MVSTIME                                                       
         STC   R1,TIMEN+7                                                       
         OI    TIMEN+7,X'F0'                                                    
****     MVI   SXBUYTIM-1,SXTRTQ                                                
         MVC   SXBUYTIM,TIMEN      SET TIME                                     
*                                                                               
IALL110  B     YES                                                              
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* INITIALISE BUY EXTRACT RECORD                                       *         
***********************************************************************         
*                                                                               
INITBUY  NTR1                                                                   
         LA    R1,SXBUYDL          R1=L'RECORD                                  
         BAS   RE,INITALL                                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALISE BUY DETAIL EXTRACT RECORD                                *         
***********************************************************************         
*                                                                               
INITDTL  NTR1                                                                   
         LA    R1,SXBUYDL          R1=L'RECORD                                  
         BAS   RE,INITALL                                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT SPOT RECORDS IN LOAD MODE                     *         
* AEXFIEL - A(FILE TO PUT TO)                                         *         
* R2 = A(SPOT DIRECTORY RECORD BUFFER)                                *         
* R4 = A(CURRENT ELEMENT) OR 0                                                  
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(FORMAT CONVERT ROUTINE)                                      *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
*                                                                               
SPTLOAD  NTR1                                                                   
         BAS   RE,CLRAREA          CLEAR AREAS USED                             
         ST    R4,AELEM            A(CURRENT ELEMENT)/0                         
*                                                                               
         LM    R3,R6,0(R1)                                                      
         LTR   R6,R6               TEST IF FILTER ROUTINE PASSED                
         BZ    SLOA010                                                          
         GOTO1 (R6)                FILTER RECORD, GET NEXT IF NOT VALID         
         BNE   SLOAX                                                            
*                                                                               
SLOA010  GOTO1 (R5)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,(RC),MYAREA,AELEM                                      
         CLI   SPTLSW,C'D'         IF COMING FROM DETAIL                        
         BNE   SLOA015                                                          
         BAS   RE,ACCAMTS          GO ACCUMUALTE SPOTS/GRS/NET                  
         BAS   RE,SETELNO          SET SPOT NUMBER                              
*                                                                               
SLOA015  CLI   SPTLSW,C'B'         IF COMING FROM BUY                           
         BNE   *+8                                                              
         BAS   RE,SETAMTS          SET ACCUMULATED AMOUNTS IN MYAREA            
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   SLOA020                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(11),=C'FROM SX---C'                                         
         MVC   WORK+7(1),SPTLSW                                                 
         GOTO1 MYTRACE,DMCB,MYAREA,L'MYAREA,WORK,11                             
*                                  CONVERT RECORD TO SQL BUFFER                 
SLOA020  GOTO1 (R4),DMCB,MYAREA,ASQLBUFF,0                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   SLOA030                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(11),=C'FROM SX---X'                                         
         MVC   WORK+7(1),SPTLSW                                                 
         GOTO1 MYTRACE,DMCB,ASQLBUFF,L'MYAREA,WORK,11                           
*                                                                               
SLOA030  L     R1,AEXFILE                                                       
         L     RF,ASQLBUFF                                                      
         PUT   (R1),(RF)                                                        
*                                                                               
SLOAX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        ACCUMULATE AMOUNTS FROM DTL'S CALL TO GETRATE                          
*                                                                               
ACCAMTS  NTR1                                                                   
         LA    R4,MYAREA                                                        
         USING SXDTLD,R4                                                        
         L     R1,TOTGRS                                                        
         PACK  DUB,SXDTLGRS(12)                                                 
         CVB   R5,DUB                                                           
         AR    R1,R5                                                            
         ST    R1,TOTGRS                                                        
*                                                                               
         L     R1,TOTNET                                                        
         PACK  DUB,SXDTLNET(12)                                                 
         CVB   R5,DUB                                                           
         AR    R1,R5                                                            
         ST    R1,TOTNET                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
*        SET ACCUMULATED AMOUNTS INTO BUYD                                      
*                                                                               
SETAMTS  NTR1                                                                   
         LA    R4,MYAREA                                                        
         USING SXBUYD,R4                                                        
         MVI   SXBUYGRS-1,SXTRTQ                                                
         EDIT  TOTGRS,(12,SXBUYGRS),FILL=0                                      
         MVI   SXBUYNET-1,SXTRTQ                                                
         EDIT  TOTNET,(12,SXBUYNET),FILL=0                                      
         MVI   SXBUYTAX-1,SXTRTQ                                                
         EDIT  TOTTAX,(12,SXBUYTAX),FILL=0                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        SET SPOT NUMBER                                                        
*                                                                               
SETELNO  NTR1                                                                   
         LA    R4,MYAREA                                                        
         USING SXDTLD,R4                                                        
         L     R6,AELEM            ADDRESS OF ELEMENT                           
         CLC   CURDATE,2(R6)       IS THIS THE CURRENT DATE                     
         BE    SEL10                                                            
         MVC   CURDATE,2(R6)       SET THE CURRENT DATE                         
         XC    ELEMNO,ELEMNO       CLEAR ELEMENT NUMBER                         
*                                                                               
SEL10    ZIC   R1,ELEMNO                                                        
         LA    R1,1(R1)                                                         
         STC   R1,ELEMNO                                                        
         MVI   SXDTLSNO-1,SXTRTQ                                                
         EDIT  ELEMNO,(3,SXDTLSNO),FILL=0                                       
*                                                                               
SELX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
CLRAREA  NTR1                                                                   
         LA    R0,MYAREA           R3=A(EXTRACT RECORD AREA)                    
         LA    R1,L'MYAREA                                                      
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         LA    R0,SQLBUFF          R3=A(SQLBUFF)                                
         LA    R1,L'SQLBUFF                                                     
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
         B     EXIT                                                             
         EJECT                                                                  
XLAST    DS    0H                                                               
         EDIT  CNTRECS,(10,P)                                                   
         MVC   P+12(17),=C'RECORDS PROCESSED'                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PRINT BUY RECORD INFO                                                         
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         USING BUYRECD,R6                                                       
         L     R6,ADBUY                                                         
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(2),=C'TV'                                                 
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,BDMASPRD,PPROD,1,=C'TOG'                             
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* READ AND SAVE ALL COMMERCIALS AND SEQ NUMBERS FOR CLIENT IN 'KEY' *           
*                                                                               
         SPACE 1                                                                
BLDCML   NTR1                                                                   
         MVC   MYKEY,KEY           SAVE KEY                                     
         L     R1,=A(CMLTAB)                                                    
         ST    R1,ADCOMREC         FOR NOW SO DTL WILL HAVE ACCESS              
         L     R0,=A(CMLTABX)                                                   
         BAS   RE,CLEAR                                                         
         L     R4,=A(CMLTAB)                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         GOTO1 HIGH                                                             
         B     BLDCML30                                                         
*                                                                               
BLDCML20 GOTO1 SEQ                                                              
*                                                                               
BLDCML30 CLC   KEY(5),KEYSAVE      0A21/A-M/CLT                                 
         BNE   BLDCML40                                                         
*                                                                               
         L     R6,=A(MYIO)                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING CMLRECD,R6                                                       
*                                                                               
         MVC   0(8,R4),KEY+5       SAVE COMMERCIAL CODE                         
         MVC   8(2,R4),CMLSEQ+1    AND 2 BYTES OF CMML SEQ                      
         LA    R4,10(R4)           NEXT ENTRY                                   
         L     R0,=A(CMLTABX)                                                   
         CR    R4,R0                                                            
         BL    BLDCML20                                                         
         DC    H'0'                                                             
*                                                                               
BLDCML40 MVC   0(10,R4),=X'FFFFFFFFFFFFFFFFFFFF'                                
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO CLEAR FROM R1 TO R0. RF IS DESTROYED.               *           
*                                                                               
CLEAR    LA    RF,256                                                           
*                                                                               
CLEAR2   SR    R0,R1               GIVES LENGTH TO CLEAR                        
         CR    R0,RF                                                            
         BNH   CLEAR4                                                           
         XC    0(256,R1),0(R1)                                                  
         AR    R1,RF                                                            
         SR    R0,RF                                                            
         BZR   RE                                                               
         B     CLEAR2                                                           
CLEAR4   LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,CLEARXC                                                       
         BR    RE                                                               
CLEARXC  XC    0(0,R1),0(R1)  ** EXECUTED **                                    
         EJECT                                                                  
*                                                                               
*        FIND ELEMENT WITHIN ELCDL0 - ELCDHI                                    
*                                                                               
NXTEL    CLI   0(R4),0                                                          
         BE    NXTELX                                                           
         ZIC   R0,1(R4)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
NXTEL2   CLI   0(R4),0                                                          
         BE    NXTELX                                                           
         CLC   ELCDLO,0(R4)                                                     
         BH    NXTEL                                                            
         CLC   ELCDHI,0(R4)                                                     
         BL    NXTEL                                                            
         CR    RB,RB                                                            
         B     *+6                                                              
NXTELX   LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA) OR ZERO FOR RECORD                               
*        PARAMETER 3 - A(LABEL) OR ZERO FOR NO LABEL                            
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=================================================================*             
MYTRACE  NTR1                                                                   
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
EXFILE1  DCB   DDNAME=EXFILE1,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=600,  X        
               BLKSIZE=604                                                      
*                                                                               
EXFILE2  DCB   DDNAME=EXFILE2,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=600,  X        
               BLKSIZE=604                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
*                                                                               
VSXBUYC  DC    V(SXBUYC)           A(RTN CONVERT BUY - SXBUYD FORMAT)           
VSXBUYX  DC    V(SXBUYX)           A(RTN SXBUYD FORMAT - SQL FORMAT)            
VSXDTLC  DC    V(SXDTLC)           A(RTN CNVERT BUY DETAILS SXDTLD FMT)         
VSXDTLX  DC    V(SXDTLX)           A(RTN SXDTLD FORMAT - SQL FORMAT)            
*                                                                               
ASQLBUFF DS    A                   SQL BUFFER                                   
AEXFILE  DS    A                   A(FILE TO PUT TO)                            
AELEM    DS    A                   A(CURRENT ELEMENT)                           
*                                                                               
TOTCLTS  DS    F                   TOTAL CLIENTS                                
TOTPRDS  DS    F                   TOTAL PRODUCTS                               
TOTESTS  DS    F                   TOTAL ESTIMATES                              
TOTBUYS  DS    F                   TOTAL BUYS                                   
*                                                                               
TOTGRS   DS    F                   TOTAL GROSS                                  
TOTNET   DS    F                   TOTAL NET                                    
TOTTAX   DS    F                   TOTAL TAX                                    
*                                                                               
CNTRECS  DS    F                   RECORD COUNTER                               
MVSTIME  DS    F                   MVS TIME                                     
TIMEN    DS    CL8                 MVS TIME - EBCDIC                            
*                                                                               
MYKEY    DS    CL(L'KEY)                                                        
*                                                                               
ELCODE   DS    XL1                 ELCODE                                       
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
SPTLSW   DS    CL1                                                              
ELEMNO   DS    XL1                                                              
CURDATE  DS    CL2                                                              
         DS    CL3                                                              
*                                                                               
         DS    F                                                                
MYAREA   DS    CL1000                                                           
SQLBUFF  DS    CL1000                                                           
*                                                                               
MYIO     DS    CL2000                                                           
CMLTAB   DS    CL30000                                                          
CMLTABX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
       ++INCLUDE SXBUYD                                                         
         EJECT                                                                  
       ++INCLUDE SXDTLD                                                         
         EJECT                                                                  
*                                                                               
* DSECT FOR PRINT LINE                                                          
*                                                                               
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPROD    DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         SPACE 2                                                                
*                                                                               
* SPREPWORKD                                                                    
* SPREPMODES                                                                    
* FAFACTS                                                                       
* DDPERVALD                                                                     
* DMDTFIS                                                                       
* DMGREQUS                                                                      
* SPGENBUY                                                                      
* SPTRCMML                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMGREQUS                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
*                                                                               
       ++INCLUDE SPTRCMML                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120SPREPQL02 05/01/02'                                      
         END                                                                    
