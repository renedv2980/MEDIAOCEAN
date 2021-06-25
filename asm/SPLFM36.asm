*          DATA SET SPLFM36    AT LEVEL 035 AS OF 06/19/06                      
*PHASE T21936A                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'SPLFM36 - TALENT FACTOR MAINTENANCE'                            
T21936   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21936                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC     RC - GENOLD                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA    RA - TT219FFD                                      
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING TALREC,R8     R8 - TALREC                                        
         CLI   SVFMTSW,0     TEST IF FORMAT OR EDIT                             
         BE    FMT00                                                            
         CLI   TALDATH+5,0                                                      
         BNE   EDIT00                                                           
         CLI   TALFCTH+5,0                                                      
         BNE   EDIT00                                                           
*                                                *                              
*THIS SECTION DISPLAYS THE RECORD ON THE SCREEN  *                              
*                                                *                              
FMT00    XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY                                                     
         CLI   SVEBCMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   KEY(9),SVKEY        MOVE KEY THROUGH NETWORK                     
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOFNDERR                                                   
FMT02    LHI   RE,5                SET KEY COMPARE LEN                          
         CLI   SVEBCMED,C'N'                                                    
         BNE   *+8                                                              
         LHI   RE,9                                                             
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   ERRORTN                                                          
*                                                                               
         CLC   SVKEY+11(1),KEY+11  TALENT FACTOR GROUP CODE                     
         BE    FMT03                                                            
         GOTO1 SEQ                                                              
         B     FMT02                                                            
*                                                                               
FMT03    GOTO1 GETREC                                                           
*                                  *                                            
*   THIS SECTION CLEARS THE SCREEN *                                            
*                                  *                                            
FMT05    XC    TALDAT,TALDAT                                                    
         FOUT  TALDATH                                                          
         XC    TALFCT,TALFCT                                                    
         FOUT  TALFCTH                                                          
         LA    R2,TALPER1H         R2 POINTS TO PERIOD HEADER                   
FMT10    OC    8(36,R2),8(R2)      - ERASE LOOP -                               
         BZ    FMT15                                                            
         CLC   8(36,R2),SPACES                                                  
         BE    FMT15                                                            
         XC    8(36,R2),8(R2)                                                   
         FOUT  (R2)                                                             
FMT15    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             IS SCREEN AT END?                            
         BH    FMT10               NO.                                          
*                                                                               
* THIS SECTION SETS UP PERIOD FIELD OF FIRST RECORD *                           
*                                                                               
         LA    R2,TALPER1H         R2 POINTS TO PERIOD HEADER                   
         CLI   SVEBCMED,C'N'                                                    
         BE    FMT17                                                            
         MVC   WORK(6),TALKDATE    SAVE END DATE IN WORK                        
         GOTO1 VDATCON,DMCB,(0,TALKDATE),(5,WORK+6)                             
         B     FMT18                                                            
*                                                                               
FMT17    GOTO1 VDATCON,DMCB,(2,NTALKDT),(5,WORK+6)                              
         GOTO1 (RF),(R1),,(0,WORK) SAVE END DATE IN WORK                        
*                                                                               
FMT18    MVC   17(8,R2),WORK+6     ENDDATE(MMMDD/YY) TO SCREEN                  
         MVI   16(R2),C'-'         '-' BETWEEN DATES                            
         MVC   11(3,R2),=C'...'   '...' TO BEGIN DATE SCREEN AREA               
         B     FMT30                                                            
*                                                                               
* THIS SECTION SETS UP PERIOD FIELD FOR ALL RECORDS AFTER THE FIRST *           
*                                                                               
FMT20    GOTO1 SEQ                                                              
         CLC   KEY(5),KEYSAVE      IS THERE ANOTHER RECORD ?                    
         BNE   EXITPRC             NO.                                          
         CLI   SVEBCMED,C'N'                                                    
         BNE   FMT21                                                            
         CLC   KEY(9),KEYSAVE      SAME THROUGH NETWORK                         
         BNE   EXITPRC                                                          
*                                                                               
FMT21    CLC   SVKEY+11(1),KEY+11  TALENT FACTOR GROUP CODE                     
         BNE   FMT20                                                            
*                                                                               
         ZIC   R0,0(R2)            YES.SO ADVANCE THE                           
         AR    R2,R0               PERIOD HEADER.                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+6,1    PREV END+1=NEW BEGIN                
         GOTO1 VDATCON,DMCB,(0,WORK+6),(5,WORK+12)  YYMMDD TO MMMDD/YY          
         MVC   8(8,R2),WORK+12     BEGIN DATE TO SCREEN                         
*                                                                               
         CLI   SVEBCMED,C'N'                                                    
         BE    FMT22                                                            
         MVC   WORK(6),TALKDATE    SAVE END DATE IN WORK                        
         GOTO1 VDATCON,DMCB,(0,TALKDATE),(5,WORK+6)                             
         B     FMT24                                                            
*                                                                               
FMT22    GOTO1 VDATCON,DMCB,(2,NTALKDT),(5,WORK+6)                              
         GOTO1 (RF),(R1),,WORK     SAVE END DATE IN WORK                        
*                                                                               
FMT24    MVC   17(8,R2),WORK+6     END DATE TO SCREEN                           
         MVI   16(R2),C'-'                                                      
*                                                                               
* THIS IS A COMMON ROUTINE FOR TALENT FACTOR AND LAST ACTIVITY DATE *           
*                                                                               
FMT30    ICM   R5,15,TAL05MUL      GET TALENT FACTOR                            
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  27(5,R2),DUB                                                     
         MVC   WORK2(4),28(R2)                                                  
         MVC   29(4,R2),WORK2      SHIFT 4 DIGITS BACK 1                        
         MVI   28(R2),C'.'         INSERT DECIMAL                               
         GOTO1 VDATCON,DMCB,(3,TALACDAT),(5,WORK+6)   DATE TO MMMDD/YY          
         MVC   35(8,R2),WORK+6     ACTIVITY DATE TO SCREEN                      
         FOUT  (R2)                                                             
         B     FMT20                                                            
*                                                                               
* THIS SECTION CHECKS FOR AND VALIDATES INPUT FIELDS *                          
* INPUT DATE IS STORED IN SVKEY+5 IN FORM YYMMDD    *                           
*                                                                               
EDIT00   LA    R2,TALDATH          DATE TEST                                    
         GOTO1 ANY                                                              
         LA    R3,TALDAT                                                        
         GOTO1 VDATVAL,DMCB,(0,(R3)),WORK   INPUT DATE TO WORK                  
         MVI   ERRCD,DATERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRORTN                                                          
         CLI   SVEBCMED,C'N'                                                    
         BE    EDIT02                                                           
         MVC   SVKEY+5(6),WORK     INPUT DATE TO SVKEY+5                        
         B     EDIT04                                                           
*                                                                               
EDIT02   LA    R0,NTALKDT-TALKEY+SVKEY                                          
         GOTO1 VDATCON,DMCB,WORK,(2,(R0))  NETWORK USES 2-BYTE DATES            
*                                                                               
EDIT04   LA    R2,TALFCTH          TALENT-FACTOR TEST                           
         GOTO1 ANY                                                              
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),6             INPUT LENGTH =6?                             
         BNE   ERRORTN                                                          
         TM    8(R2),X'F0'         TALENT FACTOR = N.NNNN ?                     
         BNO   ERRORTN               "      "        "                          
         CLI   9(R2),C'.'            "      "        "                          
         BNE   ERRORTN               "      "        "                          
         MVC   DUB(4),=C'0000'       "      "        "                          
         MVZ   DUB(4),10(R2)         "      "        "                          
         CLC   DUB(4),=C'0000'       "      "        "                          
         BNE   ERRORTN               "      "        "                          
*                                                                               
* THIS SECTION CHANGES A RECORD *                                               
*                                                                               
EDIT10   CLI   SVACT,C'A'          IS IT AN ADD?                                
         BE    ADD00               YES.                                         
         MVC   KEY(11),SVKEY       NO.                                          
         GOTO1 HIGH                                                             
*                                                                               
EDIT12   LA    R2,TALDATH                                                       
         MVI   ERRCD,NOFNDERR                                                   
         CLC   KEY(11),KEYSAVE     DOES INPUT KEY = RECORD KEY ?                
         BNE   ERRORTN             NO.                                          
         CLC   SVKEY+11(1),KEY+11  RIGHT GROUP CODE                             
         BE    EDIT14                                                           
         GOTO1 SEQ                                                              
         B     EDIT12                                                           
*                                                                               
EDIT14   DS    0H                                                               
         GOTO1 GETREC              YES.                                         
*                                                                               
* SET LAST ACTIVITY DATE - DELETE 05 ELEMENT *                                  
*                                                                               
EDIT20   GOTO1 VDATCON,DMCB,(5,0),(3,TALACDAT)   CHANGE ACTIVITY DATE           
         LA    R6,24(R8)           R6 POINTS TO 01 ELEMENT                      
         MVI   ELCODE,5                                                         
         BAS   RE,NEXTEL           FIND NEXT 05 ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,(0,REC),0(R6),0   DELETE OLD 05 ELEMENT              
         B     ADD40                                                            
*                                                                               
* THIS SECTION ADDS A NEW RECORD *                                              
* PREVIOUS ENDDATE IS SAVED IN WORK2 / SVKEY+5 HAS INPUT ENDDATE *              
*                                                                               
ADD00    XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY                                                     
         CLI   SVEBCMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   KEY(9),SVKEY                                                     
         GOTO1 HIGH                                                             
*                                                                               
ADD05    LHI   RE,5                SET KEY COMPARE LEN                          
         CLI   SVEBCMED,C'N'                                                    
         BNE   *+8                                                              
         LHI   RE,9                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      TEST NEW CLIENT(/NETWORK)                    
         BNE   ADD30               YES.                                         
*                                                                               
         CLC   KEY+11(1),SVKEY+11  TALENT FACTOR GROUP CODE                     
         BE    ADD10                                                            
         GOTO1 SEQ                                                              
         B     ADD05                                                            
*                                                                               
ADD10    MVC   WORK2(6),KEY+5      NO. PREV ENDDATE TO WORK2                    
         CLI   SVEBCMED,C'N'                                                    
         BNE   ADD12                                                            
         LA    R0,NTALKDT-TALKEY+KEY                                            
         GOTO1 VDATCON,DMCB,(2,(R0)),WORK2                                      
*                                                                               
ADD12    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
ADD15    LHI   RE,5                SET KEY COMPARE LEN                          
         CLI   SVEBCMED,C'N'                                                    
         BNE   *+8                                                              
         LHI   RE,9                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
*                                                                               
         CLC   KEY(0),KEYSAVE                                                   
         BNE   ADD20                                                            
*                                                                               
         CLC   KEY+11(1),SVKEY+11  TALENT FACTOR GROUP CODE                     
         BE    ADD10                                                            
         B     ADD12                                                            
*                                                                               
* TEST INPUT ENDDATE - PREV ENDDATE SPREAD *                                    
*                                                                               
ADD20    GOTO1 VADDAY,DMCB,WORK2,WORK2+6,1    ADD 1 TO PREV ENDDATE             
         GOTO1 =V(PERVERT),DMCB,WORK2+6,WORK,RR=YES                             
         MVI   ERRCD,SPDERR        "INVALID DATE SPREAD"                        
         LA    R2,TALDATH                                                       
         CLC   DMCB+12(2),=H'52'                                                
         BL    ERRORTN                                                          
         CLC   DMCB+12(2),=H'53'                                                
         BH    ERRORTN                                                          
*                                                                               
* BUILDS NEW RECORD *                                                           
*                                                                               
ADD30    XC    REC(256),REC                                                     
         MVC   REC(13),SVKEY       SET NEW REC KEY                              
         LA    R0,32                                                            
         STCM  R0,3,REC+13        SET RECORD LENGTH                             
         MVC   TALAGYA,AGYALPHA    SET TALAGYA                                  
* BUILD 01 ELEMENT *                                                            
         MVC   TALEL01(2),=X'0108'                                              
         GOTO1 VDATCON,DMCB,(5,0),(3,TALCRDAT)   SET CREATION DATE              
         MVC   TALACDAT,TALCRDAT   SET LAST ACTIVITY DATE                       
* BUILD 05 ELEMENT *                                                            
ADD40    LA    R4,ELEM                                                          
         USING TALEL05,R4                                                       
         XC    ELEM,ELEM                                                        
         MVC   TALEL05(2),=X'050A'                                              
         MVC   WORK(6),TALFCT      ELIMINATE DECIMAL                            
         MVC   WORK+1(4),WORK+2                                                 
         PACK  DUB,WORK(5)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,TAL05MUL      SET TALENT FACTOR                            
         MVC   TAL05DIV,=F'10000'  SET DIVISION FACTOR                          
         LA    R6,24(8)            R6 POINTS TO 01 ELEMENT                      
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,NEXTEL           GET END OF REC ADDRESS                       
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R6)                                   
*                                                                               
* WRITE THE RECORD *                                                            
*                                                                               
WRITE00  CLI   SVACT,C'A'          IS THIS AN ADD?                              
         BNE   WRITE10             NO.                                          
         GOTO1 ADDREC              YES.                                         
         B     FMT00                                                            
WRITE10  GOTO1 PUTREC                                                           
         B     FMT00                                                            
*                                                                               
* GET NEXT ELEMENT ROUTINE *                                                    
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BNE   NEXTEL                                                           
         BR    RE                  EXIT WITH COND CODE EQUAL                    
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH COND CODE NOT EQUAL                
*                                                                               
* ERROR ROUTINE *                                                               
ERRORTN  GOTO1 ERROR                                                            
*                                                                               
*EXIT PROC *                                                                    
EXITPRC  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMD6 - TALENT FACTOR MAINTENANCE SCREEN -                                   
       ++INCLUDE SPLFMD6D                                                       
         EJECT                                                                  
TALRECD  DSECT                                                                  
       ++INCLUDE SPGENTAL                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035SPLFM36   06/19/06'                                      
         END                                                                    
