*          DATA SET CTLFM1D    AT LEVEL 020 AS OF 05/01/02                      
*PHASE TA021DA                                                                  
         TITLE 'CTLFM1D - CONTROL MAINT - NTWK UNIVERSE REC'                    
CTLFM1D  CSECT                                                                  
         NMOD1 0,**LFM1D**                                                      
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL TEMP W/S)                        
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CT2REC,R4                                                        
*                                                                               
         EJECT                                                                  
*              VALIDATE AND BUILD A KEY                                         
KEYVAL   XC    CT2KEY,CT2KEY                                                    
         MVI   CT2KEY,C'2'                                                      
         LA    R1,UNICODEH                                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         CLI   FLDH+5,4                                                         
         BH    KEYV3                                                            
         TM    FLDH+4,X'08'        TEST NUMERIC                                 
         BNO   KEYV3               NO MUST BE DATE                              
         MVI   CT2KUTYP,X'01'      FOR CODES                                    
         ZIC   R5,FLDH+5                                                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  TEMP(3),FLD(0)      PWOS                                         
         MVC   CT2KCODE(2),TEMP                                                 
         OC    CT2KCODE,CT2KCODE                                                
         BZ    EIIF                                                             
         B     KEYV7                                                            
*                                                                               
KEYV3    DS    0H                  END DATE EDIT                                
         ZIC   R5,FLDH+5                                                        
         GOTO1 VDATVAL,DMCB,(0,FLD),TEMP,(R5)                                   
         OC    DMCB(4),DMCB                                                     
         BZ    EIIF                                                             
         GOTO1 VDATCON,DMCB,(0,TEMP),(2,CT2KEND)                                
KEYV7    MVC   KEY,CT2KEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,UNICODEH                                                      
         ST    R1,FADR                                                          
         CLI   ACTN,2                                                           
         BNE   KEYV7A                                                           
         CLC   KEY,LKEY                                                         
         BE    KEYV7A                                                           
         MVI   ACTN,3              SET TO DISPLAY                               
*                                                                               
KEYV7A   DS    0H                                                               
         CLI   ACTN,3                                                           
         BE    *+8                                                              
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST FOR NOT FOUND                           
         BZ    KEYV8                                                            
         CLI   ACTN,1              ONLY OK FOR ADDS                             
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV8    CLI   ACTN,1              CAN'T EXIST FOR ADD                          
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,3              A DELETED REC CAN ONLY BE DISP               
         BNE   ERNF                                                             
         CLI   ACTN,2                                                           
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
FMT      DS    0H                                                               
DISPREC  DS    0H                                                               
FMTCOM   DS    0H                                                               
         XC    UNIDESC,UNIDESC                                                  
         LA    R7,CT2DATA                                                       
         MVI   DUB1,X'66'                                                       
         CLI   0(R7),X'66'                                                      
         BE    FMTC5                                                            
         BAS   RE,NEXTEL                                                        
         BNE   FMTCX                                                            
*                                                                               
FMTC5    ZIC   R5,1(R7)                                                         
         SH    R5,=H'3'                                                         
         BM    FMTCX                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   UNIDESC(0),2(R7)                                                 
*                                                                               
FMTCX    FOUT  UNIDESCH                                                         
         LA    R7,CT2DATA                                                       
         CLI   0(R7),X'02'                                                      
         BE    FMT2                                                             
         MVI   DUB1,X'02'                                                       
         BAS   RE,NEXTEL                                                        
         BE    FMT2                                                             
         DC    H'0'                MUST FIND 02 ELEM                            
FMT2     DS    0H                                                               
         USING NUNEL02,R7                                                       
         LA    R8,UNIW1H                                                        
         LA    R5,MAXUNIS                                                       
         LA    RA,DISPTAB                                                       
*                                                                               
FMT4     MVC   8(6,R8),=CL6'0'                                                  
         LA    R6,NUNIVES                                                       
         ZIC   R0,0(RA)                                                         
         AR    R6,R0               ADD DISPLACEMENT                             
         OC    0(4,R6),0(R6)                                                    
         BZ    FMT4B                                                            
         L     R0,0(R6)                                                         
         CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         EDIT  (P6,DUB),(6,8(R8)),0,ALIGN=LEFT                                  
FMT4B    FOUT  (R8)                                                             
         BAS   RE,NEXTUN                                                        
         LA    RA,1(RA)            NEXT DISPLACEMENT                            
         BCT   R5,FMT4                                                          
*                                                                               
FMT5     DS    0H                  DISPLAY ADULTS                               
         LA    R8,UNIA1H                                                        
         LA    RA,WOMEN            IN DISPTAB                                   
         LA    R5,MEN              IN DISPTAB                                   
         LA    R6,6                FOR BCT                                      
FMT5B    LA    RC,NUNIVES                                                       
         ZIC   R0,0(RA)                                                         
         AR    RC,R0                                                            
         LA    R1,NUNIVES                                                       
         IC    R0,0(R5)                                                         
         AR    R1,R0                                                            
         L     RF,0(RC)                                                         
         L     RE,0(R1)                                                         
         AR    RF,RE               ADD WOMEN AND MEN                            
         CVD   RF,DUB                                                           
         DP    DUB,=P'10'                                                       
         EDIT  (P6,DUB),(6,8(R8)),0,ALIGN=LEFT                                  
         FOUT  (R8)                                                             
         ZIC   R0,0(R8)                                                         
         AR    R8,R0               NEXT FIELD                                   
         LA    RA,1(RA)            NEXT WOMEN DISPLACEMENT                      
         LA    R5,1(R5)            NEXT MEN                                     
         BCT   R6,FMT5B                                                         
         XC    UNISPEC,UNISPEC                                                  
         FOUT  UNISPECH                                                         
*                                                                               
FMT6X    DS    0H                                                               
         TM    CT2STAT,X'80'                                                    
         BO    DISPX1                                                           
         MVI   NACTN,X'03'         SET OK TO CHA/DEL                            
         LA    R1,UNIW1H                                                        
         ST    R1,FADR                                                          
         B     DISPXX                                                           
DISPX1   MVI   NACTN,X'04'         SET OK TO RESTORE                            
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
DISPXX   B     EXIT                                                             
         EJECT                                                                  
EDT      DS    0H                                                               
DATAVAL  DS    0H                  ADD/CHANGE RECORD                            
         MVC   TEMP,0                                                           
         GOTO1 ABLDREC             REREAD REC                                   
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
*                                                                               
EDT1E    LA    R1,UNIDESCH                                                      
         CLI   5(R1),0                                                          
         BE    EDT2                                                             
         XC    TEMP(100),TEMP                                                   
         MVI   TEMP,X'66'                                                       
         ZIC   R5,5(R1)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),8(R1)                                                  
         AH    R5,=H'3'                                                         
         STC   R5,TEMP+1           SET ELEM LENGHT                              
         GOTO1 APUTEL                                                           
*                                                                               
*                                                                               
EDT2     LA    R1,UNIW1H           CURSOR TO FIRST FIELD                        
*                                                                               
EDT6     XC    TEMP(150),TEMP                                                   
         MVC   TEMP(2),=X'0264'       SET CODE AND LENGHT                       
EDT6A    LA    R7,TEMP                                                          
         USING NUNELEM,R7                                                       
         LA    R8,UNIW1H                                                        
         LA    R5,MAXUNIS                                                       
         LA    RA,DISPTAB                                                       
*                                                                               
EDT6B    CLI   5(R8),0                                                          
         BNE   EDT6C                                                            
         B     EMIF                                                             
*                                                                               
EDT6C    ZIC   RC,5(R8)                                                         
         GOTO1 VCASHVAL,DMCB,8(R8),(RC)                                         
         CLI   DMCB,0                                                           
         BNE   EIIF                                                             
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BNH   EIIF                CAN'T BE NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'1000'                                                     
         CP    DUB+5(3),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   EIIF                NO DECIMAL OR ONES                           
         CVD   R0,DUB              I.E. NEAREST 10,000                          
         DP    DUB,=P'10'                                                       
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         LA    R6,NUNIVES                                                       
         ZIC   R1,0(RA)                                                         
         AR    R6,R1               ADD DISPLACEMENT                             
         ST    R0,0(R6)                                                         
*                                                                               
EDT6E    LA    RA,1(RA)            NEXT UNIV                                    
         BAS   RE,NEXTUN           SET R8 TO NEXT UNPROTECTED FLD               
         BCT   R5,EDT6B                                                         
*                                                                               
EDT7     DS    0H                                                               
         EJECT                                                                  
EDT8     LA    R8,UNISPECH         CHECK FOR PERCENT ADJ                        
         CLI   5(R8),0             NO INPUT                                     
         BE    EDT8X                                                            
*                                  ALLOW VPH CHANGE AND PCT ADJ                 
*                                                                               
EDT8C    DS    0H                                                               
         LA    R7,TEMP             RESET R7 TO ELEM                             
         LA    R6,NUNIVES                                                       
         LA    R5,24               CAN'T USE MAXUNIS                            
*                                  SINCE ONLY 20 DISPLAY                        
*                                  BUILD AND EXPRESSION FOR CASHVAL             
*                                                                               
*                                  TO EDIT                                      
         XC    WORK(20),WORK                                                    
         ZIC   RA,5(R8)                                                         
         BCTR  RA,0                                                             
         EX    RA,*+8                                                           
         B     *+10                                                             
         MVC   WORK+7(0),8(R8)        MOVE ADJUSTMENT                           
         LA    RC,WORK+7                                                        
         AR    RC,RA               POINT TO LAST CHAR                           
         CLI   0(RC),C'%'                                                       
         BE    EDT8C2                                                           
         MVI   1(RC),C'%'          % NOT INPUT - ADD IT                         
         LA    RA,1(RA)                                                         
*                                                                               
EDT8C2   DS    0H                                                               
         AH    RA,=H'8'            SET TOTAL LENGHT OF EXPRESSION               
EDT8D    L     R0,0(R6)                                                         
         LTR   R0,R0                                                            
         BZ    EDT8K               MIGHT BE ZERO                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(6),DUB                                                    
         MVC   WORK(4),WORK+1                                                   
         MVI   WORK+4,C'.'         ALTER TO N.NN FOR CASH VAL                   
         GOTO1 VCASHVAL,DMCB,WORK,(RA)                                          
         CLI   DMCB,0                                                           
         BE    EDT8E                                                            
EDT8ERR  DS    0H                                                               
         B     EIIF                                                             
*                                                                               
EDT8E    L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BNH   EDT8ERR             CAN'T GO NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'50'                                                  
         BL    EDT8F                                                            
         AP    DUB(6),=P'1'        ROUND TO NEAREST 10,000                      
EDT8F    ZAP   DUB,DUB(6)                                                       
         MP    DUB,=P'100'                                                      
         CVB   R0,DUB                                                           
         ST    R0,0(R6)                                                         
EDT8K    LA    R6,4(R6)                                                         
         BCT   R5,EDT8D                                                         
*                                                                               
EDT8X    DS    0H                                                               
         GOTO1 ABLDREC                                                          
         EJECT                                                                  
EDT20    DS    0H                                                               
         LA    R1,UNIW1H                                                        
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         CLI   ACTN,1              ADD                                          
         BNE   UPD1                                                             
         GOTO1 AADD                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     UPDX                                                             
*                                                                               
UPD1     GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
UPDX     MVI   NACTN,X'03'         SET OK TO CHG/DEL                            
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    NEXTEL2                                                          
         CLC   DUB1(1),0(R7)                                                    
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  LTR   R7,R7                                                            
         BR    RE                                                               
         SPACE 2                                                                
NEXTUN   DS    0H                                                               
         ZIC   R0,0(R8)                                                         
         AR    R8,R0                                                            
         CLI   0(R8),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R8),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
EDTERR   ST    R8,FADR                                                          
         B     EMIF                                                             
*                                                                               
*                                                                               
MAXUNIS  EQU   20                                                               
*                                                                               
       ++INCLUDE DDFLDIND                                                       
*CTLFMERRS                                                                      
       ++INCLUDE CTLFMERRS                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              DISPLACEMENT OF UNIV IN 02 ELEM                                  
DISPTAB  DS    0C                                                               
WOMEN    DC    AL1(24)             WOMEN   07 X 4 - 4                           
         DC    AL1(04)             WN18-34 02 X 4 - 4                           
         DC    AL1(08)             WN18-49 03 X 4 - 4                           
         DC    AL1(12)             WN25-54 02 X 4 - 4                           
         DC    AL1(88)             WN35-64 23 X 4 - 4                           
         DC    AL1(20)             WN55+   06 X 4 - 4                           
MEN      DC    AL1(48)             MEN     13 X 4 - 4                           
         DC    AL1(28)             MN18-34 08 X 4 - 4                           
         DC    AL1(32)             MN18-49 09 X 4 - 4                           
         DC    AL1(36)             MN25-54 10 X 4 - 4                           
         DC    AL1(92)             MN35-64 24 X 4 - 4                           
         DC    AL1(44)             MN55+   12 X 4 - 4                           
         DC    AL1(84)             HOMES   22 X 4 - 4                           
         DC    AL1(00)             ALL     01 X 4 - 4                           
         DC    AL1(52)             LOH     14 X 4 - 4                           
         DC    AL1(56)             WWORK   15 X 4 - 4                           
         DC    AL1(68)             TEENS   18 X 4 - 4                           
         DC    AL1(64)             FM12-17 17 X 4 - 4                           
         DC    AL1(80)             CHILD   21 X 4 - 4                           
         DC    AL1(76)             CH6-11  20 X 4 - 4                           
         DC    X'0000'             END OF TABLE                                 
*SPGENUNIV HAS ELEM DSECT I NEED                                                
       ++INCLUDE SPGENUNIV                                                      
*CTLFMDSECT                                                                     
       ++INCLUDE CTLFMDSECT                                                     
         EJECT                                                                  
*CTLFMTWA                                                                       
       ++INCLUDE CTLFMTWA                                                       
*CTLFME2D                                                                       
       ++INCLUDE CTLFME2D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTLFM1D   05/01/02'                                      
         END                                                                    
