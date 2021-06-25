*          DATA SET CTLFM12    AT LEVEL 010 AS OF 05/01/02                      
*PHASE TA0212A                                                                  
*INCLUDE DAYVAL                                                                 
*INCLUDE TIMVAL                                                                 
         TITLE 'CTLFM12 - CONTROL FILE MAINT - HUT RECORDS'                     
CTLFM12  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM12**                                                      
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTHREC,R4                                                        
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   XC    CTHKEY,CTHKEY                                                    
         MVI   CTHTYP,C'H'                                                      
         LA    R1,HUTDAYH                                                       
         GOTO1 AFVAL                                                            
         BZ    EXIT                REQUIRED                                     
         CLC   FLD(4),=C'M-S '                                                  
         BNE   KEYV1               ACCEPT M-S FOR MON-SUN                       
         MVI   CTHDAY,X'7F'                                                     
         B     KEYV5                                                            
*                                                                               
KEYV1    DS    0H                                                               
         ZIC   R5,FLDH+5                                                        
         GOTO1 =V(DAYVAL),DMCB,((R5),FLD),CTHDAY,WORK,RR=RB                     
         CLI   CTHDAY,0                                                         
         BE    EIIF                FLD INV                                      
         CLI   CTHDAY,X'7F'        M-SU                                         
         BNE   KEYV2                                                            
         B     KEYV5                                                            
*                                                                               
KEYV2    CLI   CTHDAY,X'7C'        M-F                                          
         BNE   KEYV3                                                            
         B     KEYV5                                                            
*                                                                               
KEYV3    MVI   WORK,X'40'                                                       
         MVI   WORK+1,0                                                         
         LA    R6,7                FOR BCT                                      
*                                                                               
KEYV3A   MVC   KEYV3B+1(1),WORK       ALTER TEST UNDER MASK                     
KEYV3B   TM    CTHDAY,X'00'                                                     
         BNO   KEYV3C                                                           
         CLI   WORK+1,0                                                         
         BNE   EIIF                ONLY ONE DAY CAN BE INPUT                    
         MVI   WORK+1,1                                                         
*                                                                               
KEYV3C   ZIC   R0,WORK                                                          
         SRL   R0,1                FOR NEXT MASK                                
         STC   R0,WORK                                                          
         BCT   R6,KEYV3A                                                        
*                                                                               
KEYV5    DS    0H                                                               
         LA    R1,HUTTIMH                                                       
         GOTO1 AFVAL                                                            
         ZIC   R5,FLDH+5                                                        
         GOTO1 =V(TIMVAL),DMCB,((R5),FLD),CTHTIME,RR=RB                         
         CLI   DMCB,X'FF'          TIME INV                                     
         BE    EIIF                                                             
         CLC   CTHTIME(4),=C'NONE'    DON'T ALLOW NONE                          
         BE    EIIF                                                             
         CLC   CTHTIME+2(2),=2X'00'      NO END TIME ALLOWED                    
         BNE   EIIF                                                             
         MVC   DUB(2),CTHTIME                                                   
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'          ZERO REMAINDER - OK                      
         BE    KEYV6                                                            
         CP    DUB+6(2),=P'30'     REMAINDER 30 - HALF HOUR                     
         BE    KEYV6                                                            
         B     EIIF                                                             
*                                                                               
KEYV6    LA    R1,HUTYEARH         YEAR                                         
         GOTO1 AFVAL                                                            
         BZ    EXIT                REQUIRED                                     
         TM    FLDH+4,X'08'        TEST NUMERIC                                 
         BZ    EIIF                                                             
         L     R0,FLDH             HAS BINARY VALUE                             
         C     R0,=F'99'                                                        
         BH    EIIF                                                             
         STC   R0,CTHYEAR                                                       
         B     KEYV7                                                            
*                                                                               
KEYV7    MVC   KEY,CTHKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,HUTDAYH                                                       
         ST    R1,FADR                                                          
         CLI   ACTN,2              CAN'T CHG KEY ON CHG ACTN                    
         BNE   KEYV7A                                                           
         CLC   KEY,LKEY                                                         
         BE    KEYV7A                                                           
         MVI   ACTN,3              SET ACTN TO DISPLAY ON KEY CHG               
*                                                                               
KEYV7A   DS    0H                                                               
         CLI   ACTN,3                                                           
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD               READ ERROR RECORD                            
         BZ    EIIO                DISK ERROR                                   
         TM    DMCB+8,X'10'        TEST FOR NOT FOUND                           
         BZ    KEYV8                                                            
         CLI   ACTN,1              ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV8    CLI   ACTN,1              CAN'T EXIST FOR ADD                          
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,3              A DELETED RECORD CAN ONLY BE DISP            
         BNE   ERNF                                                             
         CLI   ACTN,2                                                           
         BE    DATAVAL                                                          
         EJECT                                                                  
DISPLAY  DS    0H                                                               
         LA    R5,CTHDATA                                                       
         SR    R1,R1                                                            
DISP1    CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                BAD REC                                      
         CLI   0(R5),X'90'                                                      
         BE    DISP2                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP1                                                            
*                                                                               
DISP2    DS    0H                                                               
         USING CTHUTD,R5                                                        
         LA    R6,48               FOR BCT                                      
         LA    R7,HUTJAN1H                                                      
         LA    R8,CTHUTLST                                                      
*                                                                               
DISP3    EDIT  (B2,0(R8)),(4,8(R7)),1,ALIGN=LEFT                                
         OI    6(R7),X'80'          TRANSMIT                                    
         LA    R8,2(R8)                                                         
         BAS   RE,NEXTUN                                                        
         BCT   R6,DISP3                                                         
*                                                                               
DISPX    TM    CTHSTAT,X'80'                                                    
         BO    DISPX1                                                           
         MVI   NACTN,X'03'         SET OK TO CHG/DEL                            
         LA    R1,HUTJAN1H                                                      
         ST    R1,FADR                                                          
         B     DISPXX                                                           
*                                                                               
DISPX1   MVI   NACTN,X'04'         SET OK TO RESTORE                            
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
DISPXX   B     EXIT                                                             
         EJECT                                                                  
DATAVAL  DS    0H                                                               
         MVI   TEMP,0                                                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL              ADD ACTIVITY ELEM                            
         LA    R7,HUTJAN1H                                                      
         ST    R7,FADR                                                          
         LA    R6,48               FOR BCT                                      
         LA    R5,TEMP             BUILD ELEM IN TEMP                           
         XC    0(150,R5),0(R5)                                                  
         MVC   0(2,R5),=X'9064'       SET CODE AND LENGHT                       
         LA    R5,2(R5)                                                         
*                                                                               
DATAV2   CLI   5(R7),0                                                          
         BE    EMIF                MISSING                                      
         ZIC   R0,5(R7)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R7)),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    EIIF                                                             
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BL    EIIF                CAN'T BE NEGATIVE                            
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'         MUST GET ZERO REMAINDER                   
         BNE   EIIF                                                             
         CP    DUB(6),=P'1000'     CAN'T EXCEED 100.0                           
         BH    EFTB                EXCEEDS MAX                                  
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         STH   R0,DUB                                                           
         MVC   0(2,R5),DUB                                                      
         LA    R5,2(R5)               NEXT HUT                                  
         BAS   RE,NEXTUN                                                        
         ST    R7,FADR                                                          
         BCT   R6,DATAV2                                                        
         GOTO1 ABLDREC             ADD HUT ELEM                                 
         EJECT                                                                  
         LA    R1,HUTDAYH                                                       
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         CLI   ACTN,1                                                           
         BNE   UPD1                                                             
         GOTO1 AADD                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     UPDX                                                             
         SPACE 2                                                                
UPD1     GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
UPDX     MVI   NACTN,X'03'         SET OK TO CHANGE/DELETE                      
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         SPACE 2                                                                
NEXTUN   DS    0H                                                               
         ZIC   R0,0(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T GET HERE                           
         TM    1(R7),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMEDD                                                                      
       ++INCLUDE CTLFMEDD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010CTLFM12   05/01/02'                                      
         END                                                                    
