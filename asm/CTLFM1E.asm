*          DATA SET CTLFM1E    AT LEVEL 002 AS OF 08/22/00                      
*PHASE TA021EA                                                                  
         TITLE 'CTLFM1E - CONTROL FILE MAINT - BROADCAST MSGS'                  
CTLFM1E  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**LFM1E*                                                     
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CBCREC,R4           R4=A(BROADCAST MESSAGE RECORD)               
         EJECT                                                                  
KEYVAL   XC    CBCKEY,CBCKEY       INITIALISE KEY                               
         MVI   CBCKEY,C'4'                                                      
         SPACE 2                                                                
KEYV1    LA    R1,BCMIDH           VALIDATE MESSAGE ID                          
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING MESSAGE ID FIELD                     
         CLI   FLDH+5,3                                                         
         BL    EFTS                MINIMUM OF THREE CHRS                        
         MVC   CBCKID,FLD                                                       
         SPACE 2                                                                
KEYV2    LA    R1,BCMDATEH         VALIDATE MESSAGE DATE                        
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING DATE FIELD                           
KEYV2A   CLC   FLD(4),=C'NONE'                                                  
         BNE   KEYV2B                                                           
         XC    CBCKDATE,CBCKDATE                                                
         B     KEYV2X                                                           
KEYV2B   GOTO1 VDATVAL,DMCB,(0,FLD),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    EIIF                                                             
         GOTO1 VDATCON,DMCB,(0,DUB),(3,CBCKDATE)                                
KEYV2X   EQU   *                                                                
         SPACE 2                                                                
KEYV3    EQU   *                   NO MORE KEY FIELDS                           
         SPACE 2                                                                
KEYV4    MVC   KEY,CBCKEY          CHECK KEY COMPATIBLE WITH ACTION             
         MVC   KEYNEXT,KEY                                                      
         LA    R1,BCMIDH                                                        
         ST    R1,FADR                                                          
         CLI   ACTN,2              CANT CHANGE KEY ON CHANGE ACTION             
         BNE   KEYV4A                                                           
         CLC   KEY,LKEY                                                         
         BE    KEYV4A                                                           
         MVI   ACTN,3              SET ACTN TO DISPLAY ON KEY CHANGE            
*                                                                               
KEYV4A   DS    0H                                                               
         CLI   ACTN,3                                                           
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD               READ BROADCAST MESSAGE RECORD                
         BZ    EIIO                DISK ERROR                                   
         TM    DMCB+8,X'10'        TEST FOR NOT FOUND                           
         BZ    KEYV5                                                            
         CLI   ACTN,1              ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV5    CLI   ACTN,1              CANT EXIST FOR ADD                           
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,3              A DELETED RECORD CAN ONLY BE DISP            
         BNE   ERNF                                                             
         CLI   ACTN,2                                                           
         BE    DATAVAL                                                          
         EJECT                                                                  
DISPLAY  LA    R5,CBCDATA          DISPLAY RECORD                               
         SR    R1,R1                                                            
         SPACE 2                                                                
DISP2    CLI   0(R5),0             FIND LINE LIST ELEMENT                       
         BNE   DISP2A                                                           
         LA    R5,CBCDATA                                                       
         XC    BCMLIST,BCMLIST     SET DEFAULT LINE LIST DATA                   
         SR    R1,R1                                                            
         B     DISP2C                                                           
DISP2A   CLI   0(R5),X'02'         LINE LIST DATA IS X'02' EL                   
         BE    DISP2B                                                           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                                                               
DISP2B   IC    R1,1(R5)            R5=A(DESC EL)                                
         SH    R1,=H'3'                                                         
         XC    BCMLIST,BCMLIST                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCMLIST(0),2(R5)                                                 
         LA    R1,1(R1)                                                         
DISP2C   OI    BCMLISTH+6,X'80'                                                 
         STC   R1,BCMLISTH+5                                                    
         SPACE 2                                                                
DISP10   CLI   0(R5),0             FIND SUBJECT ELEMENT                         
         BNE   DISP10A                                                          
         LA    R5,CBCDATA                                                       
         XC    BCMSUBJ,BCMSUBJ     SET DEFAULT SUBJECT DATA                     
         SR    R1,R1                                                            
         B     DISP10C                                                          
DISP10A  CLI   0(R5),X'10'         SUBJECT DATA IS X'10' ELEMENT                
         BE    DISP10B                                                          
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP10                                                           
*                                                                               
DISP10B  IC    R1,1(R5)            R5=A(DESC EL)                                
         SH    R1,=H'3'                                                         
         XC    BCMSUBJ,BCMSUBJ                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCMSUBJ(0),2(R5)                                                 
         LA    R1,1(R1)                                                         
DISP10C  OI    BCMSUBJH+6,X'80'                                                 
         STC   R1,BCMSUBJH+5                                                    
         SPACE 2                                                                
DISPA    CLI   0(R5),0             FIND FIRST MESSAGE TEXT LINE                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'11'                                                      
         BE    DISPA1                                                           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISPA                                                            
*                                                                               
DISPA1   IC    R1,1(R5)            R5=A(MESSAGE ELEMENT)                        
         SH    R1,=H'3'                                                         
         XC    BCMM1,BCMM1                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCMM1(0),2(R5)                                                   
         LA    R1,1(R1)                                                         
         OI    BCMM1H+6,X'80'                                                   
         STC   R1,BCMM1H+5                                                      
         SPACE 2                                                                
DISPB    CLI   0(R5),0             FIND SECOND MESSAGE TEXT LINE                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'12'                                                      
         BE    DISPB1                                                           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISPB                                                            
*                                                                               
DISPB1   IC    R1,1(R5)            R5=A(MESSAGE ELEMENT)                        
         SH    R1,=H'3'                                                         
         XC    BCMM2,BCMM2                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCMM2(0),2(R5)                                                   
         LA    R1,1(R1)                                                         
         OI    BCMM2H+6,X'80'                                                   
         STC   R1,BCMM2H+5                                                      
         SPACE 2                                                                
DISPC    CLI   0(R5),0             FIND THIRD MESSAGE TEXT LINE                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'13'                                                      
         BE    DISPC1                                                           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISPC                                                            
*                                                                               
DISPC1   IC    R1,1(R5)            R5=A(MESSAGE ELEMENT)                        
         SH    R1,=H'3'                                                         
         XC    BCMM3,BCMM3                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCMM3(0),2(R5)                                                   
         LA    R1,1(R1)                                                         
         OI    BCMM3H+6,X'80'                                                   
         STC   R1,BCMM3H+5                                                      
         SPACE 2                                                                
DISPD    CLI   0(R5),0             FIND FOURTH MESSAGE TEXT LINE                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'14'                                                      
         BE    DISPD1                                                           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISPD                                                            
*                                                                               
DISPD1   IC    R1,1(R5)            R5=A(MESSAGE ELEMENT)                        
         SH    R1,=H'3'                                                         
         XC    BCMM4,BCMM4                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCMM4(0),2(R5)                                                   
         LA    R1,1(R1)                                                         
         OI    BCMM4H+6,X'80'                                                   
         STC   R1,BCMM4H+5                                                      
         SPACE 2                                                                
DISPX    TM    CBCSTAT,X'80'       TEST DELETED RECORD                          
         BO    DISPX1                                                           
         MVI   NACTN,X'03'         SET OK TO CHANGE/DELETE                      
         LA    R1,BCMLISTH                                                      
         ST    R1,FADR                                                          
         B     DISPXX                                                           
DISPX1   MVI   NACTN,X'04'         SET OK TO RESTORE                            
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
DISPXX   B     EXIT                                                             
         EJECT                                                                  
DATAVAL  MVI   TEMP,0              INITIALIZE RECORD                            
         GOTO1 ABLDREC                                                          
         SPACE 2                                                                
VALD2    LA    R1,BCMLISTH         VALIDATE LINE LIST                           
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING LINE LIST                            
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LA    R1,2(R1)                                                         
         MVI   TEMP,X'02'                                                       
         STC   R1,TEMP+1                                                        
         MVC   TEMP+2(60),FLD                                                   
         GOTO1 ABLDREC             ADD LINE LIST ELEMENT                        
         SPACE 2                                                                
VALD10   LA    R1,BCMSUBJH         VALIDATE SUBJECT                             
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING SUBJECT                              
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LA    R1,2(R1)                                                         
         MVI   TEMP,X'10'                                                       
         STC   R1,TEMP+1                                                        
         MVC   TEMP+2(60),FLD                                                   
         GOTO1 ABLDREC             ADD SUBJECT ELEMENT                          
         SPACE 2                                                                
VALDA    LA    R1,BCMM1H           VALIDATE MESSAGE TEXT LINE ONE               
         GOTO1 AFVAL                                                            
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LTR   R1,R1               SET EMPTY LINE TO ONE SPACE CHR              
         BNZ   *+8                                                              
         LA    R1,1                                                             
         LA    R1,2(R1)                                                         
         MVI   TEMP,X'11'                                                       
         STC   R1,TEMP+1                                                        
         MVC   TEMP+2(60),FLD                                                   
         GOTO1 ABLDREC             ADD ERROR MESSAGE ELEMENT                    
         SPACE 2                                                                
VALDB    LA    R1,BCMM2H           VALIDATE MESSAGE TEXT LINE TWO               
         GOTO1 AFVAL                                                            
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LTR   R1,R1               SET EMPTY LINE TO ONE SPACE CHR              
         BNZ   *+8                                                              
         LA    R1,1                                                             
         LA    R1,2(R1)                                                         
         MVI   TEMP,X'12'                                                       
         STC   R1,TEMP+1                                                        
         MVC   TEMP+2(60),FLD                                                   
         GOTO1 ABLDREC             ADD ERROR MESSAGE ELEMENT                    
         SPACE 2                                                                
VALDC    LA    R1,BCMM3H           VALIDATE MESSAGE TEXT LINE THREE             
         GOTO1 AFVAL                                                            
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LTR   R1,R1               SET EMPTY LINE TO ONE SPACE CHR              
         BNZ   *+8                                                              
         LA    R1,1                                                             
         LA    R1,2(R1)                                                         
         MVI   TEMP,X'13'                                                       
         STC   R1,TEMP+1                                                        
         MVC   TEMP+2(60),FLD                                                   
         GOTO1 ABLDREC             ADD ERROR MESSAGE ELEMENT                    
         SPACE 2                                                                
VALDD    LA    R1,BCMM4H           VALIDATE MESSAGE TEXT LINE FOUR              
         GOTO1 AFVAL                                                            
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LTR   R1,R1               SET EMPTY LINE TO ONE SPACE CHR              
         BNZ   *+8                                                              
         LA    R1,1                                                             
         LA    R1,2(R1)                                                         
         MVI   TEMP,X'14'                                                       
         STC   R1,TEMP+1                                                        
         MVC   TEMP+2(60),FLD                                                   
         GOTO1 ABLDREC             ADD ERROR MESSAGE ELEMENT                    
         EJECT                                                                  
         LA    R1,BCMIDH           POSN TO 1ST KEY FLD & SET OK                 
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
* CTLFMFE1                                                                      
       ++INCLUDE CTLFME1D                                                       
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTLFM1E   08/22/00'                                      
         END                                                                    
