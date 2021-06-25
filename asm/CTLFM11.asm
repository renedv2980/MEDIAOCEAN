*          DATA SET CTLFM11    AT LEVEL 006 AS OF 05/01/02                      
*PHASE TA0211A                                                                  
         TITLE 'CTLFM11 - CONTROL FILE MAINT - PROJECTION FACTORS'              
CTLFM11  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 001,**LFMH**                                                     
         USING WRKD,RC             RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTYREC,R4           R4=A(RECORD)                                 
*&&UK*&& MVI   COUNTRY,UK                                                       
*&&US*&& MVI   COUNTRY,US                                                       
         EJECT                                                                  
*              READ ID RECORD AND EXTRACT AGENCY ID FOR SPOT SYSTEM             
*                                                                               
KEYVAL   LA    R5,IOAREA                                                        
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY       BUILD A KEY                                  
         MVI   CTIKEY,C'I'                                                      
         LR    RE,R2                                                            
         USING TWAD,RE                                                          
         MVC   CTIKEY+L'CTIKEY-2(2),TWAUSRID                                    
         MVC   KEY,CTIKEY                                                       
         DROP  RE                                                               
         GOTO1 AREAD               READ ID RECORD                               
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         SR    R1,R1               GET SEOVNUM OF MEDIA (TV) SYSTEM             
         IC    R1,COUNTRY                                                       
         LA    R1,MEDTBL-1(R1)                                                  
         MVC   WORK(1),0(R1)                                                    
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
*                                  NOW FIND SYSTEM ELEMENT ON ID REC            
KEYV2    CLI   0(R5),0                                                          
         BE    EANA                NOT AUTHORIZED FOR SYSTEM                    
         CLI   0(R5),X'21'                                                      
         BNE   KEYV4                                                            
         USING CTSYSD,R5                                                        
         CLC   CTSYSNUM,WORK                                                    
         BNE   KEYV4                                                            
         MVC   AGYALPH,CTSYSAGA    EXTRACT AGENCY DETAILS                       
         MVC   AGYBIN,CTSYSAGB                                                  
         MVC   AGYSEN,CTSYSSE                                                   
         B     KEYV6                                                            
*                                                                               
KEYV4    IC    R6,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R6                                                            
         B     KEYV2                                                            
         DROP  R5                                                               
*                                  VALIDATE KEY FIELDS                          
KEYV6    XC    CTYKEY,CTYKEY                                                    
         MVI   CTYKEY,C'Y'         BUILD VIRGIN KEY                             
         MVC   CTYKAGY,AGYALPH                                                  
         CLI   COUNTRY,US                                                       
         BE    *+16                                                             
         MVC   CTYKAGY(1),AGYSEN   UK HAS DIFFERENT AGENCY KEY                  
         MVC   CTYKAGY+1(1),AGYBIN                                              
*                                  VALIDATE FACTOR CODE                         
         LA    R1,FACCODEH                                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         MVC   CTYKFORM,FLD                                                     
*                                  VALIDATE DATE                                
         LA    R1,FACDATEH                                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         GOTO1 VDATVAL,DMCB,(2,FLD),WORK                                        
         OC    DMCB(4),DMCB                                                     
         BZ    EIDF                                                             
         PACK  DUB,WORK(2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,CTYKYEAR                                                      
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB                                                           
         STC   R1,CTYKMON                                                       
*                                                                               
         LA    R1,FACCODEH                                                      
         ST    R1,FADR                                                          
         MVC   KEY,CTYKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE         SET ACTN TO DISPLAY IF ACTN IS               
         BNE   *+18                CHANGE AND RECORD IS NOT ALREADY             
         CLC   KEY,LKEY            DISPLAYED                                    
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU INDIC                                
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        N/F ONLY VALID FOR ADD                       
         BZ    *+16                                                             
         CLI   ACTN,ADD                                                         
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            RECORD CAN'T EXIST FOR ADD                   
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        CAN ONLY DISPLAY DELETED RECORDS             
         BNE   ERID                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY PROJECTION FACTOR RECORD                                 
*                                                                               
DISPREC  LA    R1,FACFACTH         CLEAR DOWN TWA                               
         GOTO1 ACLEAR                                                           
         LA    R5,CTYDATA                                                       
*                                                                               
DISPR2   CLI   0(R5),0             END OF RECORD                                
         BE    DISPEND                                                          
         CLI   0(R5),X'78'         PROJECTION FACTOR ELEMENT                    
         BE    DISPFAC                                                          
*                                                                               
DISPR4   SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     DISPR2                                                           
*                                  DISPLAY PROJECTION FACTOR ELEMENT            
DISPFAC  DS    0H                                                               
         USING CTPFD,R5                                                         
         EDIT  (B2,CTPFACT),(4,FACFACT),ALIGN=LEFT                              
         B     DISPR4                                                           
         DROP  R5                                                               
*                                  SET NEXT ACTION & EXIT                       
DISPEND  MVI   FERN,X'FF'                                                       
         LA    R1,FACFACTH                                                      
         ST    R1,FADR                                                          
         TM    CTYSTAT,X'80'                                                    
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/CHANGE PROJECTION FACTOR RECORD                              
*                                                                               
DATAVAL  MVI   TEMP,0                                                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
*                                                                               
         XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'7804'                                                 
         LA    R5,TEMP                                                          
         USING CTPFD,R5                                                         
*                                  VALIDATE PROJECTION FACTOR                   
         LA    R1,FACFACTH                                                      
         GOTO1 AFVAL               MUST BE PRESENT                              
         BZ    EXIT                                                             
         TM    FLDH+4,X'08'        AND NUMERIC                                  
         BZ    EFNN                                                             
         OC    FLDH(4),FLDH        AND NON-ZERO                                 
         BZ    EFLM                                                             
         MVC   CTPFACT,FLDH+2                                                   
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         DROP  R5                                                               
*                                                                               
         L     RF,AWRITE                                                        
         CLI   ACTN,CHANGE                                                      
         BE    *+8                                                              
         L     RF,AADD                                                          
         LA    R1,FACCODEH         SET FADR TO FIRST KEY FIELD                  
         ST    R1,FADR                                                          
         BASR  RE,RF               ADD/WRITE RECORD                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                  GENERATE TURNAROUND REQUEST                  
         LA    R5,TEMP                                                          
         USING REQD,R5                                                          
         XC    REQHDR,REQHDR                                                    
         MVI   REQUEST,C' '                                                     
         MVC   REQUEST+1(L'REQUEST-1),REQUEST                                   
         MVI   REQNUMB,32                                                       
         MVC   REQPROG,=C'32'                                                   
         MVC   REQAGYA,CTYKAGY                                                  
         MVC   REQSTOR,=C'FILE CONTROL'                                         
         GOTO1 VDATAMR,DMCB,=C'DMADD',=C'CTREQ  ',(R5),(R5)                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         MVI   FERN,X'FF'          SET NEXT ACTION & EXIT                       
         MVI   NACTN,OKCHA+OKDEL                                                
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
MEDTBL   DC    X'0204'             US/UK SPOT SYSTEM SE NUMBERS                 
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WRKD     DSECT                                                                  
COUNTRY  DS    C                                                                
AGYALPH  DS    CL2                                                              
AGYBIN   DS    C                                                                
AGYSEN   DS    C                                                                
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMREQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTLFMREQ                                                       
         PRINT ON                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
*                                                                               
US       EQU   1                                                                
UK       EQU   2                                                                
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMEED                                                                      
       ++INCLUDE CTLFMEED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTLFM11   05/01/02'                                      
         END                                                                    
