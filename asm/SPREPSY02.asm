*          DATA SET SPREPSY02  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPSY02A                                                                  
         TITLE 'SPSY02 - CHILD SPOT SYNDICATED PROGRAM LIST'                    
SPSY02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSY02,RC,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,ESTFRST                                                     
         BE    SY010                                                            
                                                                                
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
                                                                                
SY010    DS    0H                                                               
         ZAP   TMKTWT,=P'0'                                                     
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
                                                                                
         LA    R6,KEY                                                           
         USING CSORECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ   TYPE                                         
         MVI   CSOKSTYP,CSOKSTPQ   SUB-TYPE                                     
         MVC   CSOKAM,BAGYMD       A/M                                          
         MVC   CSOKCLT,BCLT        CLIENT                                       
                                                                                
SY015    GOTO1 HIGH                                                             
         B     SY030                                                            
                                                                                
SY020    GOTO1 SEQ                                                              
                                                                                
SY030    DS    0H                                                               
         LA    R6,KEY                                                           
         CLC   KEY(CSOKMKT-CSOKEY),KEYSAVE     A/M & CLIENT                     
         BNE   SY100                                                            
                                                                                
         ZIC   R1,CSOKEST          COMPARE ESTIMATE                             
         ZIC   R2,BEST                                                          
         BH    SY050                                                            
         BL    SY060                                                            
                                                                                
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         CLC   DSCPROG(4),Q2USER   COMPARE PROGRAM NAME                         
         BNE   SY020                                                            
                                                                                
*PRINT STATION                                                                  
         LA    R5,P1                                                            
         USING PLINED,R5                                                        
         MVC   SAVEKEY,KEY                                                      
         GOTO1 MSUNPK,DMCB,CSOKMKT,PMKT,PSTA      MARKET & STATION              
                                                                                
*PRINT THE MARKET NAME                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTKEY,R4                                                        
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMED        MEDIA                                        
         MVC   MKTKMKT,PMKT        MARKET NUMBER                                
         MVC   MKTKAGY,QAGY        AGENCY                                       
                                                                                
         MVC   DMCB+12,ADMARKET                                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY                      
                                                                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,ADMARKET                                                      
         CLC   KEY(MKTKFILL-MKTKEY),MKTKEY  SAME TYPE/MEDIA?                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   PMKTNME,MKTNAME     MARKET NAME                                  
                                                                                
         OC    MKTWT,MKTWT         MARKET WEIGHT                                
         BZ    SY040                                                            
         EDIT  (C4,MKTWT),(10,PMKTWT),2                                         
         PACK  PAMKTWT,MKTWT                                                    
         AP    TMKTWT,PAMKTWT                                                   
                                                                                
SY040    MVC   KEY,SAVEKEY                                                      
         DROP  R5                                                               
         GOTO1 REPORT                                                           
                                                                                
SY050    LA    R6,KEY              GET THE NEXT STATION                         
         MVC   CSOKEST(3),=X'FFFFFF'                                            
         B     SY015                                                            
                                                                                
SY060    LA    R6,KEY                                                           
         STC   R2,CSOKEST          PUT IN ESTIMATE NUMBER                       
         B     SY015                                                            
         DROP  R6                                                               
                                                                                
SY100    MVI   P1,0                                                             
         MVC   P2(19),=C'TOTAL MARKET WEIGHT'                                   
         EDIT  (P4,TMKTWT),(7,P2+53),2                                          
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
HDHOOK   NTR1                                                                   
         MVI   H4,0                                                             
                                                                                
* CENTER THE PROGRAM NAME                                                       
         LA    RF,Q2USER+19        LONGEST POSSIBLE IS 20 CHARACTERS            
         LA    R1,Q2USER                                                        
HDHOOK10 CR    R1,RF                                                            
         BE    HDHOOK20                                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,HDHOOK10                                                      
                                                                                
HDHOOK20 SR    RF,R1               LENGTH OF COMMENT - 1                        
         LA    RF,11(RF)           ADD LEN OF 'PROGRAM - ' + 1                  
         SR    RE,RE                                                            
         D     RE,=F'2'            HALF OF LENGTH                               
         LA    R1,H5+54            CENTER OF PAGE                               
         SR    R1,RF                                                            
                                                                                
         MVC   0(9,R1),=C'PROGRAM -'                                            
         MVC   10(20,R1),Q2USER                                                 
         MVI   H6,0                                                             
                                                                                
         MVI   H9,0                                                             
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PMKT(6),=C'MARKET'                                               
         MVC   PMKTNME(11),=C'MARKET NAME'                                      
         MVC   PSTA(7),=C'STATION'                                              
         MVC   PMKTWT(10),=C'MKT WEIGHT'                                        
         LA    R2,H11                                                           
         MVC   PMKT,=C'------'                                                  
         MVC   PMKTNME,=C'------------------------'                             
         MVC   PSTA(7),=C'-------'                                              
         MVC   PMKTWT(10),=C'----------'                                        
         DROP  R2                                                               
         XIT                                                                    
***********************************************************************         
                                                                                
         GETEL R6,24,ELCODE                                                     
                                                                                
ELCODE   DS    X                                                                
SAVEKEY  DS    CL32                                                             
TMKTWT   DS    PL4                 TOTAL MARKET WEIGHT                          
PAMKTWT  DS    PL4                 PACKED MARKET WEIGHT                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
PLINED   DSECT                                                                  
PMKT     DS    CL6                                                              
         DS    CL5                                                              
PMKTNME  DS    CL24                                                             
         DS    CL5                                                              
PSTA     DS    CL5                                                              
         DS    CL5                                                              
PMKTWT   DS    CL10                                                             
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENCSO                                                       
MKTD     DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPSY02 08/29/00'                                      
         END                                                                    
