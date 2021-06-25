*          DATA SET SPREPFXAN9 AT LEVEL 083 AS OF 08/24/98                      
*PHASE SPFX026                                                                  
         TITLE 'SPFX02 - FIX CANADIAN PXC RECORDS'                              
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
FX10     DS    0H                                                               
         GOTO1 LOADER,DMCB,=CL8'T00A7A '    STAPACK                             
         ICM   R0,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R0,MYSTAPCK                                                      
*                                                                               
         ZAP   COUNT,=P'0'                                                      
         ZAP   ERCNT,=P'0'                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D70'         PXC RECS                                 
         GOTO1 HIGH                                                             
         B     FX20                                                             
FX20HI   GOTO1 HIGH                                                             
FX20SEQ  GOTO1 SEQ                                                              
FX20     LA    R3,KEY                                                           
         CLC   =X'0D70',0(R3)      STILL PXC                                    
         BNE   FX200                                                            
*                                                                               
         CLC   =X'FFFFFF',5(R3)    STATION = ALL                                
         BE    FX20SEQ                                                          
*                                                                               
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         MVC   BMED,2(R3)          MED HEX                                      
         NI    BMED,X'0F'                                                       
*                                                                               
         LA    R2,AGYTABLE         IS THIS A CANADIAN AGENCY                    
FX25     CLC   BYTE,0(R2)                                                       
         BNE   FX27                                                             
         CLC   QAGY,1(R2)                                                       
         BE    FX30                                                             
FX27     LA    R2,3(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    FX20SEQ             NEXT                                         
         B     FX25                                                             
*                                                                               
FX30     GOTO1 GETBUY                                                           
         MVC   SAVEKEY,KEY                                                      
         L     R3,ADBUY                                                         
         XC    STAWORK,STAWORK                                                  
         LA    R6,STAWORK                                                       
         USING STAPACKD,R6                                                      
*                                                                               
         MVI   STAPACT,C'U'        UNPACK - WITHOUT MEDIA AND COUNTRY           
         MVC   STAPAGY,1(R2)                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPSTA,5(R3)       STATION                                      
*                                                                               
         GOTO1 MYSTAPCK,(R6)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(6),=C'BEFORE'                                                  
         MVC   P+15(L'STAPQSTA+L'STAPQNET),STAPQSTA                             
         GOTO1 HEXOUT,DMCB,0(R3),P+25,14,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
         MVI   STAPACT,C'P'        PACK- WITH CORRECT MEDIA AND COUNTRY         
         MVC   STAPAGY,1(R2)                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVI   STAPCTRY,C'C'       CANADIAN                                     
         MVI   STAPMED,C'T'                                                     
         CLI   BMED,X'01'                                                       
         BE    FX40                                                             
         MVI   STAPMED,C'C'                                                     
         CLI   BMED,X'08'                                                       
         BE    FX40                                                             
         MVI   STAPMED,C'N'                                                     
         CLI   BMED,X'03'                                                       
         BE    FX40                                                             
         DC    H'0'                                                             
*                                                                               
FX40     GOTO1 MYSTAPCK,(R6)                                                    
         CLI   STAPERR,0                                                        
         BE    FX50                                                             
         MVC   P(5),=C'ERROR'                                                   
         MVC   P+25(30),0(R6)                                                   
         GOTO1 REPORT                                                           
         AP    ERCNT,=P'1'                                                      
         B     FX20SEQ                                                          
*                                                                               
FX50     OI    KEY+13,X'80'        DELETE BAD REC                               
         GOTO1 WRITE                                                            
         MVC   5(3,R3),STAPSTA     CORRECT STATION                              
         MVC   KEY(13),0(R3)                                                    
         NI    KEY+13,X'FF'-X'80'                                               
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    FX55                                                             
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,(R3),DMWORK           
*                                                                               
FX55     MVC   P(5),=C'ADDED'                                                   
         GOTO1 HEXOUT,DMCB,0(R3),P+25,14,=C'TOG'                                
         GOTO1 REPORT                                                           
         AP    COUNT,=P'1'                                                      
         MVC   KEY,SAVEKEY                                                      
         B     FX20HI                                                           
*                                                                               
FX200    MVC   P(17),=C'NUMBER FIXED     '                                      
         EDIT  (P6,COUNT),(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK          
         GOTO1 REPORT                                                           
         MVC   P(17),=C'NUMBER IN ERROR  '                                      
         EDIT  (P6,ERCNT),(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK          
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
AGYTABLE DS    0H                                                               
         DC    X'90',C'DA'         SPOT5                                        
         DC    X'40',C'HD'         SPOTF                                        
         DC    X'B0',C'YR'         SPOTE                                        
         DC    X'90',C'RX'         SPOTL                                        
         DC    X'50',C'MI'         SPOTG                                        
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
MYSTAPCK DS    F                                                                
COUNT    DS    PL6                                                              
ERCNT    DS    PL6                                                              
BAGYMED  DS    X                                                                
ELCODE   DS    X                                                                
AGYALPH  DS    CL2                                                              
BMED     DS    XL1                                                              
SAVEKEY  DS    XL20                                                             
SVKEY    DS    XL10                AM(1),CLT(2),PRD(1),MKSTA(5),EST(1)          
STAWORK  DS    XL31                                                             
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
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
*                                                                               
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083SPREPFXAN908/24/98'                                      
         END                                                                    
