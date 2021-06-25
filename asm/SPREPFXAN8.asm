*          DATA SET SPREPFXAN8 AT LEVEL 072 AS OF 07/28/98                      
*PHASE SPFX026                                                                  
         TITLE 'SPFX02 - DELETE RECS WITH BAD CABLE STAIONS'                    
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
         GOTO1 LOADER,DMCB,=CL8'T00A7A '    STATPACK A VERSION                  
         ICM   R0,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R0,MYSTAPCK                                                      
*                                                                               
         ZAP   COUNT,=P'0'                                                      
         XC    KEY,KEY                                                          
*        MVI   KEY,X'0B'                                                        
         MVI   KEY,X'C1'           <==SJR                                       
FX20HI   GOTO1 HIGH                                                             
         B     FX20                                                             
FX20SEQ  GOTO1 SEQ                                                              
FX20     LA    R3,KEY                                                           
         CLC   =X'0D34',0(R3)      DARE ORDER                                   
         BE    DARE                                                             
         CLC   =X'0D7A',0(R3)      WIPW                                         
         BE    PW                                                               
         CLC   =X'0D7B',0(R3)      DOUBLE BOOK RECORDS                          
         BE    DBK                                                              
         CLC   =X'0D67',0(R3)      NEW BUYERS WORKSHEET                         
         BE    NBW                                                              
         CLI   0(R3),X'0D'                                                      
         BNH   FX20SEQ                                                          
*        CLI   0(R3),X'10'         TEST BUYREC ** FOR SJ ONLY **                
         CLI   0(R3),X'C1'         TEST BUYREC ** FOR SJ ONLY **                
         BE    BUY                                                              
         B     FX200                                                            
*                                                                               
FX100DEL DS    0H                                                               
         OI    KEY+13,X'80'                                                     
*        GOTO1 WRITE                                                            
         MVC   P+15(8),=C'*INVALID'                                             
         GOTO1 HEXOUT,DMCB,0(R3),P+25,14,=C'TOG'                                
         GOTO1 REPORT                                                           
         AP    COUNT,=P'1'                                                      
         B     FX20SEQ                                                          
*                                                                               
FX200    MVC   P(17),=C'NUMBER DELETED   '                                      
         EDIT  (P6,COUNT),(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK          
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*=================================================================              
* BUY RECORDS   R3=A(KEY)                                                       
*=================================================================              
         SPACE 1                                                                
BUY      DS    0H                                                               
         USING BUYRECD,R3                                                       
         TM    6(R3),X'F0'         TEST CABLE                                   
         BNO   FX20SEQ             NO                                           
*                                                                               
         LA    R2,4(R3)            R2 POINTS TO MKT/STATION                     
         MVC   BAGYMED,0(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*                                                                               
         EJECT                                                                  
*==================================================================             
INV      DS    0H                                                               
         TM    2(R3),X'F0'                                                      
         BNO   FX20SEQ                                                          
*                                                                               
         LR    R2,R3                                                            
         MVC   BAGYMED,1(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*==================================================================             
SNV      DS    0H                                                               
         TM    5(R3),X'F0'                                                      
         BNO   FX20SEQ                                                          
*                                                                               
         LA    R2,3(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*==================================================================             
DARE     DS    0H                                                               
         TM    9(R3),X'F0'                                                      
         BNO   FX20SEQ                                                          
*                                                                               
         LA    R2,7(R3)                                                         
         MVC   BAGYMED,4(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*==================================================================             
PW       DS    0H                                                               
         TM    9(R3),X'F0'                                                      
         BNO   FX20SEQ                                                          
*                                                                               
         LA    R2,7(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*==================================================================             
SID      DS    0H                                                               
         TM    6(R3),X'F0'                                                      
         BNO   FX20SEQ                                                          
*                                                                               
         LA    R2,4(R3)                                                         
         MVC   BAGYMED,1(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*==================================================================             
DBK      DS    0H                                                               
         TM    5(R3),X'F0'                                                      
         BNO   FX20SEQ                                                          
*                                                                               
         LA    R2,3(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*==================================================================             
NBW      DS    0H                                                               
         GOTO1 GETBUY                                                           
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         USING XMTELD,R6                                                        
         BNE   FX20SEQ                                                          
         TM    10(R6),X'F0'                                                     
         BNO   FX20SEQ                                                          
         LA    R2,12(R6)                                                        
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   FX100DEL                                                         
         B     FX20SEQ                                                          
*==================================================================             
*                                                                               
GETSTA   NTR1                      R2 POINTS TO PACKED MKT/STATION              
*                                                                               
*        CLI   BAGYMED,X'C1'       SJ/T                                         
*        BNE   GSXYES                                                           
*                                                                               
*        GOTO1 GET                                                              
*        L     R1,AREC                                                          
*        MVC   AGYALPH,20(R1)                                                   
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R2),P+2,5,=C'TOG'                                  
         MVI   P+13,C'='                                                        
*                                                                               
*        CLC   AGYALPH,SPACES                                                   
*        BH    GS10                                                             
*        GOTO1 REPORT                                                           
*        B     GSXNO                                                            
*                                                                               
GS10     XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'        UNPACK                                       
*        MVC   STAPAGY,AGYALPH                                                  
         MVC   STAPAGY,=C'SJ'      ALPHA AGENCY                                 
         MVI   STAPMED,C'T'        TELEVISION                                   
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R2)      MKT/STA                                      
*                                                                               
         GOTO1 MYSTAPCK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BNE   GSXNO                                                            
*                                                                               
         MVC   P+15(L'STAPQSTA+L'STAPQNET),STAPQSTA                             
         GOTO1 HEXOUT,DMCB,0(R3),P+25,13,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
GSXYES   SR    RC,RC                                                            
GSXNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
MYSTAPCK DS    F                                                                
COUNT    DS    PL6                                                              
BAGYMED  DS    X                                                                
ELCODE   DS    X                                                                
AGYALPH  DS    CL2                                                              
SAVEKEY  DS    XL13                                                             
SAVEKEY2 DS    XL18                                                             
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
**PAN#1  DC    CL21'072SPREPFXAN807/28/98'                                      
         END                                                                    
