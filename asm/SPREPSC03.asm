*          DATA SET SPREPSC03  AT LEVEL 017 AS OF 12/18/98                      
*PHASE SPSC03T,*,NOAUTO                                                         
         TITLE 'MARKET FIX PROGRAM - SUBCONTROLLER'                             
***********************************************************************         
*                                                                     *         
* LEV 15    JUL22/93 ADD CABLE HEAD                                   *         
* LEV 16    JAN31/95 FIX FOR CANADIAN MEDIA C                         *         
* LEV 17    FEB05/98 FIX FOR CANADIAN MEDIA C WAS 5, NOW 8 BITS       *         
* LEV 18    DEC15/98 FIX FOR MEL'S NEW MEDIA (X'08') BAGYMD           *         
*                                                                     *         
***********************************************************************         
SPSC03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSC03                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 1                                                                
         GOTO1 MSPACK,DMCB,QMKT,QSTA,SVMKTSTA                                   
         SPACE 1                                                                
         CLI   QMED,C'C'           CANADIAN COMBINED                            
         BNE   *+8                                                              
         NI    SVMKTSTA+4,X'00'    SET MEDIA 8 BITS TO ZERO                     
         SPACE                                                                  
         CLI   QSTA,C'0'           CABLE STATION                                
         BL    *+8                                                              
         NI    SVMKTSTA+4,X'80'    SET MEDIA 7 BITS TO ZERO                     
         SPACE                                                                  
SPCTL00  DC    0H'0'               GET NEXT CLIENT                              
         GOTO1 FCNXTCLT                                                         
         BE    SPCTL10                                                          
         XIT1                                                                   
         SPACE 1                                                                
SPCTL10  MVI   MODE,CLTFRST                                                     
         GOTO1 GO                                                               
         SPACE 1                                                                
SPCTL20  MVC   KEY(1),SVAGYMD                                                   
         MVC   KEY+1(2),SVCLT                                                   
         MVI   KEY+3,1                                                          
         SPACE                                                                  
SPCTL26  MVC   KEY+4(5),SVMKTSTA                                                
         SPACE                                                                  
SPCTL30  XC    KEY+9(8),KEY+9                                                   
         GOTO1 HIGH                                                             
         B     SPCTL50                                                          
         SPACE                                                                  
SPCTL40  DS    0H                                                               
         GOTO1 SEQ                                                              
         SPACE                                                                  
SPCTL50  CLC   KEYSAVE(9),KEY      SAME A-M/CLT/PRD/MKT/STA?                    
         BE    SPCTL60                                                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD STATION                    
         BL    SPCTL52                                                          
         MVC   DUB(5),KEY+4                                                     
         NI    DUB+4,X'80'                                                      
         CLC   DUB(5),SVMKTSTA                                                  
         BE    SPCTL60                                                          
         SPACE                                                                  
SPCTL52  CLC   KEYSAVE(3),KEY      SAME A-M/CLT?                                
         BE    SPCTL56              YES                                         
         SPACE                                                                  
SPCTL54  DS    0H                                                               
         TM    KEYSAVE,X'08'       DONE SPECIAL MEDIA FOR MEL                   
         BO    SPCTL00              GO TO NEXT CLIENT                           
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVAGYMD                                                   
         OI    KEY,X'08'                                                        
         MVC   KEY+1(2),SVCLT                                                   
         MVI   KEY+3,X'FF'                                                      
         SPACE                                                                  
         MVC   KEY+4(5),SVMKTSTA                                                
         SPACE                                                                  
         GOTO1 HIGH                                                             
         B     SPCTL50                                                          
         SPACE                                                                  
SPCTL56  DS   0H                                                                
         CLC   KEYSAVE(4),KEY      SAME A-M/CLT/PRD?                            
         BE    SPCTL70                                                          
         SPACE                                                                  
         B     SPCTL26                                                          
         SPACE                                                                  
SPCTL60  MVI   MODE,PROCBUY        UPDATE THE SPTDIR RECORD                     
         GOTO1 GO                                                               
         CLI   USRSW1,C'N'                                                      
         BE    SPCTL40             PASSIVE ELEMENT - READ SEQ                   
         MVI   USRSW1,C'N'                                                      
         B     SPCTL00             GET NEXT CLIENT                              
         EJECT                                                                  
SPCTL70  CLC   KEYSAVE(8),KEY                                                   
         BNE   SPCTL80                                                          
         SPACE                                                                  
         CLI   QMED,C'C'           CANADIAN COMBINED                            
         BE    SPCTL74                                                          
         SPACE                                                                  
         MVC   DUB(1),KEY          IS THIS A 'COMBINED' BUY?                    
         NI    DUB,X'0F'                                                        
         CLI   DUB,8                                                            
         BNE   SPCTL80                                                          
         SPACE                                                                  
SPCTL74  SR    R2,R2                                                            
         ICM   R2,7,KEY+6                                                       
         SRL   R2,8                GET RID OF 8 BITS OF NETWORK                 
         SR    R3,R3                                                            
         ICM   R3,7,SVMKTSTA+2                                                  
         SRL   R3,8                                                             
         CR    R2,R3               IS THIS THE SAME STATION?                    
         BE    SPCTL60              YES.                                        
         SPACE                                                                  
SPCTL80  DS   0H                                                                
         MVC   KEY(13),KEYSAVE                                                  
         SPACE                                                                  
         TM    KEYSAVE,X'08'       DOING SPECIAL MEDIA FOR MEL                  
         BO    SPCTL00              YES                                         
         SPACE                                                                  
         CLI   KEYSAVE+3,X'FF'     END OF PRODUCTS FOR THIS CLIENT?             
         BE    SPCTL54              YES                                         
         SPACE                                                                  
         ZIC   RE,KEYSAVE+3         NO, TRY NEXT PRODUCT NUMBER                 
         LA    RE,1(RE)                                                         
         STC   RE,KEY+3                                                         
         CLI   QSTA,C'0'           THIS A CABLE HEAD STATION                    
         BL    SPCTL26              YES                                         
         B     SPCTL30                                                          
         EJECT                                                                  
         DS    0D                                                               
SVMKTSTA DS    CL5                                                              
         SPACE                                                                  
         DS    0D                                                               
PATCH1   DS    CL16                                                             
PATCH2   DS    CL16                                                             
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPSC03 12/18/98'                                      
         END                                                                    
