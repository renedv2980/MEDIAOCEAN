*          DATA SET NENAV20    AT LEVEL 020 AS OF 07/27/16                      
*PHASE T31820B,+0                                                               
T31820   TITLE '-   NET STEWARD GENERAL ROUTINES'                               
T31820   CSECT                                                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
         USING *,RF                                                             
         DS    15000X                                                           
         ORG   T31820                                                           
NV20     NTR1                                                                   
         LA    RB,NV20                                                          
         DROP  RF                                                               
         USING T31820,RB,R7,R8,RA                                               
         B     *+12                                                             
         DC    CL8'**NAV20*'                                                    
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         LA    RA,2048(R8)                                                      
         LA    RA,2048(RA)                                                      
         LA    R3,RELOC                                                         
         S     R3,RELOC                                                         
         ST    R3,RELO                                                          
         USING WORKD,RC                                                         
         L     R9,ATWA                                                          
         USING TWAD,R9                                                          
         SRL   RF,24               GET ROUTINE NUMBER                           
         SLL   RF,2                MULTIPLY BY 4                                
         B     VBRANCH(RF)                                                      
*                                                                               
EXIT     XIT1                                                                   
         SPACE 1                                                                
RELOC    DC    A(*)                                                             
         EJECT                                                                  
*              BRANCH TABLE                                                     
*                                                                               
         SPACE 1                                                                
VBRANCH  B     VSPARE              0                                            
         B     VMED                1                                            
         B     VCLT                2                                            
         B     VSPARE              3                                            
         B     VSPARE              4                                            
         B     VSTA                5                                            
         B     VSPARE              6                                            
         B     VSPARE              7                                            
         B     VSPARE              8                                            
         B     VSPARE              9                                            
*                                                                               
RELO     DS    F                                                                
*                                                                               
VSPARE   DC    H'0'                                                             
         EJECT                                                                  
*==================================================================*            
* LOOK UP MARKET NUMBER AND GET 3 BYTE PACKED STA                  *            
*==================================================================*            
         SPACE 1                                                                
VSTA     DS    0H                                                               
*  READ STATION RECORD GET THE MARKET                                           
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING STAREC,R3                                                        
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),BNET                                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         CLC   KEY(2),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO                                                           
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STH   R1,BMKT                                                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE MEDIA CODE                                            *              
*================================================================*              
         SPACE 1                                                                
VMED     XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,QAGY                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+READ                                        
*                                                                               
         L     R6,AIO1                                                          
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAGYNAM,AGYNAME                                                 
         MVC   SVAGYADR,AGYADDR                                                 
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
*                                                                               
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,=H'24'                                                  
         BAS   RE,GETEL                                                         
         B     VMED4                                                            
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
VMED4    CLI   2(R6),C'N'          MATCH MEDIA CODE                             
         BNE   VMED2                                                            
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   SVMEDNM,4(R6)       MEDIA NAME                                   
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*================================================================*              
* VALIDATE CLIENT                                                *              
*================================================================*              
         SPACE 1                                                                
VCLT     MVC   ERROR,=Y(BADCLT)                                                 
         GOTO1 VCLPACK,DMCB,QCLT,BCLT                                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R6,AIO1                                                          
******   LR    R6,R7                   TO SAVE CLIENT REC IN TWA                
******   AHI   R6,(SVCLTREC-TWAD)                                               
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         USING CLTHDR,R6                                                        
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVCNAME,CNAME       AND CLIENT NAME                              
*                                                                               
         MVC   ERROR,=Y(SECLOCK)                                                
         OC    TWAACCS(2),TWAACCS  TEST ANY SECURITY LIMIT                      
         BZ    EXIT                                                             
         CLI   TWAACCS,C'*'        TEST OFFICE LOCKOUT                          
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'+'        TEST MKT LOCKOUT                             
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'$'        TEST OFFICE LIST                             
         BE    CLT20               YES                                          
*                                                                               
CLT10    CLC   TWAACCS(2),BCLT                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLT20    CLI   TWAACCS,C'$'                                                     
         BE    CLT30                                                            
         CLI   TWAACCS,C'*'                                                     
         BNE   EXIT                                                             
         LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,COFFICE                                                       
         LA    R0,1                                                             
*                                                                               
CLT25    CLC   TWAACCS+1(1),0(R1)                                               
         BE    EXIT                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,CLT25                                                         
         DC    H'0'                                                             
*                                                                               
CLT30    CLI   TWAACCS,C'$'        TEST OFFICE LIST                             
         BNE   EXIT                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCLMT,TWAACCS                                                   
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NENAVWRKA                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020NENAV20   07/27/16'                                      
         END                                                                    
