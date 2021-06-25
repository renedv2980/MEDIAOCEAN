*          DATA SET APGHFIGVD  AT LEVEL 001 AS OF 08/27/93                      
*PHASE ACHFIGVD,+0                                                              
         TITLE 'APG HOOK FOR GAVIN PROFILT AND LOSS REPORT'                     
ACHFIGVD CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
         USING SRECD,R5                                                         
         L     R5,HOOKAREC                                                      
         CLI   HOOKNUM,1           HOOKS AFTER SORT                             
         BNE   XIT                                                              
         EJECT                                                                  
BLDOFTAB CLI   ONEXONLY,NO                                                      
         BNE   GETOFFNM                                                         
         MVI   ONEXONLY,YES                                                     
         L     R3,=A(OFFTAB)       ADDRESS OF OFFICE TABLE                      
         A     R3,HKRELO                                                        
         ST    R3,OFFPT                                                         
         MVI   NOFF,0              NUMBER OF OFFICES                            
         MVI   CUROFF,0                                                         
*                                                                               
         USING ACKEYD,R2                                                        
         L     R2,=A(IO1)                                                       
         A     R2,HKRELO                                                        
         ST    R2,AIO1                                                          
         MVC   ACKEYD(42),SPACES                                                
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=CL2'2D'   GET 2D OFFICES                          
BLDOF12  MVI   ACKEYACC+4,X'FF'                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',AIO1,AIO1,0                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         CLC   ACKEYACC+1(2),=CL2'2D'                                           
         BNE   GETOFFNM            FINISHED                                     
         MVI   0(R3),0                                                          
         MVC   1(36,R3),SPACES                                                  
         MVC   CUROFF,ACKEYACC+3                                                
         MVC   0(1,R3),CUROFF                                                   
         AH    R2,DATADISP                                                      
*                                                                               
BLDOF15  CLI   0(R2),0                                                          
         BE    BLDOF90                                                          
         CLI   0(R2),X'20'                                                      
         BE    BLDOF20                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         B     BLDOF15                                                          
*                                                                               
BLDOF20  CLI   NOFF,MAXOFF         END OF TABLE?                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NOFF             BUMP UP COUNT                                
         AH    R1,=H'01'                                                        
         STC   R1,NOFF                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'03'                                                        
         EXMVC R1,1(R3),2(R2)                                                   
BLDOF90  LA    R3,37(R3)           BUMP UP IN TABLE                             
         L     R2,AIO1                                                          
         B     BLDOF12                                                          
         EJECT                                                                  
GETOFFNM L     R3,OFFPT                                                         
         SR    R1,R1                                                            
         ICM   R1,1,NOFF                                                        
         BZ    XIT                                                              
         CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         L     R3,=A(OFFTAB)                                                    
         A     R3,HKRELO                                                        
GETOFF05 CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         LA    R3,37(R3)           BUMP UP IN TABLE                             
         BCT   R1,GETOFF05                                                      
         DC    H'0'                OFFICE NOT FOUND                             
*                                                                               
GETOFF10 MVC   SRNAM1,1(R3)        RESTORE 1C OFFICE NAME                       
         ST    R3,OFFPT                                                         
         B     XIT                                                              
         SPACE 4                                                                
XIT      SR    RE,RE                                                            
         XMOD1 1                                                                
         EJECT                                                                  
ONEXONLY DC    AL1(NO)                                                          
NOFF     DS    CL1                                                              
MAXOFF   EQU   36                  MAX NUMBER OF OFFICES IN TABLE               
CUROFF   DS    CL1                                                              
HKRELO   DS    A                                                                
OFFPT    DS    A                                                                
AIO1     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------*                                            
*        OFFICE CODE(1)            *                                            
*        OFFICE NAME(36)           *                                            
*----------------------------------*                                            
OFFTAB   DS    (MAXOFF)CL37                                                     
IO1      DS    CL2001                                                           
         EJECT                                                                  
*--------------------------*                                                    
*        SORT RECORD       *                                                    
*--------------------------*                                                    
SRECD    DSECT                                                                  
SRREC    DS    0C                                                               
SRROW1   DS    CL2                 REPORT NUMBER/COPY                           
SRACC1   DS    CL1                 ROW 1 OFFICE CODE                            
         DS    CL13                                                             
SRROW2   DS    CL2                 REPORT NUMBER/COPY                           
SRACC2   DS    CL1                 ACCOUNT LEVEL 1 (SUPERLEDGER)                
         DS    CL13                                                             
SRROW3   DS    CL2                 REPORT NUMBER/COPY                           
SRACC3   DS    CL2                 ACCOUNT LEVEL 2 (SUPERLEDGER)                
         DS    CL12                                                             
SRROW4   DS    CL2                 REPORT NUMBER/COPY                           
SRACC4   DS    CL3                 ACCOUNT LEVEL 3 (SUPERLEDGER)                
         DS    CL11                                                             
         DS    CL2                 REPORT NUMBER/COPY                           
SRBINZ   DS    XL2                 BINARY ZERO                                  
SRNAM1   DS    CL36                OFFICE NAME                                  
SRNAM2   DS    CL36                ROW 2 NAME                                   
SRNAM3   DS    CL36                ROW 3 NAME                                   
SRNAM4   DS    CL36                ROW 4 NAME                                   
SRAMT    DS    6PL8                BUCKETS                                      
SRLNQ    EQU   *-SRREC                                                          
         EJECT                                                                  
         EJECT                                                                  
*        IHAASCB                                                                
*        IHASDWA                                                                
*        ACAPGWORKD                                                             
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
         PRINT OFF                                                              
       ++INCLUDE ACAPGWORKD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001APGHFIGVD 08/27/93'                                      
         END                                                                    
