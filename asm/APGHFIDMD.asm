*          DATA SET APGHFIDMD  AT LEVEL 045 AS OF 03/13/01                      
*PHASE ACHFDMDA                                                                 
         TITLE 'APG HOOK FOR DOREMUS PROFIT AND LOSS REPORT'                    
*--------------------------------------------------------------------*          
*     NOTE - FORMAT H USES THIS HOOK ALSO                                       
*--------------------------------------------------------------------*          
ACHFDMD  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
         USING SRECD,R5                                                         
         L     R5,HOOKAREC                                                      
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         CLI   HOOKNUM,1           HOOKS BEFORE SORT                            
         BE    PUTSORT                                                          
         CLI   HOOKNUM,2           HOOKS AFTER SORT                             
         BE    BLDOFTAB                                                         
         DC    H'0'                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ELIMINATE RECORDS BASED ON OFFICE GROUPING                   *         
*---------------------------------------------------------------------*         
PUTSORT  EQU   *                                                                
         LA    R3,GRPNTR                                                        
         CLC   QSELECT(2),=CL2' '                                               
         BE    XIT                                                              
PUTSRT10 CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         CLC   QSELECT(2),0(R3)                                                 
         BE    PUTSRT20                                                         
         LA    R3,8(R3)            BUMP UP TO NEXT ENTRY                        
         B     PUTSRT10                                                         
*                                                                               
PUTSRT20 L     R2,4(R3)            LOAD ADDRESS POINTING TO OFFICE LIST         
         A     R2,HKRELO                                                        
         LA    R2,36(R2)           BUMP PAST NAME                               
*                                                                               
PUTSRT22 CLI   0(R2),0             END OF LIST?                                 
         BE    XIT_NO                                                           
         CLC   QUNIT(2),=C'1C'                                                  
         BNE   PUTSRT25                                                         
         CLC   ACTACC+3(1),0(R2)                                                
         BE    XIT                 FOUND SO OK                                  
         B     PUTSRT50                                                         
*                                                                               
PUTSRT25 CLC   QUNIT(2),=C'SE'                                                  
         BNE   PUTSRT30                                                         
         CLC   ACTACC+9(1),0(R2)                                                
         BE    XIT                 FOUND SO OK                                  
         B     PUTSRT50                                                         
*                                                                               
PUTSRT30 CLC   QUNIT(2),=C'12'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CURRFLT(1),0(R2)    F1=OFFICE                                    
         BE    XIT                 FOUND SO OK                                  
*                                                                               
PUTSRT50 LA    R2,2(R2)            BUMP PAST COMMA TO NEXT CHAR                 
         B     PUTSRT22                                                         
XIT      SR    RE,RE                                                            
XIT_NO   LTR   RE,RE                                                            
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
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
         ST    R2,AIOWRK                                                        
         MVC   ACKEYD(42),SPACES                                                
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=CL2'2D'   GET 2D OFFICES                          
BLDOF12  MVI   ACKEYACC+4,X'FF'                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',AIOWRK,AIOWRK,0              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOWRK                                                        
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
         L     R2,AIOWRK                                                        
         B     BLDOF12                                                          
         EJECT                                                                  
*                                                                               
GETOFFNM SR    R1,R1                                                            
         ICM   R1,1,NOFF                                                        
         BZ    XIT                                                              
         ICM   R3,15,OFFPT                                                      
         BZ    GETOFF08                                                         
         CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         L     R3,=A(OFFTAB)                                                    
         A     R3,HKRELO                                                        
GETOFF05 CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         LA    R3,37(R3)           BUMP UP IN TABLE                             
         BCT   R1,GETOFF05                                                      
GETOFF08 MVC   SRNAM1,=CL36'UNKNOWN OFFICE'                                     
         B     XIT                                                              
*                                                                               
GETOFF10 MVC   SRNAM1,1(R3)        RESTORE 1C OFFICE NAME                       
         ST    R3,OFFPT                                                         
         B     XIT                                                              
         EJECT                                                                  
ONEXONLY DC    AL1(NO)                                                          
NOFF     DS    CL1                                                              
MAXOFF   EQU   36                  MAX NUMBER OF OFFICES IN TABLE               
CUROFF   DS    CL1                                                              
HKRELO   DS    A                                                                
OFFPT    DS    A                                                                
AIOWRK   DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GRPNTR   DS    0A                  GROUP CODE POINTERS                          
         DC    C'CH',A(GRPCH)                                                   
         DC    C'CS',A(GRPCS)                                                   
         DC    C'DC',A(GRPDC)                                                   
         DC    C'DD',A(GRPDD)                                                   
         DC    C'DO',A(GRPDO)                                                   
         DC    C'DR',A(GRPDR)                                                   
         DC    C'FA',A(GRPFA)                                                   
         DC    C'GE',A(GRPGE)                                                   
         DC    C'IN',A(GRPIN)                                                   
         DC    C'LC',A(GRPLC)                                                   
         DC    C'MA',A(GRPMA)                                                   
         DC    C'MN',A(GRPMN)                                                   
         DC    C'ME',A(GRPME)                                                   
         DC    X'FF'                                                            
*                                                                               
*              GROUP NAMES / OFFICE LIST                                        
*                                                                               
GRPCH    DC    CL36'CONSUMER HEALTHWORKS'                                       
         DC    C'4',X'0000'                                                     
*                                                                               
GRPCS    DC    CL36'OVERHEAD OFFICES'                                           
         DC    C'A,I,K,Z',X'0000'                                               
*                                                                               
GRPDC    DC    CL36'DOMESTIC, NO FINANCIAL PRINTING'                            
         DC    C'A,I,J,K,N,R,T,U,W,Z,1,3,5,6,9',X'0000'                         
*                                                                               
GRPDD    DC    CL36'DOMESTIC OFFICES'                                           
         DC    C'A,I,J,K,N,O,R,T,U,W,Z,1,3,5,6,9',X'0000'                       
*                                                                               
GRPDO    DC    CL36'REGIONAL OFFICES'                                           
         DC    C'J,R,3,6',X'0000'                                               
*                                                                               
GRPDR    DC    CL36'DOMESTIC WITHOUT SAN FRAN'                                  
         DC    C'A,I,J,K,N,O,T,U,W,Z,1,3,5,6,9',X'0000'                         
*                                                                               
GRPGE    DC    CL36'DOREMUS GENERAL'                                            
         DC    C'N,T,W,9',X'0000'                                               
*                                                                               
GRPFA    DC    CL36'FINANCIAL OFFICES'                                          
         DC    C'O,U,1,5',X'0000'                                               
*                                                                               
GRPIN    DC    CL36'INTERNATIONAL OFFICES'                                      
         DC    C'V,X,7,8',X'0000'                                               
*                                                                               
GRPLC    DC    CL36'MERKLEY OFFICES'                                            
         DC    C'B,C,D,E,F,G,H,M,P,Q,S,Y,2,4',X'0000'                           
*                                                                               
GRPMA    DC    CL36'MERKLEY NEWMAN HARTY - ATLANTA'                             
         DC    C'E',X'0000'                                                     
*                                                                               
GRPMN    DC    CL36'MERKLEY NEWMAN HARTY - NEW YORK'                            
         DC    C'B,C,D,F,G,H,M,P,Q,S,Y,2',X'0000'                               
*                                                                               
GRPME    DC    CL36'MERCEDES'                                                   
         DC    C'M',X'0000'                                                     
*                                                                               
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
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045APGHFIDMD 03/13/01'                                      
         END                                                                    
