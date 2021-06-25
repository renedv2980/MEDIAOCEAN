*          DATA SET APGHFIMKA  AT LEVEL 058 AS OF 05/07/02                      
*PHASE ACHFMKAA                                                                 
         TITLE 'APG HOOK FOR MERKLEY PROFIT AND LOSS REPORT'                    
ACHFMKA  CSECT                                                                  
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
         CLC   ACTACC+3(2),0(R2)                                                
         BE    XIT                 FOUND SO OK                                  
         B     PUTSRT50                                                         
*                                                                               
PUTSRT25 CLC   QUNIT(2),=C'SE'                                                  
         BNE   PUTSRT30                                                         
         CLC   CURROFC(2),0(R2)                                                 
         BE    XIT                 FOUND SO OK                                  
         B     PUTSRT50                                                         
*                                                                               
PUTSRT30 CLC   QUNIT(2),=C'12'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CURROFC(2),0(R2)                                                 
         BE    XIT                 FOUND SO OK                                  
*                                                                               
PUTSRT50 LA    R2,3(R2)            BUMP PAST COMMA TO NEXT CHAR                 
         B     PUTSRT22                                                         
XIT      SR    RE,RE                                                            
XIT_NO   LTR   RE,RE                                                            
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
BLDOFTAB CLI   ONEXONLY,NO                                                      
         BNE   GETOFFNM                                                         
         MVI   ONEXONLY,YES                                                     
         MVI   NOFF,0              NUMBER OF OFFICES                            
                                                                                
         L     R3,=A(OFFTAB)       ADDRESS OF OFFICE TABLE                      
         A     R3,HKRELO                                                        
*        ST    R3,OFFPT                                                         
                                                                                
         USING OFFRECD,R2                                                       
         L     R2,=A(IO1)                                                       
         A     R2,HKRELO                                                        
         ST    R2,AIOWRK                                                        
         MVC   COMMAND,DMRDHI                                                   
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,RCCOMPFL                                                 
BLDOF12  EQU   *                                                                
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',AIOWRK,AIOWRK,0                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   COMMAND,DMRSEQ                                                   
         L     R2,AIOWRK                                                        
         CLC   OFFKCPY,RCCOMPFL                                                 
         BH    GETOFFNM                                                         
         TM    OFFKSTAT,OFFSLIST                                                
         BO    BLDOF12                                                          
                                                                                
         SR    R1,R1                                                            
         IC    R1,NOFF             BUMP UP COUNT                                
         AH    R1,=H'01'                                                        
         STC   R1,NOFF                                                          
         CLI   NOFF,MAXOFF         END OF TABLE?                                
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   0(2,R3),OFFKOFF                                                  
         MVC   2(36,R3),SPACES                                                  
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
BLDOF20  SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'03'                                                        
         EXMVC R1,2(R3),2(R2)                                                   
BLDOF90  LA    R3,38(R3)           BUMP UP IN TABLE                             
         L     R2,AIOWRK                                                        
         B     BLDOF12                                                          
         EJECT                                                                  
                                                                                
GETOFFNM SR    R1,R1                                                            
         ICM   R1,1,NOFF                                                        
         BZ    XIT                                                              
*        ICM   R3,15,OFFPT                                                      
*        BZ    GETOFF08                                                         
*        CLC   SRACC1,0(R3)                                                     
*        BE    GETOFF10                                                         
         L     R3,=A(OFFTAB)                                                    
         A     R3,HKRELO                                                        
GETOFF05 CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         LA    R3,38(R3)           BUMP UP IN TABLE                             
         BCT   R1,GETOFF05                                                      
GETOFF08 MVC   SRNAM1,=CL36'UNKNOWN OFFICE'                                     
         B     XIT                                                              
*                                                                               
GETOFF10 MVC   SRNAM1,2(R3)        RESTORE 1C OFFICE NAME                       
*        ST    R3,OFFPT                                                         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LITERAL POOL                                                           
*------------------------------------------------------------------*            
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LOCAL WORKING STORAGE                                                  
*------------------------------------------------------------------*            
ONEXONLY DC    AL1(NO)                                                          
NOFF     DS    CL1                                                              
MAXOFF   EQU   99                  MAX NUMBER OF OFFICES IN TABLE               
HKRELO   DS    A                                                                
OFFPT    DS    A                                                                
AIOWRK   DS    A                                                                
*------------------------------------------------------------------*            
*        OFFICE TABLES                                                          
*------------------------------------------------------------------*            
GRPNTR   DS    0A                  GROUP CODE POINTERS                          
         DC    C'NY',A(GRPNY)                                                   
         DC    X'FF'                                                            
                                                                                
GRPNY    DC    CL36'MNH AND STUDIO OFFICES'                                     
         DC    C'02,03,04,06,07,08',X'0000'                                     
                                                                                
*----------------------------------*                                            
*        OFFICE CODE(2)            *                                            
*        OFFICE NAME(36)           *                                            
*----------------------------------*                                            
OFFTAB   DS    (MAXOFF)CL38                                                     
COMMAND  DS    CL8                                                              
IO1      DS    CL2001                                                           
                                                                                
*--------------------------*                                                    
*        SORT RECORD       *                                                    
*--------------------------*                                                    
SRECD    DSECT                                                                  
SRREC    DS    0C                                                               
SRROW1   DS    CL2                 REPORT NUMBER/COPY                           
SRACC1   DS    CL2                 ROW 1 OFFICE CODE                            
         DS    CL12                                                             
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
**PAN#1  DC    CL21'058APGHFIMKA 05/07/02'                                      
         END                                                                    
