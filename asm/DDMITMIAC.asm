*          DATA SET DDMITMIAC  AT LEVEL 001 AS OF 03/07/12                      
*PROCESS USING(WARN(15))                                                        
*PHASE MITMIACA                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'MITM ACC INITIALIZATION MODULE'                                 
         PRINT NOGEN                                                            
MITMIACC CSECT                                                                  
         NBASE 0,*MITMIAC,=V(REGSAVE)                                           
*                                                                               
         SAM31                                                                  
*                                                                               
         USING ATTINITD,R1                                                      
         MVC   VDATAMGR,ATTINIT_VDATAMGR                                        
         L     R3,ATTINIT_ATTBLK   THIS SUBTASK'S CONTROL AREA                  
         DROP  R1                                                               
*                                                                               
         USING ATTBLKD,R3                                                       
         L     R4,ATTBPTR          GET ADDRESS OF SUBTASK BLOCK                 
         USING ATTD,R4                                                          
         DROP  R3                                                               
*                                                                               
* BUILD ACC COMPANY RECORD INFORMATION                                          
*                                                                               
         L     R7,ATT31STO                                                      
         L     R7,ATT31_ACC_COMPANY_TABLE-ATT31_BLKD(,R7)                       
         USING CPYTABHD,R7                                                      
*                                                                               
         LA    RF,CPYTABH_INDEX    CLEAR COMPANY INDEX ARRAY                    
         LHI   R0,256              MAXIMUM NUMBER OF COMPANIES                  
         XC    0(L'CPYTABH_INDEX,RF),0(RF)                                      
         LA    RF,L'CPYTABH_INDEX(,RF)                                          
         BRCT  R0,*-10                                                          
*                                                                               
         LA    R7,CPYTLEN          STORE COMPANY DATA HERE                      
         USING CPYTLEN,R7                                                       
         XC    CPYTLEN(CPYTLNQ),CPYTLEN  CLEAR FIRST ENTRY                      
*                                                                               
         USING CPYRECD,R5                                                       
         LA    R5,IOKEY                                                         
         MVI   CPYKEY,C' '         PREFILL KEY WITH BLANKS                      
         MVC   CPYKEY+1(L'CPYKEY-1),CPYKEY                                      
         MVC   CPYKEY(2),=X'3FFF'  Start the search for companies               
*                                                                               
MAIN100  GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCDIR',CPYRECD,CPYRECD          
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CPYKCPY,X'FE'       End of file                                  
         JNL   MAINX                                                            
         CLI   CPYKCPY+1,X'40'     Company record ?                             
         JE    *+6                 Yes                                          
         DC    H'0'                Should always get next company               
*                                                                               
         MVC   SVCPYCOD,CPYKCPY    SAVE COMPANY CODE                            
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=C'ACCMST',CPYKDA,IO,WORK           
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IO                                                            
         LR    R6,R5                                                            
         USING CPYELD,R6                                                        
         MVI   ELCODE,CPYELQ       X'10' (company element)                      
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                MUST BE PRESENT                              
*                                                                               
         MVC   CPYTLEN,=AL2(CPYTLNQ)  SET SOFT TABLE ENTRY LENGTH               
         MVC   CPYCODE,CPYKCPY     COMPANY CODE                                 
         MVC   CPYAALPH,CPYALPHA   AGENCY ALPHA                                 
         MVI   CPYFLAGS,0                                                       
         DROP  R6                                                               
*                                                                               
         LR    R6,R5               Point to start of company record             
         USING CPXELD,R6                                                        
         MVI   ELCODE,CPXELQ       X'39' (company extra element)                
         BAS   RE,GETEL                                                         
         JNE   MAIN105                                                          
*                                                                               
         TM    CPXSTAT8,X'02'      Company uses "Resource Management"?          
         JNO   MAIN105                                                          
         OI    CPYFLAGS,CPYFLAGS_RESOURCE_MANAGEMENT  YES                       
*                                                                               
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
* READ LEDGER RECORDS                                                           
*                                                                               
MAIN105  DS    0H                                                               
         LA    R5,LEDGERS          HARD-CODED LEDGERS TO LOOK FOR               
         LA    R8,CPYLEDGR         LEDGER STRUCTURE ARRAY                       
LEDG     USING CPYUNITC,R8                                                      
*                                                                               
         USING LDGRECD,R6                                                       
MAIN110  DS    0H                                                               
         LA    R6,IOKEY                                                         
         MVI   LDGKEY,C' '         PREFILL KEY WITH BLANKS                      
         MVC   LDGKEY+1(L'LDGKEY-1),LDGKEY                                      
         MVC   LDGKCPY,SVCPYCOD    COMPANY CODE                                 
         MVC   LDGKUNT,0(R5)                                                    
         MVC   LDGKLDG,1(R5)                                                    
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCDIR',LDGRECD,LDGRECD          
         CLI   8(R1),0                                                          
         JNE   MAIN120             NOT FOUND                                    
*                                                                               
         MVC   LEDG.CPYUNITC,LDGKUNT    UNIT                                    
         MVC   LEDG.CPYLEDGC,LDGKLDG    LEDGER                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=C'ACCMST',LDGKDA,IO,WORK           
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         LA    R6,IO                                                            
         USING ACLELD,R6                                                        
         MVI   ELCODE,ACLELQ       X'16' (ledger structure element)             
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                MUST BE PRESENT                              
*                                                                               
         LLC   RF,ACLLN                                                         
         SHI   RF,ACLLN1Q          RF = level portion only  / 16                
         SRL   RF,4                RF = max # of levels                         
         LA    R2,ACLVALS                                                       
         LA    RE,LEDG.CPYLEDST    LEDGER STRUCTURE                             
                                                                                
ACL      USING ACLVALS,R2                                                       
         MVC   0(1,RE),ACL.ACLVLEN level length                                 
         LA    R2,L'ACLVALS(,R2)                                                
         LA    RE,L'ACLVLEN(,RE)                                                
         BRCT  RF,*-14                                                          
         DROP  ACL                                                              
         DROP  R6                                                               
*                                                                               
MAIN120  DS    0H                                                               
         LA    R8,CPYLEDNQ(,R8)    BUMP TO NEXT LEDGER STRUCTURE                
         LA    R5,L'LEDGERS(,R5)                                                
         CLI   0(R5),X'FF'                                                      
         BNE   MAIN110                                                          
         DROP  LEDG                                                             
*                                                                               
         L     R2,ATT31STO                                                      
         USING ATT31_BLKD,R2                                                    
         L     R2,ATT31_ACC_COMPANY_TABLE                                       
         LA    R2,CPYTABH_INDEX-CPYTABHD(,R2)                                   
         LLC   R1,CPYCODE          HEX COMPANY CODE                             
         MHI   R1,4                INDEX INTO ADCON TABLE                       
         AR    R2,R1               R2 = A(THIS ADCON)                           
         ST    R7,0(,R2)           SAVE A(THIS COMPANY'S DATA)                  
         DROP  R2                                                               
*                                                                               
         LA    R7,CPYTLNQ(,R7)     BUMP TO NEXT AVAILABLE TABLE SLOT            
         XC    CPYTLEN(CPYTLNQ),CPYTLEN  CLEAR THE SLOT                         
*                                                                               
MAIN130  LA    R5,IOKEY                                                         
         USING CPYRECD,R5                                                       
         MVI   CPYKCPY,C' '        PREFILL KEY TO BLANKS                        
         MVC   CPYKCPY+1(L'CPYKCPY-1),CPYKCPY                                   
         MVC   CPYKCPY,SVCPYCOD    RESTORE COMPANY CODE                         
         MVI   CPYKCPY+1,X'FF'                                                  
         J     MAIN100                                                          
         DROP  R4                                                               
         DROP  R5                                                               
         DROP  R7                                                               
*                                                                               
MAINX    DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,56,ELCODE                                                     
*                                                                               
WORK     DS    XL132                                                            
DMCB     DS    6A                                                               
VDATAMGR DS    V                                                                
ELCODE   DS    X                                                                
*                                                                               
SVCPYCOD DS    XL(L'CPYKCPY)       SAVED COMPANY CODE                           
IOKEY    DS    XL(ACTRFST-ACTRECD)                                              
*                                                                               
LEDGERS  DC    C'SJ'                                                            
         DC    C'1R'                                                            
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
         DC    C'IOIOIOIO'                                                      
IO       DS    XL(4*1024)                                                       
         EJECT                                                                  
       ++INCLUDE DDMITMATTD                                                     
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDMITMIAC 03/07/12'                                      
         END                                                                    
