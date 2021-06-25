*          DATA SET ACREP7402  AT LEVEL 011 AS OF 05/01/02                      
*PHASE AC7402A                                                                  
*INCLUDE CONFUSE                                                                
         TITLE 'AC74 - USER PROFILE LISTING'                                    
AC7402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC7402,RR=R5                                                 
         L     RA,0(R1)            RA=A(GLOBAL WORK)                            
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND          RC=A(TEMP WORK)                              
         USING WORKD,RC                                                         
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   PR2                                                              
         L     RE,=V(CONFUSE)                                                   
         A     RE,RELO                                                          
         ST    RE,VCONFUSE                                                      
         B     EXIT                                                             
*                                                                               
PR2      CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVC   PAGE,=H'1'          SET-UP FOR NEW REQUEST                       
         MVI   RCSUBPRG,0                                                       
         MVI   CONSW,C'F'                                                       
         XC    CONKEY,CONKEY                                                    
         MVI   CONKEY,C'U'         BUILD CONFUSE KEY                            
         MVI   CONKEY+1,C'A'                                                    
         MVC   CONKEY+2(2),ALPHAID                                              
         MVC   CONKEY+4(3),QOPT1                                                
         EJECT                                                                  
*                                                                               
PR4      GOTO1 VCONFUSE,DMCB,(CONSW,CONKEY),CONIO,DATAMGR,UIDNAME               
         MVI   CONSW,C'S'          SEQ FOR NEXT CALL                            
         CLI   8(R1),0                                                          
         BE    PR6                                                              
         TM    8(R1),X'80'         E-O-F                                        
         BO    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
PR6      LA    R2,CONIO                                                         
         USING CTUREC,R2                                                        
         OC    CTUKAGY(6),CTUKAGY  AGENCY RECORD                                
         BNZ   PR18                                                             
         MVI   FORCEHED,C'Y'       NO - MUST BE DEFINITION                      
         MVI   RCSUBPRG,0                                                       
         MVC   THISREP,SPACES                                                   
         XC    SPECTAB,SPECTAB                                                  
         LA    R3,CTUDATA                                                       
*                                                                               
PR8      CLI   0(R3),0                                                          
         BE    PR16                                                             
         CLI   0(R3),X'02'         DESCRIPTION ELEMENT                          
         BE    PR12                                                             
         CLI   0(R3),X'70'         FIELD DEFINITION ELEMENT                     
         BE    PR14                                                             
PR10     ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PR8                                                              
*                                  HANDLE DESCRIPTION ELEMENT                   
PR12     MVC   THISREP(2),=C'AC'                                                
         MVC   THISREP+2(2),CTUKPROG+1                                          
         CLI   CTUKPROG,0                                                       
         BE    *+10                                                             
         MVC   THISREP+2(3),CTUKPROG                                            
         ZIC   R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     PR10                                                             
         MVC   THISREP+6(0),2(R3)                                               
         EJECT                                                                  
*                                  HANDLE DEFINITION ELEMENT                    
         USING CTFDD,R3                                                         
PR14     EDIT  (B1,CTFDNUM),(2,P+3),FILL=0                                      
         ZIC   R1,CTFDLEN                                                       
         SH    R1,=H'27'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+9(0),CTFDDESC                                                  
         MVC   P+41(L'CTFDLIST),CTFDLIST                                        
         TM    CTFDOTHR,X'80'                                                   
         BZ    *+8                                                              
         MVI   P+73,C'*'                                                        
*                                  ADD ENTRY TO SPECTAB FOR FIELD               
         ZIC   R1,CTFDNUM                                                       
         LA    R1,SPECTAB-1(R1)                                                 
         MVC   0(1,R1),CTFDOTHR                                                 
         CLI   CTFDTYPE,C'C'       SET FIELD TYPE                               
         BNE   *+8                                                              
         OI    0(R1),X'08'                                                      
         CLI   CTFDTYPE,C'N'                                                    
         BNE   *+8                                                              
         OI    0(R1),X'04'                                                      
         CLI   CTFDTYPE,C'X'                                                    
         BNE   *+8                                                              
         OI    0(R1),X'02'                                                      
         ZIC   R2,CTFDNUM                                                       
         LA    R5,CTFDDEF                                                       
         LA    R4,P+65                                                          
         BAS   RE,GETVALUE         EDIT DEFAULT VALUE                           
         MVI   SPACING,2                                                        
         BAS   R9,REPORT                                                        
         B     PR10                                                             
*                                  SET-UP FOR AGENCY RECORDS                    
PR16     MVI   RCSUBPRG,1                                                       
         MVI   FORCEMID,C'Y'                                                    
         B     PR4                                                              
         EJECT                                                                  
*                                  HANDLE AGENCY RECORDS                        
*R18     MVC   P+3(2),CTUKAGY+1    FORMAT KEY                                   
*                                  FORMAT KEY                                   
PR18     MVC   P+3(3),=C'ALL'      CHECK FOR USERID LEVEL PROFILE               
         CLC   CTUKAGY(2),=XL2'4040'                                            
         BNL   *+10                                                             
         MVC   P+3(10),UIDNAME                                                  
*                                                                               
         MVC   P+15(2),CTUKUNT     FORMAT REST OF KEY                           
*        OC    CTUKAGY+1(2),CTUKAGY+1                                           
         OC    CTUKUNT(2),CTUKUNT                                               
         BNZ   *+10                                                             
         MVC   P+15(3),=C'ALL'                                                  
*        MVC   P+23(3),CTUKCLT                                                  
         MVC   P+23(3),CTUKACT                                                  
*        OC    CTUKCLT,CTUKCLT                                                  
         OC    CTUKACT,CTUKACT                                                  
         BNZ   *+10                                                             
         MVC   P+23(3),=C'ALL'                                                  
         LA    R3,CTUDATA                                                       
         SR    R1,R1                                                            
PR20     CLI   0(R3),0             FIND VALUE ELEMENT                           
         BE    PR4                                                              
         CLI   0(R3),X'72'                                                      
         BE    *+14                                                             
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PR20                                                             
         USING CTPVD,R3                                                         
         LA    R2,16                                                            
         LA    R5,CTPVALUE+L'CTPVALUE-1                                         
         LA    R4,P+111                                                         
         BAS   RE,GETVALUE         EDIT OVERRIDE VALUES                         
         SH    R4,=H'5'                                                         
         BCTR  R5,0                                                             
         BCT   R2,*-10                                                          
         BAS   R9,REPORT                                                        
         B     PR4                                                              
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*              EDIT A FIELD VALUE                                               
*              R2=FIELD NUMBER,R5=A(INPUT VALUE),R4=A(OUTPUT VALUE)             
*                                                                               
*                                                                               
GETVALUE NTR1                                                                   
         LA    R2,SPECTAB-1(R2)                                                 
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLI   0(R3),0                                                          
         BNE   *+12                                                             
         TM    0(R2),X'40'                                                      
         BO    EXIT                                                             
         MVC   0(1,R4),0(R5)                                                    
         TM    0(R2),X'08'                                                      
         BO    EXIT                                                             
         EDIT  (B1,0(R5)),(3,0(R4)),ALIGN=LEFT                                  
         OI    0(R4),X'F0'                                                      
         TM    0(R2),X'04'                                                      
         BO    EXIT                                                             
         GOTO1 HEXOUT,DMCB,0(R5),0(R4),1,=C'TOG'                                
         MVI   2(R4),C' '                                                       
         B     EXIT                                                             
*                                                                               
*              PRINT A LINE                                                     
*                                                                               
REPORT   MVC   HEAD5+12(L'THISREP),THISREP                                      
         GOTO1 ACREPORT                                                         
         MVI   SPACING,1                                                        
         BR    R9                                                               
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    F                                                                
VCONFUSE DS    V                                                                
THISREP  DS    CL40                                                             
CONKEY   DS    CL7                                                              
CONSW    DS    C                                                                
UIDNAME  DS    CL10                                                             
SPECTAB  DS    CL16                                                             
CONIO    DS    1000C                                                            
*                                                                               
* ACREPWORKD/ACGENMODES/CTGENFILE                                               
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREP7402 05/01/02'                                      
         END                                                                    
