*          DATA SET PPREP7402  AT LEVEL 013 AS OF 02/02/15                      
*PHASE PP7402A                                                                  
*INCLUDE CONFUSE                                                                
         TITLE 'PP74 - USER PROFILE LISTING'                                    
PP7402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PP7402                                                       
         L     RA,0(R1)            RA=A(GLOBAL WORK)                            
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND          RC=A(TEMP WORK)                              
         USING WORKD,R8                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   PR2                                                              
         RELOC (R3)                                                             
         L     RE,=V(CONFUSE)                                                   
         AR    RE,R3                                                            
         ST    RE,VCONFUSE                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                X        
               =C'NGENDIR NGENFIL X',PBUYREC,0                                  
*                                                                               
         B     EXIT                                                             
*                                                                               
PR2      CLI   MODE,PROCREQ                                                     
         BNE   EXIT                                                             
         MVC   PAGE,=H'1'          SET-UP FOR NEW REQUEST                       
         MVI   RCSUBPRG,0                                                       
         MVI   CONSW,C'F'                                                       
         XC    CONKEY,CONKEY                                                    
         MVI   CONKEY,C'U'         BUILD CONFUSE KEY                            
         MVI   CONKEY+1,C'P'                                                    
         MVC   CONKEY+2(2),QAGENCY                                              
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
         MVC   THISREP(2),=C'PP'                                                
         MVC   THISREP+2(2),CTUKPROG+1                                          
         CLI   CTUKPROG,0                                                       
         BE    *+10                                                             
         MVC   THISREP+2(3),CTUKPROG                                            
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
PR12     ZIC   R1,1(R3)                                                         
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
         BAS   R9,PRINTIT                                                       
         B     PR10                                                             
*                                  SET-UP FOR AGENCY RECORDS                    
PR16     MVI   RCSUBPRG,1                                                       
         MVI   FORCEMID,C'Y'                                                    
         B     PR4                                                              
         EJECT                                                                  
*                                  HANDLE AGENCY RECORDS                        
PR18     CLI   QPUB,C' '           HAVE CLT/OFF FILTER?                         
         BNH   *+14                NO, DISPLAY EVERYTHING                       
         CLC   CTUKCLT,QPUB        MATCH FILTER?                                
         BNE   PR4                                                              
*                                                                               
         MVC   P+3(3),=C'ALL'      CHECK FOR USERID LEVEL PROFILE               
         CLC   CTUKAGY,=XL2'4040'                                               
         BNL   *+10                                                             
         MVC   P(10),UIDNAME                                                    
*                                                                               
         MVC   P+14(1),CTUKMED     FORMAT REST OF KEY                           
         OC    CTUKMED,CTUKMED                                                  
         BNZ   *+10                                                             
         MVC   P+13(3),=C'ALL'                                                  
         MVC   P+19(3),CTUKCLT                                                  
         OC    CTUKCLT,CTUKCLT                                                  
         BNZ   *+10                                                             
         MVC   P+19(3),=C'ALL'                                                  
*                                                                               
         CLI   CTUKCLT,C'*'        DO WE HAVE PROFILE FOR AN OFFICE?            
         BNE   PR19                NO                                           
         XC    MYKEY,MYKEY         READ OFFICE RECORD                           
         LA    R6,MYKEY                                                         
         USING MOFRECD,R6                                                       
         MVI   MOFKTYP,MOFKTYPQ    C'O'                                         
         MVI   MOFKSUB,MOFKS1Q     X'01' = ONE BYTE INTERNAL OFFICE             
         MVC   MOFKAGY,QAGENCY     AGENCY                                       
         MVC   MOFK1OF,CTUKCLT+1   INTERNAL OFFICE                              
         MVI   MOFKSYS,X'04'                                                    
         MVC   MYKEYSV,MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',MYKEY,MYKEY,0                 
         CLI   8(R1),0             ANY ERRORS?                                  
         BNE   PR19                YES                                          
         CLC   MOFKEY(32),MYKEYSV                                               
         BNE   PR19                                                             
         MVC   P+20(2),MOFKC2OF                                                 
*                                                                               
PR19     LA    R3,CTUDATA                                                       
         SR    R1,R1                                                            
PR20     CLI   0(R3),0             FIND VALUE ELEMENT                           
         BE    PR4                                                              
         CLI   0(R3),X'01'         ACTIVITY DATE                                
         BE    PR24                                                             
         CLI   0(R3),X'72'         FIELD VALUES                                 
         BE    PR26                                                             
PR22     ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PR20                                                             
*                                                                               
PR24     GOTO1 DATCON,DMCB,(3,2(R3)),(8,P+90)                                   
         B     PR22                                                             
*                                                                               
PR26     DC    0H'0'                                                            
         USING CTPVD,R3                                                         
         LA    R2,16                                                            
         LA    R5,CTPVALUE+L'CTPVALUE-1                                         
         LA    R4,P+87                                                          
         BAS   RE,GETVALUE         EDIT OVERRIDE VALUES                         
         SH    R4,=H'4'                                                         
         BCTR  R5,0                                                             
         BCT   R2,*-10                                                          
         BAS   R9,PRINTIT                                                       
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
PRINTIT  MVC   HEAD4+8(L'THISREP),THISREP                                       
         GOTO1 REPORT                                                           
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
VCONFUSE DS    V                                                                
THISREP  DS    CL40                                                             
CONKEY   DS    CL7                                                              
CONSW    DS    C                                                                
UIDNAME  DS    CL10                                                             
SPECTAB  DS    CL16                                                             
CONIO    DS    1000C                                                            
MYKEY    DS    CL44                                                             
MYKEYSV  DS    CL44                                                             
*                                                                               
* PPWORKD/PPMODEQU/CTGENFILE                                                    
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEGENOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREP7402 02/02/15'                                      
         END                                                                    
