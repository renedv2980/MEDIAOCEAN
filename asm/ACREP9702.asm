*          DATA SET ACREP9702  AT LEVEL 081 AS OF 08/06/19                      
*PHASE AC9702A                                                                  
*INCLUDE ACCEDIT                                                                
*INCLUDE ACJOBMNT                                                               
*INCLUDE ACRAPPER                                                               
*INCLUDE AC1RMNT                                                                
         TITLE 'MODULE TO DELETE ACCOUNTS'                                      
*----------------------------------------------------------------------         
* HISTORY      11/1/94 ADD RAP POINTERS TO PROD VENDORS                         
*              5/25/95 ADD RAP POINTERS TO EXTRA PROD VENDORS                   
*VGUP 07AUG19 078 DSRD-22480 RELINK FOR ACJOBMNT                                
*----------------------------------------------------------------------         
*                                                                               
AC9702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC97**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   DL1                                                              
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT2,C'1'           DEL ACCTS NEVER USED ONLY                   
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
         MVI   RQSW,0                                                           
         L     RE,ADCMPEL                                                       
         TM    CPYSTAT5-CPYELD(RE),CPYSNCST   NEW COST                          
         BNO   *+8                                                              
         OI    RQSW,RQNC                      SET NEW METHOD                    
         TM    CPYSTAT7-CPYELD(RE),CPYSTMSY                                     
         BNO   *+8                                                              
         OI    RQSW,RQTMS                     TMS IN USE                        
*                                                                               
         L     RE,=A(IO)                                                        
         ST    RE,AIO                                                           
*                                                                               
         XC    VRAPPER,VRAPPER                                                  
         L     RE,ADCMPEL                                                       
         TM    CPYSTAT6-CPYELD(RE),CPYSRAPP ACTIVITY POINTERS?                  
         BNO   XIT                                                              
         L     RE,=V(ACRAPPER)                                                  
         ST    RE,VRAPPER                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              CHECK IF ACCOUNT CAN BE DELETED AND REPORT                       
*                                                                               
DL1      CLI   MODE,PROCACC                                                     
         BNE   DL10                                                             
*                                                                               
         TM    RQSW,RQNC+RQTMS     NEW COST OR TMS?                             
         BZ    DL1Z                                                             
         L     R3,ADACC                                                         
         CLC   1(2,R3),=C'1C'                                                   
         BE    XIT                                                              
                                                                                
         USING ACTRECD,R3                                                       
         CLC   1(2,R3),=C'1R'      BUCKETS ONLY SO DON'T ALLOW DELETE           
         BNE   DL1Z                                                             
         MVC   DIRKEY,SPACES                                                    
         LA    R2,DIRKEY                                                        
         USING PERRECD,R2                                                       
         MVI   PERKTYP,PERKTYPQ                                                 
         L     R3,ADACC                                                         
         MVC   PERKCPY,0(R3)                                                    
         LA    R4,ACTKACT                                                       
         ZIC   R5,DSPLPER                                                       
         AR    R4,R5                                                            
         ZIC   R5,LENPER                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PERKCODE(0),0(R4)                                                
                                                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DIRKEY,DMDKEY                         
         CLC   DIRKEY(42),DMDKEY                                                
         BNE   DL1Z                                                             
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,DMDKEYA,AIO,ADWRK                      
                                                                                
*        DON'T USE R4 UNTIL THIS LOOP IS COMPLETE                               
         L     R2,AIO                                                           
         USING LOCELD,R4                                                        
         LA    R4,PERRFST                                                       
DL1C     CLI   0(R4),0                                                          
         BE    DL1Z                                                             
         CLI   LOCEL,LOCELQ                                                     
         BE    DL1E                                                             
DL1D     ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     DL1C                                                             
                                                                                
DL1E     DS    0H                                                               
         L     R3,ADACC                                                         
         LA    R3,ACTKACT                                                       
         ZIC   R5,LVALEN                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),LOCOFF                                                   
         BNE   DL1D                                                             
                                                                                
         CLI   LVBLEN,0                                                         
         BE    DL1D                                                             
         L     R3,ADACC                                                         
         LA    R3,ACTKACT                                                       
         ZIC   R5,LVALEN                                                        
         AR    R3,R5                                                            
         ZIC   R2,LVBLEN                                                        
         SR    R2,R5                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),LOCDEPT                                                  
         BNE   DL1D                                                             
                                                                                
         CLI   LVCLEN,0                                                         
         BE    DL1D                                                             
         L     R3,ADACC                                                         
         LA    R3,ACTKACT                                                       
         ZIC   R5,LVBLEN                                                        
         AR    R3,R5                                                            
         ZIC   R2,LVCLEN                                                        
         SR    R2,R5                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),LOCSUB                                                   
         BNE   DL1D                                                             
                                                                                
* IF I HAVEN'T GOTTEN PAST THIS LOOP BY NOW A PERSON REC WITH A                 
* LOCATION EL WITH OFF/DEPT/SUB EXISTS - THEREFORE CANNOT DELETE                
* THIS 1R RECORD                                                                
         B     XIT                                                              
                                                                                
                                                                                
DL1Z     L     R2,ADACCSTA         CHECK DATES ON ALL RECORDS                   
         USING ACSTATD,R2                                                       
         CLI   QOPT2,C'1'           DEL ACCTS NEVER USED ONLY                   
         BNE   *+18                                                             
         CLC   ACSTBFDT,ACSTLAST    PEEL DATE > OR = LAST TRANS?                
         BL    XIT                                                              
         B     *+14                                                             
         CLC   ACSTBFDT,ACSTLAST   PEEL DATE > LAST TRANSACTION ?               
         BNH   XIT                 NO, CAN'T DELETE IT                          
         GOTO1 DATCON,DMCB,(1,ACSTLAST),(0,WORK)                                
         CLC   WORK(6),QEND                                                     
         BNH   DL2                                                              
         CLC   QEND,SPACES         END DATE IS OPTIONAL                         
         BNE   XIT                                                              
*                                                                               
         USING ACCOMPD,R2                                                       
DL2      L     R2,ADCOMP                                                        
         AH    R2,DATADISP                                                      
         L     R3,ADACC                                                         
         CLC   1(2,R3),ACMPJOB     IS THIS A JOB ?                              
         BNE   DL4                 NO                                           
         BAS   RE,DELETIT          YES, GO TO DELETE OVERLAY                    
         BE    DL6                 NO ERRORS, PRINT IT                          
         B     XIT                                                              
*                                                                               
         USING ACBALD,R2                                                        
DL4      L     R2,ADACCBAL         CHECK BALANCE FOR ALL OTHERS                 
         CP    ACBLFRWD,=P'0'                                                   
         BNE   XIT                                                              
         CP    ACBLDR,=P'0'                                                     
         BNE   XIT                                                              
         CP    ACBLCR,=P'0'                                                     
         BNE   XIT                                                              
*                                                                               
         L     R3,ADACC                                                         
         LA    R2,ACCORFST(R3)                                                  
         USING ASTELD,R2                                                        
DL4A     CLI   0(R2),0                                                          
         BE    DL4C                                                             
         CLI   ASTEL,ASTELQ                                                     
         BE    DL4B                                                             
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DL4A                                                             
DL4B     OC    ASTDRAFT,ASTDRAFT    CHECK FOR DRAFT TRANSACTIONS                
         BNZ   XIT                                                              
*                                                                               
DL4C     CLI   QOPT2,C'1'           DEL ACCTS NEVER USED ONLY                   
         BNE   DL5                                                              
         L     R3,ADACC                                                         
         USING ACTRECD,R3                                                       
         MVC   DIRKEY,SPACES                                                    
         MVC   DIRKEY(L'ACTKCULA),ACTKCULA                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DIRKEY,DMDKEY                         
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DIRKEY,DMDKEY                         
         CLC   DIRKEY(L'ACTKCULA),DMDKEY                                        
         BE    XIT                                                              
*                                                                               
DL5      CLI   RCWRITE,C'N'                                                     
         BE    DL6                                                              
         CLI   QOPT1,C'N'          OPTION NOT TO WRITE                          
         BE    *+8                                                              
         MVI   MODE,WRITACC        SET TO DELETE                                
*                                                                               
DL6      L     R2,ADACC                                                         
         L     R3,ADLDGHIR                                                      
         BAS   RE,PRERAP                                                        
         USING ACKEYD,R2                                                        
         OI    ACSTATUS,X'80'      DELETE RECORD                                
         BAS   RE,POSTRAP                                                       
         GOTO1 =V(ACCEDIT),DMCB,(R2),(R3),P+20,RR=RB                            
         AP    DELCOUNT,=P'1'                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(5,P+82)                                    
         L     R2,ADACCNAM                                                      
         USING ACNAMED,R2                                                       
         SR    R3,R3                                                            
         IC    R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+40(0),ACNMNAME                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
         L     R3,ADACC                                                         
         CLC   1(2,R3),=C'1R'      ONE MORE STEP IF 1R                          
         BNE   XIT                                                              
         CLI   QOPT1,C'N'          UPDATE?                                      
         BE    XIT                 NO                                           
*                                                                               
         GOTO1 =V(AC1RMNT),DMCB,(C'D',(R3)),DATAMGR                             
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
*                                                                               
* ---------------------------------------------------------------------         
* IF THIS IS 1R WE NEED DISPLACEMENT AND LENGTH OF PERSON CODE                  
* ---------------------------------------------------------------------         
DL10     CLI   MODE,LEDGFRST                                                    
         BNE   DL20                                                             
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING LDGRECD,R2                                                       
         L     R2,ADLEDGER                                                      
         CLC   LDGKUNT(2),=C'1R'                                                
         BNE   XIT                                                              
*                                                                               
         XC    LEVLENS,LEVLENS                                                  
         USING ACLELD,R2                                                        
         L     R2,ADLDGHIR                                                      
         LA    R4,ACLVALS                                                       
         LA    R5,LEVLENS                                                       
*                                                                               
DL15     DS    0H                                                               
         MVC   0(1,R5),0(R4)                                                    
         CLI   0(R4),12                                                         
         BE    DL18                                                             
         LA    R4,16(R4)                                                        
         LA    R5,1(R5)                                                         
         B     DL15                                                             
*                                                                               
DL18     LA    R2,12                                                            
         SH    R4,=H'16'                                                        
         ZIC   R3,0(R4)                                                         
         STC   R3,DSPLPER                                                       
         SR    R2,R3                                                            
         STC   R2,LENPER                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* ---------------------------------------------------------------------         
* DELETE A PRODUCTION JOB                                                       
* ---------------------------------------------------------------------         
*                                                                               
DELETIT  NTR1                                                                   
         LA    R3,MPARM                                                         
         XC    MPARM,MPARM                                                      
         L     R2,ADACC                                                         
         ST    R2,MJOB                                                          
*                                                                               
         L     RF,ADCOMFAC                                                      
         ST    RF,MCOMFACS                                                      
*                                                                               
         MVI   MACTION,C'D'                                                     
         MVI   MDRAFT,C'Y'                                                      
         CLI   QOPT1,C'N'          UPDATE RECORDS ?                             
         BE    *+8                 NO                                           
         MVI   MDRAFT,C'N'         YES                                          
*                                                                               
         USING ACMD,RE                                                          
         L     RE,AMONACC                                                       
         L     RE,ACMAPRAT                                                      
         ST    RE,MPRORATA                                                      
         DROP  RE                                                               
*                                                                               
         MVC   MGETOPT,GETOPT                                                   
*                                                                               
         MVC   MRAPPER,VRAPPER     A(0) IF NOT GETTING RAPS                     
*                                                                               
         LA    R2,QUESTOR                                                       
         ST    R2,MPERSON                                                       
*                                                                               
         GOTO1 =V(ACJOBMNT),MPARM,RR=RB                                         
         CLI   MPARM+4,0                                                        
*                                                                               
DELEX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ADD A RECORD ACTIVITY ELEMENT TO A PRODUCTION VENDOR                          
*                                                                               
PRERAP   NTR1                                                                   
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         CLI   QOPT1,C'N'          OPTION NOT TO WRITE                          
         BE    XIT                 YES                                          
*                                                                               
         OC    VRAPPER,VRAPPER     IS AGY USING RAPS                            
         BZ    XIT                 NO                                           
*                                                                               
         L     RE,ADCMPEL                                                       
         USING CPYELD,RE                                                        
         L     R3,ADACC                                                         
         CLC   CPYSUPP,1(R3)              IS THIS A PROD SUP'L'R                
         BE    PRERAP2             YES                                          
*                                                                               
         CLI   CPYLN,CPYLN2Q                                                    
         BL    XIT                                                              
         CLI   CPYXSUPP,0          TEST FOR EXTRA PROD SUPPLIERS                
         BE    XIT                 NO                                           
         CLC   CPYSUPP(1),1(R3)                                                 
         BNE   XIT                                                              
         CLC   CPYXSUPP,2(R3)      TEST IF ACCOUNT IS EXTRA SUPPLIER            
         BNE   XIT                                                              
         DROP  RE                                                               
*                                                                               
PRERAP2  MVI   RAPACTN,RAPAELEM                                                 
         MVC   RAPCPY,RCCOMPFL                                                  
         MVI   RAPRTYP,RAPKRSUP    RECORD=PRODUCTION SUPPLIER                   
         MVI   RAPEMU,C'Y'                                                      
         MVC   RAPACOM,ADCOMFAC                                                 
         L     RE,ADACC                                                         
         ST    RE,RAPAREC                                                       
*                                                                               
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
* CREATE A RECORD ACTIVITY POINTER FOR A PRODUCTION VENDOR                      
*                                                                               
POSTRAP  CLI   RAPACTN,RAPAELEM    DID I ADD A RAP ELEMENT                      
         BNER  RE                  NO, DON'T ADD POINTER                        
         NTR1                                                                   
         MVI   RAPACTN,RAPAPTR                                                  
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*              PRINT A TOTAL AT REQUEST END                                     
         SPACE 3                                                                
DL20     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         CP    DELCOUNT,=P'0'                                                   
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         MVC   P+40(28),=C'NUMBER OF ACCOUNTS DELETED -'                        
         EDIT  (P6,DELCOUNT),(10,P+69),ALIGN=LEFT                               
         GOTO1 ACREPORT                                                         
         ZAP   DELCOUNT,=P'0'                                                   
         MVC   PAGE,=H'1'                                                       
         SPACE 2                                                                
XIT      XMOD1 1                                                                
                                                                                
VRAPPER  DS    A(0)                                                             
AIO      DS    A(IO)                                                            
RQSW     DS    XL1                                                              
RQNC     EQU   X'80'               AGENCY ON NEW COST                           
RQTMS    EQU   X'40'               AGENCY ON TMS                                
DELCOUNT DC    PL6'0'                                                           
*                                                                               
DSPLPER  DS    XL1                 DISPL INTO ACCT FOR PERSON CODE              
LENPER   DS    XL1                 LENGTH OF PERSON CODE                        
*                                                                               
LEVLENS  DS    0XL4                LENGTHS FROM HEIRARCHY ELEMENT               
LVALEN   DS    XL1                                                              
LVBLEN   DS    XL1                                                              
LVCLEN   DS    XL1                                                              
LVDLEN   DS    XL1                                                              
*                                                                               
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
*                                                                               
DIRKEY   DS    CL56                MY KEY FOR DIRECTORY READ                    
*                                                                               
DMDKEY   DS    0CL56               RETURNED KEY ON DIRECTORY READ               
         ORG   DMDKEY+(TRNKDA-TRNRECD)                                          
DMDKEYA  DS    0XL4                DISK ADDRESS                                 
         ORG   DMDKEY                                                           
         DS    CL56                                                             
*                                                                               
ADWRK    DS    12D                                                              
*                                                                               
         DC    C'**IO AREA*'       MY IO AREA                                   
IO       DS    2000C                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE ACRAPPERD                                                      
*                                                                               
       ++INCLUDE ACJOBMNTD                                                      
*                                                                               
         EJECT                                                                  
* ACGENBOTH                                                                     
        PRINT OFF                                                               
       ++INCLUDE ACGENBOTH                                                      
        PRINT ON                                                                
* ACGENFILE                                                                     
        PRINT OFF                                                               
       ++INCLUDE ACGENFILE                                                      
        PRINT ON                                                                
* ACREPWORKD                                                                    
        PRINT OFF                                                               
       ++INCLUDE ACREPWORKD                                                     
        PRINT ON                                                                
* ACMASTD                                                                       
        PRINT OFF                                                               
       ++INCLUDE ACMASTD                                                        
        PRINT ON                                                                
* ACGENMODES                                                                    
        PRINT OFF                                                               
       ++INCLUDE ACGENMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081ACREP9702 08/06/19'                                      
         END                                                                    
