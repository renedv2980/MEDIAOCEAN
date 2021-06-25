*          DATA SET PPREPSB02  AT LEVEL 107 AS OF 10/15/20                      
*PHASE PPSB02B                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE HEXIN                                                                  
                                                                                
         TITLE 'PPSB02 - GENERATE SAP SUPPORT RECORDS'                          
***********************************************************************         
* LEVEL CHANGE COMMENTS                                               *         
* ---------------------                                               *         
* JSHA 24SEP18 106-SPEC-22213-SUPPORT NEW PRNT MEDIA FOR DIGITAL AUDIO*         
* MHER 03OCT19 107-SPEC-33177-INCLUDE A TAG BRAND W/SAP PRODUCT ID  *         
***********************************************************************         
                                                                                
PPSB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPSB02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
*                                                                               
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         CLI   SVCMPNY,0           TEST FIRST TIME THRU                         
         JNE   EXIT                NO - EXIT                                    
         CLI   MODE,PROCREQ                                                     
         BE    FX00                                                             
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         OPEN  (SAPCODES,(OUTPUT))                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EQXIT    CR    RC,RC                                                            
         J     *+6                                                              
NEQXIT   LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*==============================================================                 
* GET THE ACCESS RECORD TO GET THE COMPANY CODE                                 
*==============================================================                 
                                                                                
FX00     DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,QAGENCY                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,PBUYREC              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,PBUYREC                                                       
         LA    R4,CT5DATA-CT5REC(R4)                                            
         USING CTSYSD,R4                                                        
         SR    R0,R0                                                            
         B     FX02B                                                            
*                                                                               
FX02A    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
FX02B    CLI   0(R4),0                                                          
         JE    *+2                                                              
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    X'21' ELEM?                                  
         BNE   FX02A                                                            
         CLI   CTSYSNUM,X'06'      ACC?                                         
         BNE   FX02A                                                            
         MVC   SVCMPNY,CTSYSAGB    SAVE COMPANY CODE                            
         DROP  R4                                                               
         EJECT                                                                  
*=============================================================                  
* OUTPUT MEDIA RECORDS                                                          
*=============================================================                  
                                                                                
         LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_MED                                            
         MVI   SAPKSYS,SAPKSYS_PRNT                                             
*                                                                               
         LA    R4,SAPMDTAB                                                      
         LA    R5,(SAPMDTBX-SAPMDTAB)/L'SAPMDTAB                                
*                                                                               
FX04     MVC   SAPKMED,0(R4)                                                    
         MVC   SAPRMEDT(2),1(R4)      MEDIA TYPE                                
         OC    SAPRMEDT,SPACES                                                  
         MVC   SAPRMATL(6),3(R4)                                                
         OC    SAPRMATL,SPACES                                                  
*                                                                               
         AP    MEDCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
*                                                                               
         LA    R4,L'SAPMDTAB(R4)                                                
         JCT   R5,FX04                                                          
                                                                                
*==============================================================                 
* READ THROUGH CLIENT RECORDS                                                   
*==============================================================                 
                                                                                
         LA    R4,MDTAB                                                         
*                                                                               
FX10     XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING PCLTKEY,R8                                                       
*                                                                               
         MVC   PCLTKAGY,QAGENCY                                                 
         MVC   PCLTKMED,0(R4)                                                   
         MVI   PCLTKRCD,X'02'                                                   
         DROP  R8                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(4),KEYSAVE      SAME AGY-MED/TYPE                            
         BNE   FX19X                                                            
*                                                                               
         GOTO1 GETCLI                                                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX15                NOPE                                         
*                                                                               
         MVI   ELCODE,X'51'        FIND SAP INTERFAC ELEMENT                    
         LA    R6,PCLTELEM                                                      
         BAS   RE,NEXTEL                                                        
         BNE   FX12                                                             
*                                                                               
         USING PSAPEL,R6                                                        
*                                                                               
         CLI   PSAPCODE,C' '                                                    
         BNH   FX12                                                             
*                                                                               
         CLI   PSAPCODE,C'X'                                                    
         BE    FX12                                                             
*                                                                               
         CLC   =C'NONE',PSAPCODE                                                
         BE    FX12                                                             
*                                                                               
FX15     LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_CLT                                            
         MVI   SAPKSYS,SAPKSYS_PRNT                                             
         MVC   SAPKMED,PCLTKMED                                                 
         MVC   SAPKCLT,PCLTKCLT                                                 
*                                                                               
         MVC   SAPRDATA,SPACES                                                  
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX15X               NOPE                                         
*                                                                               
         MVC   SAPRCUST,PSAPCODE                                                
         MVC   SAPROFFC(1),PCLTOFF                                              
         MVI   SAPROFFC+1,C' '                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
FX15X    AP    CLTCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
                                                                                
*===============================================================                
* NOW DO PRODUCT RECORDS FOR THIS CLIENT                                        
*===============================================================                
                                                                                
FX16     MVC   MYCLTKEY,KEY        SAVE CURRENT CLIENT KEY                      
*                                                                               
         LA    R8,KEY                                                           
         USING PPRDKEY,R8                                                       
         MVI   PPRDKRCD,X'06'      SET TO READ PRDS FOR THIS CLT                
         GOTO1 HIGH                                                             
         J     FX17A                                                            
         DROP  R8                                                               
*                                                                               
FX17     GOTO1 SEQ                                                              
*                                                                               
FX17A    CLC   KEY(7),KEYSAVE      SAME A-M/RECTYPE/CLT                         
         JNE   FX19                                                             
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX18                NOPE                                         
*                                                                               
         GOTO1 GETPROD                                                          
*                                                                               
         MVI   ELCODE,X'51'        FIND SAP INTERFAC ELEMENT                    
         LA    R6,PPRDELEM                                                      
         BAS   RE,NEXTEL                                                        
         JNE   FX17                                                             
*                                                                               
         USING PPSAPEL,R6                                                       
*                                                                               
         CLI   PPSAPCODE,C' '                                                   
         JNH   FX17                                                             
*                                                                               
         CLI   PPSAPCODE,C'X'                                                   
         JE    FX17                                                             
*                                                                               
         CLC   =C'NONE',PPSAPCODE                                               
         JE    FX17                                                             
*                                                                               
FX18     LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_PRD                                            
         MVI   SAPKSYS,SAPKSYS_PRNT                                             
         MVC   SAPKMED,PPRDKMED                                                 
         MVC   SAPKCLT,PPRDKCLT                                                 
         MVC   SAPKPRD,PPRDKPRD                                                 
*                                                                               
         MVC   SAPRDATA,SPACES                                                  
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX18X               NOPE                                         
*                                                                               
         MVC   SAPRPRD,PPSAPCODE                                                
         MVC   SAPROFFC(1),PPRDOFFC                                             
         MVI   SAPROFFC+1,C' '                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
FX18X    AP    PRDCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
*                                                                               
         J     FX17                                                             
                                                                                
*=======================================================                        
* AT END OF AG/MD, TRY NEXT MEDIA CODE                                          
*=======================================================                        
                                                                                
FX19     MVC   KEY,MYCLTKEY        RESTORE LAST CLIENT KEY                      
         GOTO1 HIGH                                                             
         B     FX12                                                             
*                                                                               
FX19X    LA    R4,1(R4)            NO MORE CLTS, TRY NEXT MEDIA                 
         CLI   0(R4),X'FF'                                                      
         BNE   FX10                                                             
         B     FX20                                                             
*                                                                               
MDTAB    DC    C'BDILMNOSTVW',X'FF'                                             
SAPMDTAB DS    0CL9                                                             
         DC    C'MPRM00001'        MAGAZINE                                     
         DC    C'NPRM00002'        NEWSPAPER                                    
         DC    C'IPRM00007'        INTERACTIVE                                  
         DC    C'OPRM00005'        OUTDOOR                                      
         DC    C'SPRM00004'        SUPPLEMENT                                   
         DC    C'TPRM00003'        TRADE                                        
         DC    C'LPRM00007'  ???   SOCIAL                                       
         DC    C'BPRM00008'        SOCIAL                                       
         DC    C'VPRM00009'        SOCIAL                                       
         DC    C'WPRM00010'        SOCIAL                                       
         DC    C'DPRM00011'        DIGITAL                                      
SAPMDTBX EQU   *                                                                
TMPCOUNT DC    PL4'0'                                                           
         EJECT                                                                  
*===============================================================                
* NOW READ PUB ADDRESS RECORDS FOR PUBS ON SAP INPUT FILE                       
*===============================================================                
*                                                                               
FX20     LA    R4,MDTAB                                                         
*                                                                               
FX22     XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING PUBKEY,R8                                                        
         MVC   PUBKMED,0(R4)       MOVE IN MEDIA                                
*                                                                               
         GOTO1 HIGHPUB                                                          
*                                                                               
FX24     CLC   KEY(L'PUBKMED),KEYSAVE SAME MED?                                 
         JNE   FX46                NO SAP CODE FOR THIS PUB!                    
*                                                                               
         CLC   PUBKAGY,QAGENCY     RIGHT AGY                                    
         BNE   FX26                                                             
         CLI   PUBKCOD,X'81'       TEST PUB NAME REC                            
         JNE   FX26                                                             
         J     FX28                                                             
*                                                                               
FX26     GOTO1 SEQPUB                                                           
         B     FX24                                                             
         DROP  R8                                                               
*                                                                               
FX28     GOTO1 GETNAME             READ PUBNAME REC                             
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX29                NO                                           
*                                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCODE,X'91'                                                     
         BAS   RE,NEXTEL                                                        
         JNE   FX26                                                             
         J     FX30                                                             
*                                                                               
FX29     LA    R6,PUBREC+33        FIND PUB NAMEL TO SEND NAME/ZONE             
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL2                                                       
         JNE   *+2                                                              
         J     FX36                                                             
*                                                                               
         USING PUBSAPEL,R6                                                      
                                                                                
* MAKE SURE IT'S A VALID SAP CODE (STARTS WITH 5 DIGITS)                        
                                                                                
FX30     LA    R1,PUBSAPCD                                                      
         LA    R0,5                                                             
*                                                                               
FX32     CLI   0(R1),C'A'                                                       
         BL    FX26                                                             
         CLI   0(R1),C'Z'                                                       
         BNH   FX34                                                             
         CLI   0(R1),C'0'                                                       
         BL    FX26                                                             
         CLI   0(R1),C'9'                                                       
         BH    FX26                                                             
*                                                                               
FX34     LA    R1,1(R1)                                                         
         BCT   R0,FX32                                                          
*                                                                               
FX36     MVC   SAPREC,SPACES                                                    
         LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_VNDR                                           
         MVI   SAPKSYS,SAPKSYS_PRNT                                             
         MVC   SAPKMED,PUBKMED                                                  
         LA    R3,6                                                             
         CLI   PUBKED,0            DO WE HAVE AN EDITION?                       
         JH    FX40                                                             
         LA    R3,5                                                             
         CLI   PUBKZON,0           DO WE HAVE A ZONE?                           
         JH    FX40                                                             
         LA    R3,4                                                             
*                                                                               
FX40     GOTO1 HEXOUT,DMCB,PUBKPUB,SAPKVJN,(R3),=C'TOG'                         
         CLI   QOPT1,C'Y'          TEST NO SAP CODES                            
         JE    FX42                                                             
         MVC   SAPRVEND(L'PUBSAPCD),PUBSAPCD-PUBSAPEL(R6)                       
         OC    SAPRVEND,SPACES                                                  
         J     FX44                                                             
*                                                                               
FX42     MVC   SAPRNAME(20),PUBNAME-PUBNAMEL(R6)                                
         CLI   PUBZNAME,C' '                                                    
         JNH   FX44                                                             
         MVI   SAPRNAME+16,C'/'                                                 
         MVC   SAPRNAME+17(13),PUBZNAME-PUBNAMEL(R6)                            
*                                                                               
FX44     OC    SAPRNAME,SPACES                                                  
         AP    PUBCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
         B     FX26                GET NEXT REC                                 
         DROP  R6,R7                                                            
                                                                                
*=======================================================                        
* AT END OF AG/MD, TRY NEXT MEDIA CODE                                          
*=======================================================                        
                                                                                
FX46     LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   FX22                                                             
         B     FX50                                                             
*                                                                               
         EJECT                                                                  
*===============================================================                
* NOW READ MEDIA OFFICE RECORDS FROM GENDIR/GENFIL                              
* NEED TO OPEN GENDIR/GENFILE                                                   
*===============================================================                
                                                                                
FX50     GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,PBUYREC                   
*                                                                               
         LA    R8,BIGKEY                                                        
         USING MOFRECD,R8                                                       
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVI   MOFKTYP,C'O'                                                     
         MVI   MOFKSUB,MOFKS1Q                                                  
         MVC   MOFKAGY,QAGENCY                                                  
         MVC   BIGKEYSV,BIGKEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',BIGKEYSV,BIGKEY               
         B     FX54                                                             
*                                                                               
FX52     GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',BIGKEY,BIGKEY                 
*                                                                               
FX54     CLC   BIGKEY(24),BIGKEYSV   TEST SAME THRU AGY                         
         BNE   FX60                                                             
         LA    R8,BIGKEY                                                        
         CLI   MOFKSYS,4           PRINT ONLY                                   
         BNE   FX52                                                             
*                                                                               
         LA    R0,MOFKDA-MOFRECD+BIGKEY                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',(R0),PBUYREC,DMWORK           
*                                                                               
         LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         LA    R8,PBUYREC                                                       
         USING MOFRECD,R8                                                       
*                                                                               
         LA    R6,MOFFIRST(R8)                                                  
         SR    R0,R0                                                            
*                                                                               
FX56     CLI   0(R6),MOSAPELQ      X'0D'                                        
         BE    FX58                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   FX56                                                             
         B     FX52                                                             
*                                                                               
         LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
FX58     XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_MOFF                                           
         MVI   SAPKSYS,C'P'                                                     
         MVC   SAPKMDOF,MOFK1OF                                                 
         MVI   SAPKMDOF+1,C' '                                                  
*                                                                               
         USING MOSAPD,R6                                                        
         MVC   SAPRCOMP,MOSAPCOM                                                
         MVC   SAPRSORG,MOSAPORG                                                
         MVC   SAPRPCTR,MOSAPCTR                                                
         DROP  R6                                                               
*                                                                               
FX58X    AP    MOFCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
         B     FX52                                                             
*                                                                               
         DROP  R7,R8                                                            
         EJECT                                                                  
*============================================================                   
* NOW OUTPUT REP RECORDS FROM PRTFILE                                           
*============================================================                   
                                                                                
FX60     LA    R4,MDTAB                                                         
         ZAP   TMPCOUNT,=P'0'                                                   
*                                                                               
FX62     XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING PREPKEY,R8                                                       
*                                                                               
         MVC   PREPKAGY,QAGENCY                                                 
         MVC   PREPKMED,0(R4)                                                   
         MVI   PREPKRCD,X'11'                                                   
         DROP  R8                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     FX66                                                             
*                                                                               
FX64     GOTO1 SEQ                                                              
*                                                                               
FX66     CLC   KEY(4),KEYSAVE      SAME AGY-MED/TYPE                            
         BNE   FX80                                                             
*                                                                               
         GOTO1 GETREP                                                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX70                                                             
*                                                                               
         MVI   ELCODE,X'21'        FIND SAP INTERFAC ELEMENT                    
         LA    R6,PREPELEM                                                      
         BAS   RE,NEXTEL                                                        
         BNE   FX64                                                             
*                                                                               
         USING PREPSAPEL,R6                                                     
*                                                                               
         LA    R1,PREPSAPCD                                                     
         LA    R0,5                                                             
*                                                                               
FX68     CLI   0(R1),C'A'                                                       
         BL    FX64                                                             
         CLI   0(R1),C'Z'                                                       
         BNH   FX69                                                             
         CLI   0(R1),C'0'                                                       
         BL    FX64                                                             
         CLI   0(R1),C'9'                                                       
         BH    FX64                                                             
*                                                                               
FX69     LA    R1,1(R1)                                                         
         BCT   R0,FX68                                                          
*                                                                               
FX70     LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_REP                                            
         MVI   SAPKSYS,SAPKSYS_PRNT                                             
         MVC   SAPKMED,PREPKMED                                                 
         MVC   SAPKVJN(4),PREPKREP                                              
*                                                                               
         MVC   SAPRVEND(L'PREPSAPCD),PREPSAPCD                                  
         OC    SAPRVEND,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         AP    REPCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
         B     FX64                                                             
                                                                                
*=======================================================                        
* AT END OF AG/MD, TRY NEXT MEDIA CODE                                          
*=======================================================                        
                                                                                
FX80     LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   FX62                                                             
         B     ENDIN                                                            
         EJECT                                                                  
*============================================================                   
* CLOSE FILES AND PRINT COUNTS                                                  
*============================================================                   
                                                                                
ENDIN    CLOSE SAPCODES                                                         
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,COUNTS                                                        
         LA    R5,COUNTX                                                        
*                                                                               
ENDIN2   OI    3(R4),X'0F'                                                      
         UNPK  P(6),0(4,R4)                                                     
         MVC   P+8(20),4(R4)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    R4,24(R4)                                                        
         CR    R4,R5                                                            
         BL    ENDIN2                                                           
*                                                                               
         MVI   MODE,REQLAST                                                     
         J     EXIT                                                             
*                                                                               
SVCMPNY  DS    X                                                                
*                                                                               
         DS    0D                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=24'                                    
*                                                                               
         DC    CL8'*SORTREC'                                                    
SORTREC  DS    CL24                                                             
         ORG   SORTREC                                                          
SORTMED  DS    C                   IF KEY CHANGES, CHANGE SORTCARD              
SORTPUB  DS    CL12                                                             
         ORG                                                                    
SVSORT   DS    CL13                                                             
*                                                                               
COUNTS   DS    0D                                                               
MEDCOUNT DC    PL4'0',CL20'MEDIA RECORDS OUT'                                   
CLTCOUNT DC    PL4'0',CL20'CLIENTS OUT'                                         
PRDCOUNT DC    PL4'0',CL20'PRODUCTS OUT'                                        
PUBCOUNT DC    PL4'0',CL20'PUBS OUT'                                            
MOFCOUNT DC    PL4'0',CL20'MEDIA OFFICES OUT'                                   
REPCOUNT DC    PL4'0',CL20'REP RECORDS OUT'                                     
COUNTX   EQU   *                                                                
*                                                                               
         DS    0D                                                               
BIGKEY   DS    XL48                                                             
BIGKEYSV DS    XL48                                                             
MYCLTKEY DS    CL25                                                             
*                                                                               
         DS    0D                                                               
*                                                                               
SAPCODES DCB   DDNAME=SAPCODES,DSORG=PS,RECFM=FB,LRECL=48,BLKSIZE=7200,X        
               MACRF=PM                                                         
*                                                                               
FLIST    DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
         DC    CL8'X       '                                                    
ELCODE   DS    C                                                                
         LTORG                                                                  
                                                                                
         SPACE 2                                                                
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
NEXTEL2  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         CLI   0(R6),0                                                          
         BNE   NEXTEL                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*SAPREC*'                                                    
SAPREC   DS    CL64                                                             
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVLEN   DC    F'0'                                                             
RCVREC   DS    0C                                                               
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
RKEY     DS    XL13                                                             
         DS    6000C                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
*                                                                               
* DSECT FOR PRINT LINE                                                          
PPWORKD  DSECT                                                                  
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
POFFC    DS    CL2                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PPUB     DS    CL14                                                             
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PYRSVC   DS    CL2                                                              
         DS    CL1                                                              
PMONSVC  DS    CL2                                                              
         DS    CL1                                                              
PINV     DS    CL6                                                              
         DS    CL2                                                              
PDOLS    DS    CL13                                                             
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE ACSAPREC                                                       
       ++INCLUDE GEGENOFF                                                       
       ++INCLUDE PUBSAPEL                                                       
       ++INCLUDE CTGENFILE                                                      
*PREFIX=AC_                                                                     
       ++INCLUDE ACGENFILE                                                      
*PREFIX=                                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107PPREPSB02 10/15/20'                                      
         END                                                                    
