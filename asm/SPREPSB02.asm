*          DATA SET SPREPSB02  AT LEVEL 078 AS OF 03/24/20                      
*PHASE SPSB02B                                                                  
         TITLE 'SPSB02 - GENERATE SAP SUPPORT RECORDS'                          
***********************************************************************         
* LEVEL CHANGE COMMENTS                                               *         
* ---------------------                                               *         
* MHER 14OCT19 078-SPEC-33177-INCLUDE A TAG BRAND W/SAP PRODUCT ID  *         
***********************************************************************         
                                                                                
SPSB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSB02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX00                                                             
*                                                                               
EQXIT    CR    RC,RC                                                            
         J     *+6                                                              
NEQXIT   LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
FX00     DS    0H                                                               
*                                                                               
         OPEN  (SAPCODES,(OUTPUT))                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
*=============================================================                  
* OUTPUT MEDIA RECORDS                                                          
*=============================================================                  
                                                                                
         CLI   QOPT1,C'Y'          TEST SKIP SAP DATA                           
         JE    FX10                                                             
         LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
         MVI   SAPKTYPE,SAPKTYPE_MED                                            
         MVI   SAPKSYS,SAPKSYS_SPOT                                             
*                                                                               
         LA    R4,SAPMDTAB                                                      
         LA    R5,(SAPMDTBX-SAPMDTAB)/L'SAPMDTAB                                
*                                                                               
FX02     MVC   SAPKMED,0(R4)                                                    
         MVC   SAPRMEDT(2),1(R4)      MEDIA TYPE                                
         OC    SAPRMEDT,SPACES                                                  
         MVC   SAPRMATL(6),3(R4)                                                
         OC    SAPRMATL,SPACES                                                  
*                                                                               
         AP    MEDCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
*                                                                               
         LA    R4,L'SAPMDTAB(R4)                                                
         JCT   R5,FX02                                                          
         EJECT                                                                  
*==============================================================                 
* READ THROUGH HEADER RECORDS                                                   
*==============================================================                 
                                                                                
FX10     XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
*                                                                               
         NI    KEY+1,X'F0'         DROP MEDIA                                   
         OI    KEY+1,X'01'         START WITH SPOT TV                           
*                                                                               
         MVC   SVHIAM,KEY+1                                                     
         OI    SVHIAM,X'04'        SET A 5 AS END OF HEADERS                    
*                                                                               
FX11     GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY+1(1),SVHIAM                                                  
         JH    FX20                                                             
         OC    KEY+4(9),KEY+4     TEST CLTHDR                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX12     MVC   KEYSAVE,KEY         SAVE CLIENT IN PROCESS                       
         GOTO1 GETCLT                                                           
*                                                                               
         L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX14                NO                                           
* IF CLIENT HAS NO SAPCODE, SKIP CLIENT                                         
         CLI   CSAPCODE,C' '                                                    
         BNH   FX13                                                             
         CLI   CSAPCODE,C'X'                                                    
         BE    FX13                                                             
         CLC   =C'NONE',CSAPCODE                                                
         BNE   FX14                                                             
*                                                                               
FX13     MVC   KEY+4(4),=X'FFFFFFFF'   SKIP TO NEXT CLT                         
         J     FX11                                                             
*                                                                               
FX14     LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES                                                  
         MVI   SAPKTYPE,SAPKTYPE_CLT                                            
         MVI   SAPKSYS,SAPKSYS_SPOT                                             
*                                                                               
         LLC   RE,KEY+1            GET AGY-MD                                   
         N     RE,=X'0000000F'     DROP AGY                                     
         LA    RE,MDTAB-1(RE)                                                   
         MVC   SAPKMED,0(RE)                                                    
*                                                                               
         GOTO1 CLUNPK,DMCB,(CPROF+6,KEY+2),SAPKCLT                              
*                                                                               
         MVC   SAPRCUST,CSAPCODE                                                
         MVC   SAPROFFC(1),COFFICE                                              
         MVI   SAPROFFC+1,C' '                                                  
*                                                                               
         AP    CLTCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
*                                                                               
         MVC   SVSAPKEY,SAPRKEY    SAVE CLT KEY FOR PRDS                        
*                                                                               
FX16     GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         JNE   FX19                                                             
*                                                                               
         OC    KEY+7(6),KEY+7      TEST ANY EST/BILL                            
         JNZ   FX16                                                             
         CLI   KEY+7,0             TEST ANY EST                                 
         JNE   FX16                YES - SKIP                                   
                                                                                
* MUST BE PRDHDR                                                                
                                                                                
         GOTO1 GETPRD                                                           
*                                                                               
         L     R8,ADPRD                                                         
         USING PRDHDRD,R8                                                       
*                                                                               
         CLC   PLEN,=Y(PRDHDRL)    TEST ROOM IN REC FOR SAP CODE                
         BNH   FX16                NO - SKIP                                    
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX18                NO                                           
*                                                                               
         CLI   PSAPCODE,C' '                                                    
         BNH   FX16                                                             
         CLI   PSAPCODE,C'X'                                                    
         BE    FX16                                                             
         CLC   =C'NONE',PSAPCODE                                                
         BE    FX16                                                             
*                                                                               
FX18     LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPRKEY,SVSAPKEY                                                 
         MVI   SAPKTYPE,SAPKTYPE_PRD                                            
         MVC   SAPKPRD,PKEYPRD                                                  
         MVC   SAPRPRD,PSAPCODE                                                 
*                                                                               
         AP    PRDCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
         J     FX16                                                             
*                                                                               
FX19     CLC   KEY+1(1),SVHIAM     TEST PAST END OF AGY                         
         JH    FX20                                                             
         OC    KEY+4(9),KEY+4      MUST BE A CLTHDR                             
         JNZ   *+2                                                              
         J     FX12                                                             
*                                                                               
MDTAB    DC    C'TRNX'                                                          
SAPMDTAB DS    0CL9                                                             
         DC    C'TSPM00019'        SPOT TV                                      
         DC    C'RSPM00018'        SPOT RADIO                                   
         DC    C'XSPM00017'        NETWORK RADIO                                
                                                                                
SAPMDTBX EQU   *                                                                
TMPCOUNT DC    PL4'0'                                                           
         EJECT                                                                  
*===============================================================                
* NOW READ STATION ADDRESS RECORDS                                              
*===============================================================                
FX20     ZAP   TMPCOUNT,=P'0'                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'A'                                                         
*                                                                               
         MVI   NETPAK,C'Y'                                                      
         L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
         CLI   MCNETPAK,C'Y'                                                    
         JE    *+12                                                             
         MVI   KEY+1,C'D'          SKIP MEDIA C                                 
         MVI   NETPAK,C'N'                                                      
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGHSTAD                                                         
         B     FX24                                                             
*                                                                               
FX22     GOTO1 SEQSTAD                                                          
*                                                                               
FX24     L     R8,ADSTATAD                                                      
         USING ADDRREC,R8                                                       
*                                                                               
         CLI   0(R8),C'A'                                                       
         BNE   FX30                                                             
*                                                                               
         CLC   ADDKAGY,QAGY        RIGHT AGENCY                                 
         BNE   FX22                NO                                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX28                NO                                           
*                                                                               
         CLC   ARECL,=AL2(ADRREC3Q) REC BIG ENOUGH TO HAVE SAPCODE              
         BL    FX22                                                             
*                                                                               
         LA    R0,5                                                             
         LA    R1,ADRSAP                                                        
*                                                                               
FX26     CLI   0(R1),C'A'                                                       
         BL    FX22                                                             
         CLI   0(R1),C'Z'                                                       
         BNH   FX27                                                             
         CLI   0(R1),C'0'                                                       
         BL    FX22                                                             
         CLI   0(R1),C'9'                                                       
         BH    FX22                                                             
FX27     LA    R1,1(R1)                                                         
         BCT   R0,FX26                                                          
*                                                                               
FX28     LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES     SPACE FILL KEY                               
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_VNDR                                           
         MVI   SAPKSYS,SAPKSYS_SPOT                                             
         MVC   SAPKMED,ADDKMED                                                  
*                                                                               
         CLI   NETPAK,C'Y'                                                      
         JNE   *+12                                                             
         MVI   SAPKSYS,SAPKSYS_NET                                              
         MVI   ADDKCALL+4,C' '      SUPPRESS N FOLLOWING NETWORK                
         DROP  RE                                                               
*                                                                               
         MVC   SAPKVJN,SPACES                                                   
         MVC   SAPKVJN(5),ADDKCALL  OUTPUT 5 STATION CHARS FOR JOIN             
* OUTPUT SUBMEDIA EXCEPT FOR TV                                                 
         CLI   SAPKMED,C'T'                                                     
         JNE   FX28A                                                            
         CLI   SAPKVJN+4,C' '      THIS IS INVALID AND CAUSES DUPS              
         JE    FX22                                                             
         CLI   SAPKVJN+4,C'T'                                                   
         JNE   FX28A                                                            
         MVI   SAPKVJN+4,C' '      ONLY BLANK SUB-MED T FOR MED T               
         J     FX28B                                                            
*                                                                               
FX28A    CLI   SAPKVJN+4,C'/'      AND FIX FOR NETWORK                          
         BNE   *+8                                                              
         MVI   SAPKVJN+4,C'N'                                                   
*                                                                               
FX28B    CLI   QOPT1,C'Y'          OUTPUT SAP CODE?                             
         JE    FX29                NO - DO NAME AND CITY                        
                                                                                
         CLC   ARECL,=X'00D0'                                                   
         BNH   FX22                                                             
         CLI   ADRSAP,C' '         SAP CODE THERE                               
         BNH   FX22                                                             
         MVC   SAPRNAME(L'ADRSAP),ADRSAP     SAP INTFC CODE                     
         J     FX29X                                                            
*                                                                               
FX29     MVC   SAPRNAME(16),ANAME                                               
         MVI   SAPRNAME+16,C'/'                                                 
         MVC   SAPRNAME+17(15),A2LINE       USUALLY CITY                        
*                                                                               
FX29X    OC    SAPRNAME,SPACES                                                  
*                                                                               
         CLI   NETPAK,C'Y'                                                      
         JNE   FX29Y                                                            
         BAS   RE,GETMED           GO GET MEDIA CODE FROM MASTER REC            
         JNE   FX29Y                                                            
         L     RE,ADSTAT           POINT TO STATION MASTER REC                  
         MVC   SAPKMED,STYPE-STAREC(RE)                                         
*                                                                               
FX29Y    AP    STACOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
         B     FX22                                                             
         DROP  R7                                                               
         EJECT                                                                  
*===============================================================                
* NOW READ STATION REP RECORDS                                                  
*===============================================================                
                                                                                
FX30     ZAP   TMPCOUNT,=P'1'                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'R'                                                         
         GOTO1 HIGHREP                                                          
         B     FX34                                                             
*                                                                               
FX32     GOTO1 SEQREP                                                           
*                                                                               
FX34     L     R8,ADREP                                                         
         USING REPREC,R8                                                        
*                                                                               
         CLI   0(R8),C'R'          STILL IN THE REP RECORDS                     
         BNE   FX40                NO                                           
*                                                                               
         CLC   REPKAGY,QAGY        RIGHT AGENCY                                 
         BNE   FX32                NO                                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST CARE ABOUT SAP CODES                    
         JE    FX38                                                             
*                                                                               
         LA    R0,5                                                             
         LA    R1,RSAPCODE                                                      
*                                                                               
FX36     CLI   0(R1),C'A'                                                       
         BL    FX32                                                             
         CLI   0(R1),C'Z'                                                       
         BNH   FX37                                                             
         CLI   0(R1),C'0'                                                       
         BL    FX32                                                             
         CLI   0(R1),C'9'                                                       
         BH    FX32                                                             
*                                                                               
FX37     LA    R1,1(R1)                                                         
         BCT   R0,FX36                                                          
*                                                                               
FX38     LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
         XC    SAPREC,SAPREC                                                    
         MVC   SAPKDATA,SPACES     SPACE FILL KEY                               
*                                                                               
         MVI   SAPKTYPE,SAPKTYPE_REP                                            
         MVI   SAPKSYS,SAPKSYS_SPOT                                             
         MVC   SAPKMED,REPKMED                                                  
         MVC   SAPKVJN,SPACES                                                   
         MVC   SAPKVJN(3),REPKREP                                               
*                                                                               
         MVC   SAPRVEND(L'RSAPCODE),RSAPCODE   SAP INTFC CODE                   
         OC    SAPRVEND,SPACES                                                  
*                                                                               
         AP    REPCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
         B     FX32                                                             
         DROP  R7                                                               
         EJECT                                                                  
*===============================================================                
* NOW READ MEDIA OFFICE RECORDS FROM GENDIR/GENFIL                              
* NEED TO OPEN GENDIR/GENFILE                                                   
*===============================================================                
                                                                                
FX40     DS    0H                                                               
*        B     FAKEIT                                                           
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,ADBUY                     
         ZAP   TMPCOUNT,=P'0'                                                   
*                                                                               
         LA    R8,BIGKEY                                                        
         USING MOFRECD,R8                                                       
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVI   MOFKTYP,C'O'                                                     
         MVI   MOFKSUB,MOFKS1Q                                                  
         MVC   MOFKAGY,QAGY                                                     
         MVC   BIGKEYSV,BIGKEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',BIGKEYSV,BIGKEY               
         B     FX44                                                             
*                                                                               
FX42     GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',BIGKEY,BIGKEY                 
*                                                                               
FX44     CLC   BIGKEY(24),BIGKEYSV   TEST SAME THRU AGY                         
         BNE   FX50                                                             
         LA    R8,BIGKEY                                                        
         CLI   MOFKSYS,2           SPOT ONLY                                    
         BNE   FX42                                                             
*                                                                               
FX45     LA    R0,MOFKDA-MOFRECD+BIGKEY                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',(R0),ADBUY,DMWORK             
         DROP  R8                                                               
*                                                                               
         L     R8,ADBUY                                                         
         USING MOFRECD,R8                                                       
*                                                                               
         LA    R6,MOFFIRST(R8)                                                  
         SR    R0,R0                                                            
*                                                                               
FX46     CLI   0(R6),MOSAPELQ      X'0D'                                        
         BE    FX48                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   FX46                                                             
         B     FX42                                                             
*                                                                               
         LA    R7,SAPREC                                                        
         USING SAPRECD,R7                                                       
*                                                                               
FX48     XC    SAPREC,SAPREC                                                    
         MVI   SAPKTYPE,SAPKTYPE_MOFF                                           
         MVI   SAPKSYS,C'S'                                                     
         MVC   SAPKMDOF,SPACES                                                  
         MVC   SAPKMDOF(L'MOFK1OF),MOFK1OF                                      
*                                                                               
         USING MOSAPD,R6                                                        
         MVC   SAPRCOMP,MOSAPCOM                                                
         MVC   SAPRSORG,MOSAPORG                                                
         MVC   SAPRPCTR,MOSAPCTR                                                
         DROP  R6                                                               
*                                                                               
         AP    MOFCOUNT,=P'1'                                                   
         PUT   SAPCODES,(R7)                                                    
         B     FX42                                                             
*                                                                               
FX50     B     ENDIN                                                            
         DROP  R7                                                               
         EJECT                                                                  
*=============================================================                  
* FOR NETPAK, READ STATION MASTER REC FOR CORRECT MEDIA CODE                    
*=============================================================                  
                                                                                
GETMED   NTR1                                                                   
         XC    KEY,KEY                                                          
         L     R8,ADSTATAD                                                      
         USING ADDRREC,R8                                                       
         MVC   KEY(9),0(R8)       TYPE/MEDIA/CALL/AGY                           
         MVI   KEY,C'S'                                                         
         MVI   KEY+6,C'N'         NEED THE N BACK NOW!                          
         GOTO1 HIGHSTA                                                          
*                                                                               
         MVI   BYTE,C'Y'                                                        
         L     RE,ADSTAT                                                        
         CLC   KEY(9),0(RE)                                                     
         JE    *+8                                                              
         MVI   BYTE,C'N'                                                        
                                                                                
*NOW RESTORE STAFILE READ SEQUENCE                                              
                                                                                
         MVI   KEY,C'A'                                                         
         GOTO1 HIGHSTAD                                                         
         CLC   KEY(9),0(R8)                                                     
         JNE   *+2                 WHERE DID RECORD GO?                         
*                                                                               
         CLI   BYTE,C'Y'                                                        
         J     EQXIT                                                            
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
SVHIAM   DC    X'00'                                                            
SVSAPKEY DS    XL16                                                             
NETPAK   DC    C'N'                                                             
FLIST    DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
         DC    CL8'X       '                                                    
*                                                                               
COUNTS   DS    0D                                                               
MEDCOUNT DC    PL4'0',CL20'MEDIA RECORDS'                                       
CLTCOUNT DC    PL4'0',CL20'CLIENTS OUT'                                         
PRDCOUNT DC    PL4'0',CL20'PRODUCTS OUT'                                        
STACOUNT DC    PL4'0',CL20'STATIONS OUT'                                        
REPCOUNT DC    PL4'0',CL20'REPS OUT ...'                                        
MOFCOUNT DC    PL4'0',CL20'MEDIA OFFICES OUT'                                   
COUNTX   EQU   *                                                                
*                                                                               
         DS    0D                                                               
BIGKEY   DS    XL48                                                             
BIGKEYSV DS    XL48                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*SAPREC*'                                                    
SAPREC   DS    CL48                                                             
*                                                                               
SAPCODES DCB   DDNAME=SAPCODES,DSORG=PS,RECFM=FB,LRECL=48,BLKSIZE=7200,X        
               MACRF=PM                                                         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACSAPREC                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT OFF                                                              
       ++INCLUDE GEGENOFF                                                       
       ++INCLUDE DDMASTD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078SPREPSB02 03/24/20'                                      
         END                                                                    
