*          DATA SET NENAV30    AT LEVEL 062 AS OF 04/04/18                      
*PHASE T31830A                                                                  
T31830   TITLE 'NENAV30 - NETWORK MATCHMAKER - DOWNLOAD HEADERS'                
T31830   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV30**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         USING TSARD,TSARLOCL                                                   
         USING TSARFLDS,TSARREC                                                 
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
*=================================================================*             
* H2 DATA                                                         *             
*=================================================================*             
         SPACE 1                                                                
*                                                                               
* PASS OFIICE BUYER INFORMATION                                                 
*                                                                               
         L     RE,AIO3             INIT AIO3                                    
         MVI   0(RE),X'00'                                                      
*                                                                               
         LHI   R1,X'50'            LOCATE 02 HEADER                             
         BAS   RE,SENDH                                                         
*                                                                               
* READ THE OFFICE RECORD                                                        
S3       XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D60'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'         DROP MEDIA CODE                              
         MVC   KEY+3(2),QGRP       BUYING GROUP = OFFICE                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    S4                                                               
         MVI   ERROR+1,BADOFC                                                   
         GOTO1 SENDMSG                                                          
*                                                                               
S4       L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         USING OFCRECD,R6                                                       
         LA    R1,OFCEL                                                         
         MVC   SVOFCNAM(24),2(R1)                                               
*                                                                               
* PASS OFFICE NAME                                                              
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,SVOFCNAM                                                      
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
         EJECT                                                                  
* READ THE BUYER RECORD SUPERVISOR SEQUENCE                                     
*                                                                               
S5       OC    QSPV,QSPV           CHECK SUPERVISOR REQUEST                     
         BZ    S6                                                               
         CLI   LSTBUYKY,X'0D'      FIRST PASS                                   
         BNE   S5A                                                              
         MVC   KEY,LSTBUYKY                                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH    RESET THE POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                KEY MUST BE THERE                            
         GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
         B     S5B                                                              
S5A      XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DE3'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'         DROP MEDIA CODE                              
         MVC   KEY+3(2),QGRP       BUYING GROUP = OFFICE                        
         MVC   KEY+5(4),QSPV       SUPERVISOR CODE                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
S5B      CLC   KEY(9),KEYSAVE                                                   
         BNE   EXIT                                                             
         MVC   LSTBUYKY,KEY                                                     
         MVC   CURBUYER,KEY+9                                                   
         B     S7                                                               
         SPACE 2                                                                
*                                                                               
* READ THE BUYER RECORD BUYER LEVEL REQUEST                                     
*                                                                               
S6       XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D62'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'         DROP MEDIA CODE                              
         MVC   KEY+3(2),QGRP       BUYING GROUP = OFFICE                        
         MVC   KEY+5(4),QBYR                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    S7                                                               
         MVI   ERROR+1,BADBYR                                                   
         GOTO1 SENDMSG                                                          
*                                                                               
S7       L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         USING BYRRECD,R6                                                       
*                                                                               
         LA    R6,BYREL                                                         
         USING BYRNAMED,R6                                                      
*                                                                               
         MVC   SVBYRNAM,SPACES                                                  
         MVC   SVBYRNAM(L'BYRFNAME),BYRFNAME                                    
         LA    R4,SVBYRNAM+L'BYRFNAME+1                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         AHI   R4,2                                                             
         MVC   0(13,R4),BYRLNAME                                                
         MVC   SVBYRFLT,BYRFILT                                                 
         MVC   SVBYROP1,BYROPT1                                                 
         MVC   FULL,BYRSPV         SAVE SUPERVISOR CODE FOR GETSPV              
*                                                                               
* PASS BUYER NAME                                                               
*                                                                               
         OC    QBYR,QBYR           IS THIS A BUYER LEVEL REQUEST                
         BZ    S8                                                               
         LHI   R1,X'02'                                                         
         LA    R4,SVBYRNAM                                                      
         BAS   RE,SENDD                                                         
         SPACE 1                                                                
*=================================================================*             
* READ SUPERVISOR RECORD                                          *             
*=================================================================*             
         SPACE 1                                                                
S8       BAS   RE,GETSPV                                                        
         SPACE 1                                                                
*=================================================================*             
* CHECK CONNECT PASSWORD MATCHES BUYER OR SUPERVISOR PASSWORD     *             
*=================================================================*             
         SPACE 1                                                                
         CLI   DDS,C'Y'            DDS TERM?                                    
         BE    S10                  YES - ALL ACCESS                            
         CLI   SVMKPRF+5,C'Y'       USE SECURITY?                               
         BNE   S10                  NO                                          
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',FULL),F#TPASS   EXTRACT CONNECT PWD         
         OC    FULL(2),FULL        ON SECURITY?                                 
         BZ    S10                  NO                                          
*                                                                               
         USING BPIDELD,R6                                                       
         L     R6,AIO2             A(BUYER REC)                                 
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'22',(R6)),0                        
         CLI   12(R1),0                                                         
         BNE   S9                                                               
         L     R6,12(R1)                                                        
         CLC   BPIDNO,FULL         THIS USER?                                   
         BE    S10                  NO                                          
*                                                                               
S9       L     R6,AIO3             A(BUYER REC)                                 
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'22',(R6)),0                        
         L     R6,12(R1)                                                        
         CLI   12(R1),0                                                         
         BNE   *+14                                                             
         CLC   BPIDNO,FULL                                                      
         BE    S10                                                              
*                                                                               
         MVI   ERROR+1,SCTYERR                                                  
         GOTO1 SENDMSG                                                          
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* BUILD TSAR BUFFER OF RELEVANT MATCHMAKER PASSIVES               *             
*=================================================================*             
         SPACE 1                                                                
S10      XC    TSARLOCL,TSARLOCL                                                
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,HKEYX-HKEY                                                
         MVI   TSPAGN,8            REQUEST 8 PAGES                              
         OI    TSRECI,TSRWSSVR+TSRXTN                                           
         OI    TSINDS,TSINODSK     TEMPEST IS IN USE BY FALINK !!!              
         OI    TSINDS,TSIXTTWA     AND IT HAS BIG PAGES !                       
         LHI   R0,HRECX-HREC                                                    
         STH   R0,TSRECL                                                        
         LA    R1,HREC                                                          
         ST    R1,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
S12      L     R6,AIO2                                                          
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'15',(R6)),0                        
         L     R6,12(R1)                                                        
         CLI   12(R1),0                                                         
         BNE   S18                                                              
         MVI   0(R6),X'14'          DONE TO FAKE OUT HELLO                      
         BAS   RE,BLDSTGRP                                                      
         B     S12                                                              
*                                                                               
S18      MVI   ALLSTA,C'Y'                                                      
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'05',AIO2),0                        
         L     R6,12(R1)                                                        
         CLI   12(R1),0                                                         
         BNE   S19                                                              
         L     R6,12(R1)                                                        
         ST    R6,STAELEM                                                       
         MVI   ALLSTA,C'N'                                                      
*                                                                               
*  GET FIRST CLIENT                                                             
S19      GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'05',AIO3),0                        
         CLI   12(R1),0                                                         
         BNE   S50                                                              
         L     R7,12(R1)                                                        
*                                                                               
S20      ST    R7,CLIELEM                                                       
         BAS   RE,CLTFILT           CHECK FOR FILTER MATCH                      
         BNE   S50                                                              
*                                                                               
*  READ INV HEADER                                                              
*                                                                               
         BAS   RE,GETHEAD                                                       
         EJECT                                                                  
*                                                                               
S50      MVI   TSACTN,TSAGET                                                    
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    S53                                                              
         MVI   ERROR+1,NOINVS                                                   
         GOTO1 SENDMSG                                                          
*                                                                               
S51      MVI   TSACTN,TSANXT                                                    
         OC    QSPV,QSPV                                                        
         BZ    S52A                                                             
*                                                                               
S52      BRAS  RE,CALLTSAR                                                      
         BNE   S5                  SUPERVISOR LEVEL                             
         B     S53                                                              
*                                                                               
S52A     BRAS  RE,CALLTSAR                                                      
         BNE   EXIT                BUYER LEVEL                                  
*                                                                               
S53      BAS   RE,AUTHCLT          MAKE SURE CLT IS AUTH FOR THIS ID            
         BNE   S51                  YES                                         
*                                                                               
* PASS INVENTORY HEADER INFORMATION                                             
*                                                                               
S54      LHI   R1,X'51'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
* PASS THE NETWORK                                                              
*                                                                               
         LHI   R1,X'03'                                                         
         LA    R4,HNET                                                          
         BAS   RE,SENDD                                                         
*                                                                               
* PASS THE CLIENT                                                               
*                                                                               
         GOTO1 VCLUNPK,DMCB,(SVCPROF+6,HCLT),WORK                               
         LHI   R1,X'04'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
* PASS THE PRODUCT                                                              
*                                                                               
         CLI   HPRDG,0                                                          
         BNE   S55                                                              
         LHI   R1,X'05'                                                         
         LA    R4,HPRD                                                          
         BAS   RE,SENDD                                                         
*                                                                               
* PASS THE PRODUCT NAME                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,HCLT                                                     
         MVC   PKEYPRD,HPRD                                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO4                                    
         L     R4,AIO4                                                          
         LA    R4,PNAME-PKEY(R4)                                                
         LHI   R1,X'0B'                                                         
         BAS   RE,SENDD                                                         
         B     S55A                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
* PASS THE PRODUCT GROUP                                                        
*                                                                               
S55      MVC   FULL(1),HPRDG                                                    
         MVC   FULL+1(3),HPRD                                                   
         LHI   R1,X'12'                                                         
         LA    R4,FULL                                                          
         BAS   RE,SENDD                                                         
*                                                                               
* PASS THE PRODUCT GROUP NAME                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGRECD,R4                                                       
         MVC   PRGKTYP,=XL2'0D01'                                               
         MVC   PRGKAGMD,BAGYMD                                                  
         MVC   PRGKCLT,HCLT                                                     
         MVC   PRGKID,HPRDG                                                     
         OI    PRGKID,X'40'        MAKE CAPITAL                                 
         XC    WORK,WORK                                                        
         MVC   WORK(3),HPRD                                                     
         PACK  WORK+10(3),WORK(5)                                               
         MVC   PRGKGRP,WORK+10                                                  
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   S55A                                                             
******   BE    *+6                                                              
******   DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO4                                    
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'10',AIO4),0                        
         CLI   12(R1),0                                                         
         BNE   S55A                                                             
         L     R4,12(R1)                                                        
         LA    R4,2(R4)                                                         
         CLI   24(R4),X'40'         CHECK SECONDARY BREAK NAME                  
         BNH   *+8                  NO USE FIRST BRAEK NAME                     
         LA    R4,24(R4)            BUMP TO SECOND BREAK NAME                   
         LHI   R1,X'15'                                                         
         BAS   RE,SENDD                                                         
         B     S55A                                                             
         DROP  R4                                                               
*                                                                               
* PASS THE PIGGYBACK PRODUCT                                                    
*                                                                               
S55A     LHI   R1,X'06'                                                         
         LA    R4,HPRD2                                                         
         CLC   0(3,R4),SPACES                                                   
         BNH   *+8                                                              
         BAS   RE,SENDD                                                         
*                                                                               
* DECODE AND PASS THE MONTH OF SERVICE                                          
*                                                                               
         MVC   HALF,HMOS                                                        
         XC    HALF,=X'FFFF'                                                    
         GOTO1 VDATCON,DMCB,(2,HALF),(X'20',WORK)  GET YYMMDD                   
         LHI   R1,X'07'                                                         
         LA    R4,WORK                                                          
         LA    R5,4                HABES WANTS YYMM                             
         BAS   RE,SENDD                                                         
*                                                                               
*  PASS THE INVOICE PAY LEVEL                                                   
*                                                                               
         MVI   BYTE,C'I'           SET FOR CALANDER                             
         TM    HSTAT,X'08'                                                      
         BO    S56                                                              
         MVI   BYTE,C'O'           SET FOR CALANDER                             
         TM    HSTAT,X'04'                                                      
         BZ    S57                                                              
S56      LHI   R1,X'13'                                                         
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
*                                                                               
* DECODE AND PASS THE ESTIMATE RANGE                                            
*                                                                               
S57      LHI   R1,X'08'                                                         
         MVC   WORK(10),SPACES                                                  
         SR    R0,R0                                                            
         ICM   R0,1,HEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         ICM   R0,1,HEST2                                                       
         BZ    S57A                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK+3,C'-'                                                      
         UNPK  WORK+4(3),DUB                                                    
*                                                                               
S57A     LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
* READ FOR POL ESTHDR AND SET POL/NON-POL FLAG                                  
* PASS THE STATUS FLAGS                                                         
*                                                                               
         XC    WORK,WORK                                                        
         USING H03FLAGD,R4                                                      
*                                                                               
         MVI   H03F_POL,C'Y'       SET DEFAULT TO POL                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),HCLT       MOVE PACKED CLIENT CODE                      
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),HEST       SET LOW EST NUMBER                           
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(8),KEYSAVE      TEST POL EST FOUND                           
         BE    *+8                 YES                                          
         MVI   H03F_POL,C'N'                                                    
*                                                                               
         MVI   H03F_STAT,C'M'      SET MATCHED                                  
         TM    HSTAT,X'80'                                                      
         BO    S58                                                              
         MVI   H03F_STAT,C'W'      SET WIP                                      
         TM    HSTAT,X'40'                                                      
         BO    S58                                                              
         MVI   H03F_STAT,C' '                                                   
*                                                                               
S58      LHI   R1,X'10'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         MVI   BYTE,C'B'           SET FOR BROADCAST                            
         TM    HSTAT,X'10'                                                      
         BZ    *+8                                                              
         MVI   BYTE,C'C'           SET FOR CALANDER                             
         LHI   R1,X'11'                                                         
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
*                                                                               
* PASS THE PIGGYBACK PRODUCT                                                    
*                                                                               
         OC    CURBUYER,CURBUYER                                                
         BZ    S59                                                              
         LHI   R1,X'14'                                                         
         LA    R4,CURBUYER                                                      
         BAS   RE,SENDD                                                         
*                                                                               
* PASS THE FILM/SECONDAY SEP ERRORS                                             
*                                                                               
S59      LHI   R1,X'16'                                                         
         MVI   BYTE,0                                                           
         TM    HSTAT2,X'20'         FILM ANALYSIS ERROR                         
         BZ    *+8                                                              
         OI    BYTE,X'20'                                                       
         TM    HSTAT2,X'08'         SEC-SEP ERROR                               
         BZ    *+8                                                              
         OI    BYTE,X'08'                                                       
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         B     S51                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*==============================================================*                
* MAKE SURE THIS USER ID IS AUTH TO THIS CLT                   *                
*==============================================================*                
         SPACE 1                                                                
AUTHCLT  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDR,R6                                                        
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,HCLT                                                     
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         CLC   KEY(13),0(R6)       HAVE THIS REC ALREADY?                       
         BE    AC10                 YES                                         
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE     MAKE SURE FOUND IT!                          
         BE    *+6                                                              
         DCHO                                                                   
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*                                                                               
AC10     MVC   SVCPROF,CPROF                                                    
*                                                                               
***         OC    TWAACCS(2),TWAACCS  TEST ANY SECURITY LIMIT                   
***         BZ    ACYES                                                         
***         CLI   TWAACCS,C'*'        TEST OFFICE LOCKOUT                       
***         BE    AC20                YES                                       
***         CLI   TWAACCS,C'+'        TEST MKT LOCKOUT                          
***         BE    AC20                YES                                       
***         CLI   TWAACCS,C'$'        TEST OFFICE LIST                          
***         BE    AC20                YES                                       
***                                                                             
***         CLC   TWAACCS(2),HCLT                                               
***         BE    ACYES                                                         
***         B     ACNO                                                          
***                                                                             
***AC20     CLI   TWAACCS,C'$'                                                  
***         BE    AC30                                                          
***         CLI   TWAACCS,C'*'                                                  
***         BNE   ACNO                                                          
***         LA    R1,CACCESS                                                    
***         LA    R0,3                                                          
***         CLI   0(R1),C' '                                                    
***         BH    *+12                                                          
***         LA    R1,COFFICE                                                    
***         LA    R0,1                                                          
***                                                                             
***AC25     CLC   TWAACCS+1(1),0(R1)                                            
***         BE    ACYES                                                         
***         LA    R1,1(R1)                                                      
***         BCT   R0,AC25                                                       
***         B     ACNO                                                          
***                                                                             
***AC30     CLI   TWAACCS,C'$'        TEST OFFICE LIST                          
***         BNE   ACYES                                                         
***         XC    DUB,DUB                                                       
***         LA    R1,DUB                                                        
***         USING OFFICED,R1                                                    
***         MVI   OFCSYS,C'S'                                                   
***         MVC   OFCAUTH,TWAACCS                                               
***         MVC   OFCAGY,QAGY                                                   
***         MVC   OFCOFC,COFFICE                                                
***         DROP  R1                                                            
***                                                                             
***         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                    
***         CLI   0(R1),0                                                       
***         BE    ACYES                                                         
***         B     ACNO                                                          
*                                                                               
*          DATA SET NENAV00    AT LEVEL 111 AS OF 07/08/02                      
*                                                                               
*  CHECK CLIENT SECURITY                                                        
*                                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2(200),WORK2                                                 
         LA    R2,WORK2                                                         
         USING OFFICED,R2                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 VCLUNPK,DMCB,(CPROF+6,CKEYCLT),OFCCLT                            
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         L     RE,ATWA                                                          
         AH    RE,=Y(SVSECRET-TWAD)                                             
         ST    RE,OFCSECD                                                       
****     MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 VOFFICER,DMCB,(C'N',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         BE    ACYES                                                            
         B     ACNO                                                             
*                                                                               
ACYES    CR    RE,RE                                                            
         B     ACX                                                              
*                                                                               
ACNO     LTR   RE,RE                                                            
*                                                                               
ACX      B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
*==============================================================*                
* USE STGRP PASSIVES TO BUILD ELEMENTS AND 'ADD' THEM BACK     *                
* INTO BUYER RECORD                                            *                
* R6 = STATION GROUP ELEM ON BUYER REC                         *                
*==============================================================*                
         SPACE 1                                                                
         USING BYRMKGD,R6                                                       
BLDSTGRP NTR1                                                                   
         XC    DUB,DUB             BUILD BYRSTA ELEMENT                         
         MVI   DUB,X'05'                                                        
         MVI   DUB+1,6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING GRPKTYP,RF                                                       
         MVC   GRPKTYP(2),=X'0D05'                                              
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,BYRMKGID                                                  
         MVC   GRPKCODE,BYRMKGRP                                                
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(6),KEYSAVE                                                   
         BNE   BMX                                                              
                                                                                
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
*  GET FIRST STATION FROM GROUP RECORD                                          
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'30',AIO1),0                        
         CLI   12(R1),0                                                         
         BNE   BMX                                                              
         L     R6,12(R1)                                                        
BM12     MVC   DUB+2(4),2(R6)                                                   
*  WRITE STATION ELEMENT TO SUPERVISOR RECORD                                   
         GOTO1 VHELLO,DMCB,(C'P',SPTFILE),(X'05',AIO2),DUB,0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RE,1(R6)             GET NEXT STATION ELEMENT                    
         AR    R6,RE                                                            
         CLI   0(R6),X'30'         ADD AFTER X'01'                              
         BE    BM12                                                             
*                                                                               
BMX      XIT1  REGS=(R6)                                                        
         EJECT                                                                  
*==============================================================*                
* BUYER ONLY GETS CLIENTS IN SUPERVISOR RECORD                 *                
* IF BUYER HAS A FILTER, MATCH TO CLIENT FILTERS IN SUPV REC   *                
* IF MKPROF+6 = Y, BUYER GETS ALL CLT'S                        *                
* CLIELEM POINTS TO CURRENT CLIENT ELEMENT                                      
*==============================================================*                
         SPACE 1                                                                
CLTFILT  NTR1                                                                   
         L     R7,CLIELEM                                                       
         USING SPVCLTD,R7                                                       
*                                                                               
CLTF4    CLI   SVBYRFLT,C' '       BUYER FILTER ACTIVE                          
         BH    CLTF5               NO - JUST MATCH CLIENT                       
         CLI   ALLSTA,C'N'          ARE STATION S INPUTEED                      
         BE    CLTFYES              THEN NO INPUT IS GOOD                       
         BE    CLTFNO               ELSE REJECT                                 
*                                                                               
CLTF5    CLI   SPVCLTLN,SPVFILT-SPVCLTEL  ELEM HAVE FILTER IN IT ?              
         BNH   CLTFYES                    NO - THEN PROCESS                     
         CLC   SVBYRFLT,SPVFILT           ELSE MATCH FILTER                     
         BE    CLTFYES                                                          
*                                                                               
* GET NEXT ELEMENT                                                              
         ZIC   RE,1(R7)                                                         
         AR    R7,RE                                                            
         CLI   0(R7),X'05'                                                      
         BE    CLTF4                                                            
         B     CLTFNO                                                           
*                                                                               
CLTFYES  ST    R7,CLIELEM                                                       
         CR    RE,RE                                                            
         B     CLTFX                                                            
*                                                                               
**********CLTFNO   LTR   RE,RE                                                  
CLTFNO   CLI   *,0                  JNEW MERGE                                  
*                                                                               
CLTFX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*==============================================================*                
* GETHEAD READS THE INV HEADERS WRITES TO TSAR                 *                
* R6 POINTS TO FIRST VALID STATION ELEMENT                     *                
* R7 POINTS TO FIRS VALID CLIENT ELEMENT                       *                
*==============================================================*                
         SPACE 1                                                                
GETHEAD  NTR1                                                                   
K        USING SNVKEYD,KEY                                                      
*                                                                               
GETH010  XC    KEY,KEY             SET TO READ SNV PASSIVE KEYS                 
         MVI   K.SNVNTYP,X'0E'                                                  
         MVI   K.SNVNSUB,X'93'                                                  
         MVC   K.SNVNAM,BAGYMD                                                  
         L     RE,CLIELEM                                                       
         OC    2(L'SNVNCLT,RE),2(RE) JNEW MERGE                                 
         BZ    GETHEX                JNEW MERGE                                 
         MVC   K.SNVNCLT,2(RE)                                                  
         CLI   ALLSTA,C'Y'                                                      
         BE    *+10                                                             
         MVC   K.SNVNNETW,2(R6)                                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,XSP+DIR+HIGH                                        
         B     GETH030                                                          
GETH020  GOTO1 AIOCALL,DMCB,XSP+DIR+SEQ                                         
*                                                                               
***      CLC   KEY+5(4),=CL3'DISH'                                              
***      BNE   *+6                                                              
***      DC    H'0'                                                             
GETH030  CLC   KEY(3),KEYSAVE       CHECK UP TO AGENCY                          
         BNE   GETHEX               EXIT                                        
         CLC   KEY(5),KEYSAVE       CHECK UP TO CLIENT                          
         BNE   GETH100              SKIP TO NEXT CLIENT                         
*                                                                               
         CLI   ALLSTA,C'Y'                                                      
         BE    *+14                                                             
         CLC   KEY(9),KEYSAVE       CHECK UP TO STATION                         
         BNE   GETH200              SKIP TO NEXT STATION                        
*                                                                               
* FILTER THE HEADER                                                             
*                                                                               
         TM    SVHDFLAG,FLSNDMAT    TEST SEND MATCHED                           
         BNZ   *+12                 YES                                         
         TM    K.SNVDSTAT+1,X'80'   TEST MATCHED                                
         BO    GETH020              YES - SKIP                                  
*                                                                               
* FILTER THE HEADER MONTH OF SERVICE                                            
*                                                                               
         OC    SVSDATE(4),SVSDATE   ARE FILTERS REQUESTED                       
         BZ    GETH050                                                          
         MVC   HALF,K.SNVNMOS                                                   
         XC    HALF,=X'FFFF'                                                    
         OC    SVSDATE,SVSDATE      CHECK START DATE FILTER                     
         BZ    *+14                                                             
         CLC   HALF,SVSDATE                                                     
         BL    GETH020                                                          
         OC    SVEDATE,SVEDATE      CHECK END DATE FILTER                       
         BZ    GETH050                                                          
         CLC   HALF,SVEDATE                                                     
         BH    GETH020                                                          
*                                                                               
* BUILD TSAR RECORD                                                             
*                                                                               
GETH050  XC    TSARREC,TSARREC                                                  
         MVC   HCLT,K.SNVNCLT                                                   
         MVC   HPRD,K.SNVNPRD                                                   
         MVC   HEST,K.SNVNEST                                                   
         MVC   HNET,K.SNVNNETW                                                  
         MVC   HMOS,K.SNVNMOS                                                   
         MVC   HPRD2,K.SNVNPRD2                                                 
         MVC   HPRDG,K.SNVNPGR                                                  
         MVC   HSTAT,K.SNVNSTAT+1                                               
         MVC   HSTAT2,K.SNVNSTAT                                                
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         B     GETH020                                                          
*                                                                               
* GET NEXT CLIENT                                                               
*                                                                               
GETH100  L     R7,CLIELEM                                                       
         ZIC   RE,1(R7)                                                         
         AR    R7,RE                                                            
         CLI   0(R7),X'05'          JNEW MERGE                                  
         BNE   GETHEX               JNEW MERGE                                  
         ST    R7,CLIELEM                                                       
         BAS   RE,CLTFILT                                                       
         BNE   GETHEX                                                           
* RESET STATION ELEMENTS                                                        
         CLI   ALLSTA,C'Y'                                                      
         BE    GETH010                                                          
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'05',AIO2),0                        
         L     R6,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,STAELEM                                                       
         B     GETH010              READ NEW KEY                                
*                                                                               
* GET THE NEXT STATION                                                          
*                                                                               
*******  L     R6,STAELEM                                                       
GETH200  ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'05'                                                      
         BNE   GETH100              END OF LIST RESET CLIENT                    
         B     GETH010              READ NEW KEY                                
*                                                                               
GETHEX   B     EXIT                                                             
         DROP  K                                                                
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARLOCL                                                   
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* READ SUPERVISOR RECORD                                          *             
*=================================================================*             
         SPACE 1                                                                
GETSPV   NTR1                                                                   
         L     RE,AIO3             WAS THIS ROUTINE ALREADY RUN                 
         CLI   0(RE),X'0D'                                                      
         BE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D61'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'         DROP MEDIA CODE                              
         MVC   KEY+3(2),QGRP       BUYING GROUP = OFFICE                        
         MVC   KEY+5(4),FULL                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETSPV2                                                          
         MVI   ERROR+1,SUPVMISS                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
GETSPV2  L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*===================================================================*           
* PASS SUPERVISOR NAME                                                          
*===================================================================*           
         SPACE 1                                                                
         OC    QSPV,QSPV           IS THIS A SUPERVISOR LEVEL REQUEST           
         BZ    GETSPV4                                                          
         USING SPVNAMED,RE                                                      
         LA    RE,SVSPVEL                                                       
         LHI   R1,X'03'                                                         
         LA    R4,SPVFNAME                                                      
         BAS   RE,SENDD                                                         
         DROP  RE                                                               
*===================================================================*           
* NEED TO PACK THE CLIENT CODES (IN PLACE)                                      
*===================================================================*           
         SPACE 1                                                                
         USING SPVCLTD,R6                                                       
GETSPV4  MVC   ELCODE,SVSPVEL                                                   
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'05',(R6)),0                        
         CLI   12(R1),0                                                         
         BNE   EXIT                                                             
         L     R6,12(R1)                                                        
         B     GETSPV10                                                         
*                                                                               
*  GET NEXT ELEMENT                                                             
GETSPV6  ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'05'                                                      
         BNE   EXIT                                                             
*                                                                               
GETSPV10 GOTO1 VCLPACK,DMCB,SPVCLT,DUB                                          
         MVC   SPVCLT(2),DUB                                                    
         MVI   SPVCLT+2,0                                                       
         B     GETSPV6                                                          
         DROP  R6                                                               
         EJECT                                                                  
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
       ++INCLUDE NENAVWRK                                                       
*                                                                               
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
CLIELEM  DS    F                                                                
STAELEM  DS    F                                                                
SVSPVEL  DS    X                                                                
SVBYREL  DS    X                                                                
ALLSTA   DS    C                                                                
ALLCLT   DS    C                                                                
LSTBUYKY DS    CL13                LAST BUY KEY (SUPERVISOR SEQUENCE)           
CURBUYER DS    CL4                 CURRENT BUYER (SUPERVISOR SEQUENCE)          
*                                                                               
TSARLOCL DS    XL(TSPXTNL)         LOCAL TSAR BLOCK                             
         EJECT                                                                  
*                                                                               
*  TSAR RECORD LAYOUT                                                           
*                                                                               
TSARFLDS DSECT                                                                  
HREC     DS    0D                                                               
*                                                                               
HKEY     DS    0XL16                                                            
HCLT     DS    XL2                                                              
HPRD     DS    XL3                                                              
HEST     DS    XL1                                                              
HNET     DS    XL4                                                              
HMOS     DS    XL2                                                              
HPRD2    DS    XL3                                                              
HEST2    DS    XL1                                                              
HPRDG    DS    XL1                                                              
HSTAT    DS    CL1                 X'80'= MATCHED                               
*                                  X'40'= WIP                                   
*                                  X'10'= CALANDER MONTH                        
*                                  X'08'= INTEGRATION+TIME INVOICE              
*                                  X'04'= INTEGRATION ONLY INVOICE              
HSTAT2   DS    CL1                                                              
*                                  X'20'= FILM ANALYSIS ERROR                   
*                                  X'08'= SECONDARY SEPERATION ERROR            
HKEYX    EQU   *                                                                
*                                                                               
         DS    CL3                                                              
HRECX    EQU   *                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGENSPV                                                       
       ++INCLUDE SPGENBYR                                                       
       ++INCLUDE SPGENOFC                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDOFFICED                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENMKG                                                       
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062NENAV30   04/04/18'                                      
         END                                                                    
