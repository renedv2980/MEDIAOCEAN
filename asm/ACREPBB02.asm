*          DATA SET ACREPBB02  AT LEVEL 004 AS OF 01/06/09                      
*PHASE ACBB02A                                                                  
*INCLUDE BUFFERIN                                                               
*INCLUDE PERVERT                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'Bucket balance for new 1C/1R and 1C/11-16 directories'          
***********************************************************************         
* Program to make sure the 1C/1R directories                          *         
*                 and the                                             *         
* 1C/11-1C/16 directories amounts match the file                      *         
***********************************************************************         
                                                                                
ACBB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBB**,R9                                                    
                                                                                
         USING ACWORKD,RA                                                       
         USING ACBBD,RC                                                         
         USING BIGPRNTD,R7                                                      
         USING PREPORT,XP                                                       
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
         L     R7,VBIGPRNT                                                      
         EJECT ,                                                                
***********************************************************************         
*  Process the modes                                                  *         
***********************************************************************         
                                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUNF000                                                          
         CLI   MODE,REQFRST                                                     
         BE    REQF000                                                          
         CLI   MODE,PROCSBAC                                                    
         BE    PSBA000                                                          
         CLI   MODE,REQLAST                                                     
         BE    REQL000                                                          
         CLI   MODE,RUNLAST                                                     
         BE    RUNL000                                                          
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  Run first                                                          *         
***********************************************************************         
RUNF000  L     RE,=A(OFFDPT)                                                    
         ST    RE,AOFFDPT                                                       
         MVI   0(RE),EOT                                                        
         XC    OFFDPT#,OFFDPT#                                                  
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  Request first                                                      *         
***********************************************************************         
         USING CPYELD,R2                                                        
REQF000  MVI   CPYINDS,0                                                        
         MVI   NEWOFF,YES                                                       
         ICM   R2,15,ADCMPEL       Company element                              
         BNZ   *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    REQF002                                                          
         TM    CPYSTATA,CPYSACCT   Accent ?                                     
         BO    REQF005             Yes                                          
                                                                                
REQF002  MVI   MODE,REQLAST        Else set mode to last                        
         B     REQFXIT             and exit                                     
                                                                                
REQF005  OI    CPYINDS,CPYACPLQ                                                 
         MVC   CPYVLOGO,CPYLOGO                                                 
                                                                                
         TM    CPYSTAT4,CPYSOFF2                                                
         BO    REQF008                                                          
         MVI   QOPT2,C' '          Turn off option no mater what                
         MVI   NEWOFF,NO                                                        
         DROP  R2                                                               
                                                                                
REQF008  MVC   MONTH#,=H'36'       Default to 36  months                        
         L     RE,AOFFDPT                                                       
         CLI   QOPT3,C'U'                                                       
         BE    REQF010                                                          
         MVI   0(RE),EOT                                                        
         XC    OFFDPT#,OFFDPT#                                                  
                                                                                
REQF010  MVI   FCSEQ,FCSEQNEW                                                   
         XC    PSTART,PSTART                                                    
         L     R5,ALEDGERS                                                      
         MVI   0(R5),EOT           Re-initailize                                
         CLI   QMTHD,C' '                                                       
         BH    *+8                                                              
         MVI   QMTHD,C'A'          Default to all methods                       
         MVC   LASTACC,SPACES                                                   
         ZAP   RECADD,=P'0'                                                     
         ZAP   RECCHG,=P'0'                                                     
         ZAP   RECDEL,=P'0'                                                     
         ZAP   MATCHS,=P'0'                                                     
         ZAP   ERRORS,=P'0'                                                     
         ZAP   ERROR1,=P'0'                                                     
         ZAP   ERROR2,=P'0'                                                     
         ZAP   ERROR3,=P'0'                                                     
         ZAP   ERROR4,=P'0'                                                     
         ZAP   ERROR9,=P'0'                                                     
         GOTOR DATCON,PARM,(4,RCDATE),(0,CTODAY)                                
         CLC   QSTART(L'QSTART+L'QEND),SPACES                                   
         BH    REQF012                                                          
         BRAS  RE,SETMOS       NO START OR END, USE CPY FISCAL YEAR             
         B     REQF032             TO GIVE 2 YEARS + CURRENT                    
                                                                                
REQF012  CLC   QSTART,SPACES                                                    
         BNH   REQF020             No start date                                
         MVC   QSTART+4(2),=C'01'                                               
         MVC   CSTART,QSTART                                                    
         GOTOR DATCON,PARM,(0,QSTART),(1,WORK)                                  
         MVC   PSTART,WORK                                                      
                                                                                
REQF020  CLC   QEND,SPACES                                                      
         BH    *+10                                                             
         MVC   QEND,CTODAY                                                      
         MVC   QEND+4(2),=C'01'                                                 
         GOTOR DATCON,PARM,(0,QEND),(1,WORK)                                    
         MVC   PEND,WORK                                                        
         CLC   QSTART,SPACES                                                    
         BNH   REQF030                    No start date                         
         GOTOR VPERVERT,PARM,QSTART,QEND                                        
         MVC   MONTH#,PARM+14             Number of months processing           
         B     REQF032                                                          
                                                                                
REQF030  GOTOR VADDAY,PARM,(C'M',QEND),CSTART,-36                               
         GOTOR DATCON,PARM,(0,CSTART),(1,PSTART)                                
                                                                                
REQF032  GOTOR VBUFFRIN,BPARM,('BUFFAINI',KEYBUFF),BUFFREC,ADCOMFAC             
                                                                                
*        XC    LDG1C,LDG1C                                                      
         XC    LDG1R,LDG1R                                                      
*        XC    LDG1R,LDG14                                                      
         GOTOR GETLDGR,PARM,=C'1R',LDG1R                                        
         GOTOR GETLDGR,PARM,=C'1C',LEVELS                                       
*        GOTOR GETLDGR,PARM,=C'1C',LDG1C                                        
                                                                                
         GOTOR GETLDGR,PARM,=C'11',LEVELS                                       
         GOTOR GETLDGR,PARM,=C'12',LEVELS                                       
         GOTOR GETLDGR,PARM,=C'13',LEVELS                                       
*        GOTOR GETLDGR,PARM,=C'14',LDG14                                        
*        GOTOR GETLDGR,PARM,=C'15',LEVELS                                       
*        GOTOR GETLDGR,PARM,=C'16',LEVELS                                       
*                                                                               
         XC    REQACCLN,REQACCLN                                                
         CLC   QACCOUNT,SPACES                                                  
         BNH   REQF040                                                          
         LA    RE,QACCOUNT+L'QACCOUNT-1                                         
         LA    RF,L'QACCOUNT                                                    
REQF036  CLI   0(RE),C' '                                                       
         BH    REQF038                                                          
         SHI   RE,1                                                             
         BCT   RF,REQF036                                                       
         DC    H'00'                                                            
                                                                                
REQF038  STH   RF,REQACCLN                                                      
                                                                                
REQF040  DS    0H                                                               
                                                                                
KEY1     USING PLCRECD,IOKEY1                                                   
DIR1     USING PLCRECD,IODIR1                                                   
                                                                                
         XC    KEY1.PLCKEY,KEY1.PLCKEY                                          
         CLI   QOPT1,OP1RONLY          Only put 1C/1R type                      
         BE    REQF200                 Yes                                      
         MVI   KEY1.PLCKTYP,PLCKTYPQ   X'18'                                    
         MVI   KEY1.PLCKSUB,PLCKSUBQ   X'CC'                                    
         MVC   KEY1.PLCKCPY,RCCOMPFL                                            
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,KEY1.PLCKEY,DIR1.PLCKEY               
                                                                                
                                                                                
REQF120  CLC   DIR1.PLCKEY(3),KEY1.PLCKEY                                       
         BNE   REQF500                   Finished with these                    
         ICM   R1,3,REQACCLN             Filter out by account                  
         BZ    REQF122                     No                                   
         BCTR  R1,0                                                             
         EXCLC R1,DIR1.PLCK1CAC,QACCOUNT                                        
         BNE   REQF180                                                          
                                                                                
REQF122  CLC   DIR1.PLCKYYMM,PSTART                                             
         BL    REQF180                                                          
         CLC   DIR1.PLCKYYMM,PEND                                               
         BH    REQF180                                                          
                                                                                
         CLI   DIR1.PLCKMTHD,C' '  Have method                                  
         BNH   REQF130               No                                         
         CLI   QMTHD,C'A'          All methods                                  
         BE    REQF130               Yes                                        
         CLC   QMTHD,DIR1.PLCKMTHD                                              
         BNE   REQF180             Skip this method                             
                                                                                
REQF130  CLI   QOPT6,C'D'          Dump DATAMGR keys                            
         BNE   REQF140                                                          
         GOTOR VPRNTBL,PARM,=C'DIR key',DIR1.PLCKEY,C'DUMP',ACCKLEN,   X        
               =C'1D',VPRINT                                                    
                                                                                
         USING BUFRECD,BUFFREC                                                  
         USING BUFFPARM,BPARM                                                   
REQF140  XC    BUFKEY(BUFKLNQ),BUFKEY                                           
         MVI   BUFRTYP,BUFRDIR                                                  
         MVC   BUFACC,DIR1.PLCK1CAC                                             
         MVC   BUFCLDG,DIR1.PLCKCLDG Contra ledger                              
         MVC   BUFCAC,DIR1.PLCKCACT                                             
         MVC   BUFOFF,SPACES                                                    
         CLI   QOPT2,OP2OFFC       Detail by office ?                           
         BNE   *+10                                                             
         MVC   BUFOFF,DIR1.PLCKAGYO                                             
*&&US*&& OC    BUFCAC,SPACES                                                    
         MVC   BUFMTHD,DIR1.PLCKMTHD                                            
         CLI   BUFCLDG,C'4'                                                     
         BNE   *+10                                                             
         MVC   BUFCAC+1(2),DIR1.PLCKAGYO                                        
                                                                                
         LA    RF,36                                                            
         LA    RE,BUFACCM                                                       
         ZAP   0(L'BUFACCM,RE),=P'0'                                            
         AHI   RE,L'BUFACCM                                                     
         BCT   RF,*-10                                                          
                                                                                
         MVC   WORK(L'PLCKYYMM),DIR1.PLCKYYMM                                   
         MVI   WORK+2,X'01'                                                     
         GOTOR DATCON,PARM,(1,WORK),(0,WORK+3)                                  
         GOTOR VPERVERT,PARM,CSTART,WORK+3                                      
         SR    R2,R2                                                            
         ICM   R2,3,PARM+14             Number of months processing             
         SHI   R2,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
         MHI   R2,L'BUFACCM                                                     
         LA    RE,BUFACCM(R2)                                                   
         ZAP   0(L'BUFACCM,RE),DIR1.PLCKAMT                                     
                                                                                
         GOTOR VBUFFRIN,BPARM,('BUFFAPUT',KEYBUFF),BUFFREC,ADCOMFAC             
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
REQF180  GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,KEY1.PLCKEY,DIR1.PLCKEY               
         B     REQF120                                                          
         DROP  KEY1,DIR1                                                        
         EJECT ,                                                                
***********************************************************************         
* Read in 1C/1R directories convert to 1C/14 buff records             *         
***********************************************************************         
                                                                                
REQF200  DS    0H                                                               
                                                                                
KEY1     USING PLDRECD,IOKEY1                                                   
DIR1     USING PLDRECD,IODIR1                                                   
                                                                                
         XC    KEY1.PLDKEY,KEY1.PLDKEY                                          
         MVI   KEY1.PLDKTYP,PLDKTYPQ   X'18'                                    
         MVI   KEY1.PLDKSUB,PLDKSUBQ   X'CD'                                    
         MVC   KEY1.PLDKCPY,RCCOMPFL                                            
         GOTOR DATAMGR,DMCB,(X'80',DMRDHI),ACCDIR,                     X        
               KEY1.PLDKEY,DIR1.PLDKEY,0                                        
                                                                                
REQF220  CLC   DIR1.PLDKEY(3),KEY1.PLDKEY                                       
         BNE   REQF500                   Finished with these                    
         ICM   R1,3,REQACCLN                                                    
         BZ    REQF222                                                          
         BCTR  R1,0                                                             
         EXCLC R1,DIR1.PLDKCACT,QACCOUNT                                        
         BNE   REQF280                                                          
                                                                                
REQF222  CLC   DIR1.PLDKYYMM,PSTART                                             
         BL    REQF280                                                          
         CLC   DIR1.PLDKYYMM,PEND                                               
         BH    REQF280                                                          
                                                                                
         CLI   DIR1.PLDKMTHD,C' '  Have method                                  
         BNH   REQF230               No                                         
         CLI   QMTHD,C'A'          All methods                                  
         BE    REQF230               Yes                                        
         CLC   QMTHD,DIR1.PLDKMTHD                                              
         BNE   REQF280             Skip this method                             
                                                                                
                                                                                
REQF230  CLI   QOPT6,C'D'                                                       
         BNE   REQF240                                                          
         GOTOR VPRNTBL,PARM,=C'DIR key',DIR1.PLDKEY,C'DUMP',ACCKLEN,   X        
               =C'1D',VPRINT                                                    
                                                                                
         USING OFDD,R2                                                          
REQF240  CLI   QOPT3,C'U'                  Update analysis values               
         BNE   REQF260                                                          
         L     R2,AOFFDPT                                                       
                                                                                
REQF241  ZIC   R3,LDG1RLV2                                                      
         BCTR  R3,0                                                             
                                                                                
REQF242  CLI   0(R2),EOT                                                        
         BE    REQF260                                                          
         CLC   OFDMTHD,DIR1.PLDKMTHD                                            
         BNE   REQF244                                                          
         EXCLC R3,OFDCODE,DIR1.PLDKRACT    Match office/department              
         BE    REQF245                                                          
REQF244  AHI   R2,OFDLNQ                                                        
         B     REQF242                                                          
                                                                                
REQF245  CLC   DIR1.PLDKANAL,OFDANAL1         Was this one I corrected          
         BE    REQF280                        Yes, so don't process             
         CLC   DIR1.PLDKANAL,OFDANAL2                                           
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   KEY1.PLDKEY,DIR1.PLDKEY        Save off key                      
         OI    DIR1.PLDKSTA,X'80'             Mark deleted                      
         AP    RECDEL,=P'1'                                                     
         CLI   RCWRITE,YES                                                      
         BNE   REQF246                                                          
         GOTOR DATAMGR,DMCB,(X'88',DMWRT),ACCDIR,                      X        
               DIR1.PLDKEY,DIR1.PLDKEY,0                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
REQF246  MVC   DIR1.PLDKANAL,OFDANAL1      Move in new analysis code            
         NI    DIR1.PLDKSTA,TURNOFF-X'80'  Un-mark deleted                      
                                                                                
*  See if record exist already                                                  
DIR2     USING PLDRECD,IODIR2                                                   
         GOTOR DATAMGR,DMCB,(X'88',DMREAD),ACCDIR,                     X        
               DIR1.PLDKEY,DIR2.PLDKEY,0                                        
         LA    R5,DMWRT            If yes then write                            
         LA    RE,RECCHG                                                        
         CLI   8(R1),0                                                          
         BE    REQF248             Record exist, so write it back               
         TM    8(R1),X'02'         Record marked deleted                        
         BO    REQF248                                                          
         LA    R5,DMADD            If not then add                              
         LA    RE,RECADD                                                        
         TM    8(R1),X'10'         Record not found                             
         BO    REQF248                                                          
         DC    H'00'               See what error is                            
         DROP  DIR2                                                             
                                                                                
*   Write back record                                                           
REQF248  AP    0(L'RECADD,RE),=P'1'                                             
         CLI   RCWRITE,YES                                                      
         BNE   REQF250                                                          
         GOTOR DATAMGR,DMCB,(X'88',(R5)),ACCDIR,                       X        
               DIR1.PLDKEY,DIR1.PLDKEY,0                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
*   Restore where we left off                                                   
REQF250  GOTOR DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,                     X        
               KEY1.PLDKEY,KEY1.PLDKEY,0                                        
         DROP  R2                                                               
                                                                                
REQF260  XC    BUFKEY(BUFKLNQ),BUFKEY                                           
         MVI   BUFRTYP,BUFRDIR                                                  
         MVC   BUFACC,DIR1.PLDKCACT                                             
         MVC   BUFOFF,SPACES                                                    
         MVI   BUFCLDG,C'4'             Direct labor                            
         MVI   BUFCAC,C'*'                                                      
         MVC   BUFANAL,DIR1.PLDKANAL    Build 14 account                        
         SR    R1,R1                                                            
         IC    R1,LDG1RLV2         office/dept                                  
         CLI   QOPT5,C'3'                                                       
         BNE   *+8                                                              
         IC    R1,LDG1RLV3         office/dept/sub-dept                         
                                                                                
         BCTR  R1,0                                                             
         EXMVC R1,BUFCAC+1,DIR1.PLDKRACT                                        
*&&US*&& OC    BUFCAC,SPACES                                                    
         MVC   BUFMTHD,DIR1.PLDKMTHD                                            
                                                                                
         LA    RF,36                                                            
         LA    RE,BUFACCM                                                       
         ZAP   0(L'BUFACCM,RE),=P'0'                                            
         AHI   RE,L'BUFACCM                                                     
         BCT   RF,*-10                                                          
                                                                                
         MVC   WORK(L'PLDKYYMM),DIR1.PLDKYYMM                                   
         MVI   WORK+2,X'01'                                                     
         GOTOR DATCON,PARM,(1,WORK),(0,WORK+3)                                  
         GOTOR VPERVERT,PARM,CSTART,WORK+3                                      
         SR    R2,R2                                                            
         ICM   R2,3,PARM+14             Number of months processing             
         SHI   R2,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
         MHI   R2,L'BUFACCM                                                     
         LA    RE,BUFACCM(R2)                                                   
         ZAP   0(L'BUFACCM,RE),DIR1.PLDKAMT                                     
                                                                                
         GOTOR VBUFFRIN,BPARM,('BUFFAPUT',KEYBUFF),BUFFREC,ADCOMFAC             
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
REQF280  GOTOR DATAMGR,DMCB,(X'80',DMRSEQ),ACCDIR,                     X        
               KEY1.PLDKEY,DIR1.PLDKEY,0                                        
         B     REQF220                                                          
         DROP  KEY1,DIR1                                                        
                                                                                
DIR1     USING CACRECD,IODIR1                                                   
REQF500  MVC   DIR1.CACKEY,SPACES                                               
         MVC   DIR1.CACKCPY,RCCOMPFL                                            
         MVC   DIR1.CACKUNT(2),=C'1C'                                           
         MVI   DIR1.CACKACT,X'41'                                               
         SR    R1,R1                                                            
         ICM   R1,3,REQACCLN                                                    
         BZ    REQF505                                                          
         BCTR  R1,0                                                             
         EXMVC R1,DIR1.CACKACT,QACCOUNT                                         
                                                                                
REQF505  GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DIR1.CACKEY,DIR1.CACKEY               
         J     REQF512                                                          
                                                                                
REQF510  GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,DIR1.CACKEY,DIR1.CACKEY               
         CLI   QOPT6,C'D'                                                       
         BNE   REQF512                                                          
         GOTOR VPRNTBL,PARM,=C'HST DIR',DIR1.CACKEY,C'DUMP',L'IODIR1,  X        
               =C'1D',VPRINT                                                    
                                                                                
REQF512  CLC   DIR1.CACKCPY,RCCOMPFL                                            
         BNE   REQFXIT                                                          
         CLC   DIR1.CACKUNT(2),=C'1C'                                           
         BNE   REQFXIT                                                          
         CLC   DIR1.CACKCACT,SPACES                                             
         BNH   REQF510                  Get next                                
         SR    R1,R1                                                            
         ICM   R1,3,REQACCLN                                                    
         BZ    REQF515                                                          
         BCTR  R1,0                                                             
         EXCLC R1,DIR1.CACKACT,QACCOUNT                                         
         BL    REQF510                                                          
         BH    REQFXIT                                                          
                                                                                
REQF515  CLC   DIR1.CACKCUNT(2),=C'13'                                          
         BH    REQF516                                                          
                                                                                
DIR1     USING TRNRECD,IODIR1                                                   
         CLI   QOPT2,OP2OFFC       Detail by office ?                           
         BNE   REQF516                                                          
         CLC   DIR1.TRNKOFF,SPACES      Office buckets only                     
         BNH   REQF510                                                          
         CLC   DIR1.TRNKREF,SPACES                                              
         BNH   REQF510                  Next, looking for transaction           
         B     REQF540                  Process transaction                     
                                                                                
DIR1     USING CACRECD,IODIR1                                                   
                                                                                
REQF516  CLC   DIR1.CACKOFF,SPACES      No office buckets                       
         BH    REQF510                                                          
                                                                                
REQF518  CLC   DIR1.CACKCUNT(2),=C'11'                                          
         BL    REQF510                                                          
         CLC   DIR1.CACKCUNT(2),=C'16'                                          
         BH    REQF510                                                          
         CLC   DIR1.CACKSPAC,SPACES                                             
         BNE   REQF510             NOT A BUCKET                                 
         CLI   DIR1.CACKBTYP,C' '                                               
         BL    REQF510             Get next                                     
         BE    REQF520             Keep going, not method to compare            
         CLI   QMTHD,C'A'                                                       
         BE    REQF520                  All methods are ok                      
         CLC   DIR1.CACKBTYP,QMTHD      Match method                            
         BNE   REQF510                  Get next                                
                                                                                
REQF520  CLC   DIR1.CACKSTYP,SPACES                                             
         BL    REQF510                                                          
         CLI   QOPT1,OP1RONLY           Process 1C/1R                           
         BNE   REQF540                                                          
         CLC   =C'14',DIR1.CACKCUNT     Yes, Only process 14                    
         BNE   REQF510                                                          
                                                                                
REQF540  MVC   ADA,DIR1.CACKDA                                                  
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,ADA,AIO1,DMWORK2                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
*        L     RF,AIO1                                                          
*        GOTOR VPRNTBL,PARM,=C'REC',(RF),C'DUMP',CACRFST-CACRECD,      X        
               =C'1D',VPRINT                                                    
         MVC   DISP2,=AL2(CACRFST-CACRECD)                                      
         L     R1,AIO1                                                          
         LR    RE,R1                                                            
         AH    RE,DISP2                                                         
         CLI   0(RE),TRNELQ        X'44', transaction record ?                  
         BE    REQF550                                                          
         GOTOR PUTBUK,(R1)                                                      
         B     REQF510             Next record                                  
                                                                                
REQF550  GOTOR PUTTRN,(R1)                                                      
         B     REQF510             Next record                                  
                                                                                
REQFXIT  B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
                                                                                
         USING CACRECD,R2                                                       
         USING BUKELD,R3                                                        
PSBA000  DS    0H                                                               
         L     R2,ADSUBAC                                                       
         CLC   CACKOFF,SPACES      No office buckets                            
         BH    PSBAXIT                                                          
         CLI   CACKCLDG,C'4'                                                    
         BL    PSBA005             No method for 11, 12 or 13                   
         CLI   CACKBTYP,C' '                                                    
         BH    PSBA005                                                          
         AP    ERROR9,=P'1'                                                     
         B     PSBAXIT             Bad contra header record probably            
                                                                                
PSBA005  CLI   QMTHD,C'A'                                                       
         BE    PSBA010                                                          
         CLC   CACKBTYP,QMTHD                                                   
         BNE   PSBAXIT                                                          
                                                                                
PSBA010  CLI   QOPT1,OP1RONLY      Process 1C/1R                                
         BNE   PSBA015             No                                           
         CLC   =C'14',CACKCUNT     Yes, Only process 14                         
         BNE   PSBAXIT                                                          
                                                                                
PSBA015  MVC   DISP2,DATADISP                                                   
         GOTOR PUTBUK,(R2)                                                      
         B     PSBAXIT                                                          
                                                                                
PSBAXIT  B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* Read and compare bufferin records                                             
***********************************************************************         
                                                                                
DIR1     USING BUFRECD,DIRREC                                                   
HST1     USING BUFRECD,HSTREC                                                   
                                                                                
REQL000  DS    0H                                                               
         TM    CPYINDS,CPYACPLQ    TEST CPY USES P&L (FOR MULTI REQS)           
         BZ    REQLXIT                                                          
         TM    CPYINDS,CPYFISRQ    TEST RUN DATES BASED ON CPY FISCAL           
         BZ    REQL005                                                          
         MVC   XHEAD2+1(9),=CL9'Period'                                         
         GOTOR DATCON,PARM,(1,PSTART),(9,XHEAD2+10)                             
         MVI   XHEAD2+16,C'-'                                                   
         GOTOR DATCON,PARM,(1,PEND),(9,XHEAD2+18)                               
         MVC   XHEAD2+25(33),=CL33'(Based on Cpy fiscal start month)'           
         MVC   SVXHEAD2,XHEAD2     SAVE FOR RE-USE LATER                        
                                                                                
REQL005  MVC   XHEAD7+10(6),=CL6'(None)'  OVERWRITTEN IF ANY ERRORS             
                                                                                
         XC    BUFKEY(BUFKLNQ),BUFKEY                                           
         MVI   COMMAND,BUFFARDH                                                 
***********************************************************************         
*        Get directory and history record pair                        *         
***********************************************************************         
                                                                                
REQL010  GOTOR VBUFFRIN,BPARM,(COMMAND,KEYBUFF),BUFFREC,ADCOMFAC                
         BE    REQL018                                                          
         TM    BUFFERRS,BUFFEEOF                                                
         BO    REQL500                                                          
         DC    H'00'                                                            
                                                                                
REQL018  MVI   COMMAND,BUFFASEQ    Sequential from now on                       
         BAS   RE,SETREC                                                        
                                                                                
REQL020  CLI   BUFRTYP,BUFRDIR                                                  
         BE    REQL030                                                          
         BAS   RE,DIRERR1          Missing Directory record                     
         B     REQL010             Try again                                    
                                                                                
REQL030  GOTOR VBUFFRIN,BPARM,(COMMAND,KEYBUFF),BUFFREC,ADCOMFAC                
         BE    REQL040                                                          
         TM    BUFFERRS,BUFFEEOF                                                
         BO    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,HSTERR1          Missing History record                       
         B     REQLXIT                                                          
                                                                                
REQL040  CLI   BUFRTYP,BUFRHST     Should be history record type                
         BE    REQL045                                                          
         MVI   BYTE,C'A'                                                        
         BAS   RE,HSTERR1          Missing History record                       
         BAS   RE,SETREC           Now overwrite last dir record                
         B     REQL030             Is directory so start again                  
                                                                                
REQL045  BAS   RE,SETREC                                                        
*                                                                               
         CLC   DIR1.BUFACC,HST1.BUFACC                                          
         BL    REQL100                                                          
         BH    REQL120                                                          
         CLC   DIR1.BUFOFF,HST1.BUFOFF                                          
         BL    REQL100                                                          
         BH    REQL120                                                          
         CLC   DIR1.BUFCLDG,HST1.BUFCLDG                                        
         BL    REQL100                                                          
         BH    REQL120                                                          
         CLC   DIR1.BUFCAC,HST1.BUFCAC                                          
         BL    REQL100                                                          
         BH    REQL120                                                          
         CLC   DIR1.BUFMTHD,HST1.BUFMTHD                                        
         BL    REQL100                                                          
         BH    REQL120                                                          
         CLI   QOPT1,OP1RONLY          Only put 1C/1R type                      
         BNE   REQL090                                                          
         CLC   DIR1.BUFANAL,HST1.BUFANAL                                        
         BE    REQL090                                                          
         MVI   BYTE,C'C'                                                        
                                                                                
         USING OFDD,RE                                                          
         L     RE,AOFFDPT                                                       
         SR    RF,RF                                                            
         IC    RF,LDG1RLV2                                                      
         BCTR  RF,0                                                             
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,OFFDPT#        Number of office/dpt                         
         BZ    REQL064             None so add                                  
                                                                                
REQL060  CLC   OFDMTHD,DIR1.BUFMTHD                                             
         BNE   REQL062                                                          
         EXCLC RF,OFDCODE,DIR1.BUFCAC+1                                         
         BNE   REQL062             Found match                                  
         CLC   HST1.BUFANAL,OFDANAL1                                            
         BNE   REQL062                                                          
         CLC   DIR1.BUFANAL,OFDANAL2                                            
         BE    REQL070                                                          
*        DC    H'00'                                                            
                                                                                
REQL062  AHI   RE,OFDLNQ                                                        
         BCT   R1,REQL060                                                       
                                                                                
REQL064  MVC   OFDCODE,SPACES           Pad with spaces                         
         EXMVC RF,OFDCODE,DIR1.BUFCAC+1                                         
         MVC   OFDANAL1,HST1.BUFANAL    Correct analysis code                   
         MVC   OFDANAL2,DIR1.BUFANAL    Wrong   analysis code                   
         MVC   OFDMTHD,DIR1.BUFMTHD                                             
         LH    R1,OFFDPT#                                                       
         AHI   R1,1                                                             
         STH   R1,OFFDPT#                                                       
         CHI   R1,100                                                           
         BNH   *+6                                                              
         DC    H'00'                                                            
         DROP  RE                                                               
                                                                                
REQL070  BAS   RE,ANLERR1                                                       
         B     REQL010                                                          
                                                                                
REQL090  BAS   RE,COMPARE                                                       
         B     REQL010             Get next pair                                
                                                                                
REQL100  MVI   BYTE,C'B'                                                        
         BAS   RE,HSTERR1          Missing history record                       
         B     REQL010             Get next pair                                
                                                                                
REQL120  BAS   RE,DIRERR1          Missing directory record                     
         B     REQL030             Try and get another history record           
                                                                                
         USING OFDD,R3                                                          
REQL500  SR    R0,R0                                                            
         ICM   R0,3,OFFDPT#                                                     
         BZ    REQL540                                                          
         MVI   FORCEHED,YES                                                     
         SR    R2,R2                                                            
         IC    R2,LDG1RLV2                                                      
         BCTR  R2,0                                                             
                                                                                
         MVI   RCSUBPRG,3                                                       
         L     R3,AOFFDPT                                                       
REQL510  MVC   XP+10(2),=C'DC'                                                  
         MVC   XP+16(11),=C'CL(OFDLNQ)'''                                       
         MVC   XP+27(OFDLNQ),OFDD                                               
         MVI   XP+27+OFDLNQ,C''''                                               
         GOTOR ACREPORT                                                         
         AHI   R3,OFDLNQ                                                        
         BCT   R0,REQL510                                                       
         DROP  R3                                                               
                                                                                
REQL540  MVI   FORCEHED,YES                                                     
         MVC   XP+1(25),=CL25'Total match pairs'                                
         OI    MATCHS+L'MATCHS-1,X'0F'                                          
         UNPK  XP+30(6),MATCHS                                                  
         GOTOR ACREPORT                                                         
                                                                                
         CLI   QOPT3,C'U'          Update                                       
         BNE   REQL550                                                          
         CP    RECADD,=P'0'                                                     
         BE    REQL542                                                          
         MVC   XP+1(25),=CL25'Number of records added'                          
         OI    RECADD+L'RECADD-1,X'0F'                                          
         UNPK  XP+30(6),RECADD                                                  
         GOTOR ACREPORT                                                         
                                                                                
REQL542  CP    RECCHG,=P'0'                                                     
         BE    REQL544                                                          
         MVC   XP+1(25),=CL25'Number of records changed'                        
         OI    RECCHG+L'RECCHG-1,X'0F'                                          
         UNPK  XP+30(6),RECCHG                                                  
         GOTOR ACREPORT                                                         
                                                                                
REQL544  CP    RECDEL,=P'0'                                                     
         BE    REQL550                                                          
         MVC   XP+1(25),=CL25'Number of records deleted'                        
         OI    RECDEL+L'RECDEL-1,X'0F'                                          
         UNPK  XP+30(6),RECDEL                                                  
         GOTOR ACREPORT                                                         
                                                                                
REQL550  CP    ERROR9,=P'0'                                                     
         BNE   *+10                                                             
         CP    ERRORS,=P'0'                                                     
         BE    REQLXIT                                                          
         MVC   XP+1(25),=CL25'Dir records missing'                              
         OI    ERROR1+L'ERROR1-1,X'0F'                                          
         UNPK  XP+30(6),ERROR1                                                  
         GOTOR ACREPORT                                                         
                                                                                
         MVC   XP+1(25),=CL25'History records missing'                          
         OI    ERROR2+L'ERROR2-1,X'0F'                                          
         UNPK  XP+30(6),ERROR2                                                  
         GOTOR ACREPORT                                                         
                                                                                
         MVC   XP+1(25),=CL25'Dir and Hst don''t match'                         
         OI    ERROR3+L'ERROR3-1,X'0F'                                          
         UNPK  XP+30(6),ERROR3                                                  
         GOTOR ACREPORT                                                         
                                                                                
         MVC   XP+1(25),=CL25'Extra Contra Headers'                             
         OI    ERROR9+L'ERROR9-1,X'0F'                                          
         UNPK  XP+30(6),ERROR9                                                  
         GOTOR ACREPORT                                                         
                                                                                
         MVC   XP+1(25),=CL25'TOTAL ERRORS FOR'                                 
         MVC   XP+18(7),CPYVLOGO                                                
         OI    ERRORS+L'ERRORS-1,X'0F'                                          
         UNPK  XP+30(6),ERRORS                                                  
         MVC   XP+40(5),=C'(1R)'                                                
         CLI   QOPT1,OP1RONLY                                                   
         JE    *+10                                                             
         MVC   XP+40(5),=C'(1C)'                                                
*&&DO                                                                           
         LA    RF,6                                                             
         LA    RE,XP+30                                                         
REQL600  CLI   0(RE),C'0'                                                       
         BH    REQL610                                                          
         MVI   0(RE),C' '                                                       
         AHI   RE,1                                                             
         BCTR  RF,REQL600                                                       
*&&                                                                             
                                                                                
REQL610  GOTOR SENDMAIL                                                         
         GOTOR ACREPORT                                                         
                                                                                
         ZAP   ERRORS,=P'0'                                                     
         ZAP   ERROR1,=P'0'                                                     
         ZAP   ERROR2,=P'0'                                                     
         ZAP   ERROR3,=P'0'                                                     
                                                                                
REQLXIT  B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
                                                                                
RUNL000  DS    0H                                                               
         GOTOR VBUFFRIN,BPARM,('BUFFACLO',KEYBUFF),BUFFREC,ADCOMFAC             
RUNLXIT  B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
                                                                                
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*        Set record into correct buffer                                         
***********************************************************************         
SETREC   ST    RE,SVRE                                                          
         LA    RE,BUFFREC                                                       
         LA    R0,DIRREC                                                        
         MVC   WORK(7),=C'Dir rec'                                              
         CLI   BUFRTYP,BUFRDIR                                                  
         BE    SETREC10                                                         
         LA    R0,HSTREC                                                        
         MVC   WORK(7),=C'Hst rec'                                              
                                                                                
SETREC10 LA    RF,BUFRLNQ                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   QOPT7,C'D'                                                       
         BNE   SETRECX                                                          
         LH    R1,MONTH#                                                        
         LA    RE,BUFFREC+(BUFACCM-BUFRECD)                                     
SETREC20 CP    0(L'BUFACCM,RE),=P'0'                                            
         BNE   SETREC30                                                         
         AHI   RE,L'BUFACCM                                                     
         BCT   R1,SETREC20                                                      
         B     SETRECX                                                          
                                                                                
SETREC30 GOTOR VPRNTBL,PARM,(7,WORK),BUFFREC,C'DUMP',BUFRLNQ,=C'1D',   X        
               VPRINT                                                           
                                                                                
SETRECX  L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* Put history record data to bufferin                                           
***********************************************************************         
         USING CACRECD,R2                                                       
PUTBUK   NTR1                                                                   
         LR    R2,R1                                                            
         CLI   QOPT7,C'D'                                                       
         BNE   PUTBUK10                                                         
         LH    RF,DISP2                                                         
         GOTOR VPRNTBL,PARM,=C'Hst Rec',(R2),C'DUMP',(RF),=C'1R',VPRINT         
                                                                                
PUTBUK10 GOTOR GETLDGR,PARM,CACKCUNT,LEVELS                                     
         SR    RF,RF                                                            
         ICM   RF,1,LEVEL2                                                      
         BNZ   *+8                                                              
         ICM   RF,1,LEVEL1                                                      
                                                                                
PUTBUK15 MVC   OFFCODE,CACKOFF                                                  
         CLI   CACKCLDG,C'3'                                                    
         BNH   PUTBUK20                                                         
         LA    RE,CACKCACT+1                                                    
         IC    RF,LDG1RLV1         Level 1 of 1R                                
         AHI   RF,1                Plus one for analysis                        
         CLI   CACKCLDG,C'6'                                                    
         BNE   *+12                                                             
         LA    RE,CACKCACT                                                      
         IC    RF,LDG1RLV2         Level 1 and 2 or 1R                          
                                                                                
         CLI   QOPT2,OP2OFFC       Detail by office ?                           
         BNE   *+10                                                             
         MVC   OFFCODE,0(RE)                                                    
                                                                                
         CLI   QOPT1,OP1RONLY                                                   
         BNE   PUTBUK20                                                         
         IC    RF,LDG1RLV2                                                      
         CLI   QOPT5,C'3'                                                       
         BNE   *+8                                                              
         IC    RF,LDG1RLV3                                                      
         AHI   RF,1                Add one for analysis                         
                                                                                
PUTBUK20 STC   RF,CONLEV                                                        
                                                                                
         XC    BUFKEY(BUFKLNQ),BUFKEY                                           
         MVI   BUFRTYP,BUFRHST                                                  
         MVC   BUFACC,CACKACT                                                   
         MVC   BUFCLDG,CACKCLDG                                                 
         MVC   BUFCAC,SPACES                                                    
         MVC   BUFOFF,OFFCODE                                                   
         ZIC   RF,CONLEV                                                        
         BCTR  RF,0                                                             
         EXMVC RF,BUFCAC,CACKCACT                                               
                                                                                
         CLI   QOPT1,OP1RONLY                                                   
         BNE   PUTBUK24                                                         
         MVC   BUFANAL,BUFCAC                                                   
         MVI   BUFCAC,C'*'                                                      
                                                                                
PUTBUK24 CLI   CACKBTYP,C'1'                                                    
         BL    *+10                                                             
         MVC   BUFMTHD,CACKBTYP                                                 
                                                                                
         LA    RF,36                                                            
         LA    RE,BUFACCM                                                       
         ZAP   0(L'BUFACCM,RE),=P'0'                                            
         AHI   RE,L'BUFACCM                                                     
         BCT   RF,*-10                                                          
                                                                                
*---------------------------------------------------------------------*         
* Fill BUCKET table with x'45' amount for account/contra by method    *         
*---------------------------------------------------------------------*         
                                                                                
         USING BUKELD,R3                                                        
PUTBUK30 LR    R3,R2                                                            
         AH    R3,DISP2                                                         
PUTBUK32 CLI   0(R3),EOR                                                        
         BE    PUTBUK50                                                         
         CLI   0(R3),BUKELQ        X'45'                                        
         BNE   PUTBUK45                                                         
         CLC   BUKMOS,PSTART       With in range                                
         BL    PUTBUK45                                                         
         CLC   BUKMOS,PEND                                                      
         BH    PUTBUK45                                                         
                                                                                
         MVC   WORK(2),BUKMOS                                                   
         MVI   WORK+2,X'01'                                                     
         GOTOR DATCON,PARM,(1,WORK),(0,WORK+3)                                  
         GOTOR VPERVERT,PARM,CSTART,WORK+3                                      
         SR    R6,R6                                                            
         ICM   R6,3,PARM+14        Number of months processing                  
         SHI   R6,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
         MHI   R6,L'BUFACCM                                                     
         LA    RE,BUFACCM(R6)                                                   
                                                                                
         ZAP   DUB,BUKDR           Contra 11 && 12 are debits                   
         SP    DUB,BUKCR                                                        
         CLI   CACKCLDG,C'3'                                                    
         BL    PUTBUK40                                                         
         ZAP   DUB,BUKCR           Contra 13, 14, 15 and 16 are credit          
         SP    DUB,BUKDR                                                        
                                                                                
PUTBUK40 ZAP   0(L'BUFACCM,RE),DUB                                              
                                                                                
PUTBUK45 ZIC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     PUTBUK32                                                         
                                                                                
PUTBUK50 CLI   QOPT7,C'K'                                                       
         BNE   PUTBUK52                                                         
         LH    RF,DISP2                                                         
         GOTOR VPRNTBL,PARM,=C'Hst Key',(R2),C'DUMP',(RF),=C'1D',VPRINT         
                                                                                
PUTBUK52 GOTOR VBUFFRIN,BPARM,('BUFFAPUT',KEYBUFF),BUFFREC,ADCOMFAC             
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
PUTBUKX  B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* Put tranaction record data to bufferin                                        
***********************************************************************         
         USING TRNRECD,R2                                                       
PUTTRN   NTR1                                                                   
         LR    R2,R1                                                            
         CLI   QOPT7,C'D'                                                       
         BNE   PUTTRN10                                                         
         LH    RF,DISP2                                                         
         GOTOR VPRNTBL,PARM,=C'Trn Rec',(R2),C'DUMP',(RF),=C'1R',VPRINT         
                                                                                
PUTTRN10 CLI   TRNKCLDG,C'1'                                                    
         BL    PUTTRNX             Bad contra ledger                            
         CLI   TRNKCLDG,C'6'                                                    
         BH    PUTTRNX             Bad contra ledger                            
         GOTOR GETLDGR,PARM,TRNKCUNT,LEVELS                                     
         SR    RF,RF                                                            
         ICM   RF,1,LEVEL2                                                      
         BNZ   *+8                                                              
         ICM   RF,1,LEVEL1                                                      
         STC   RF,CONLEV                                                        
                                                                                
         XC    BUFKEY(BUFKLNQ),BUFKEY                                           
         MVI   BUFRTYP,BUFRHST                                                  
         MVC   BUFACC,TRNKACT                                                   
         MVC   BUFCLDG,TRNKCLDG                                                 
         MVC   BUFCAC,SPACES                                                    
         MVC   BUFOFF,TRNKOFF                                                   
         ZIC   RF,CONLEV                                                        
         BCTR  RF,0                                                             
         EXMVC RF,BUFCAC,TRNKCACT                                               
                                                                                
         LA    RF,36                                                            
         LA    RE,BUFACCM                                                       
         ZAP   0(L'BUFACCM,RE),=P'0'                                            
         AHI   RE,L'BUFACCM                                                     
         BCT   RF,*-10                                                          
                                                                                
*---------------------------------------------------------------------*         
* Get MOS from x'60' and amount from x'44'                            *         
*---------------------------------------------------------------------*         
                                                                                
PUTTRN30 LR    R3,R2                                                            
         AH    R3,DISP2                                                         
         ZAP   DUB,=P'0'                                                        
                                                                                
         USING TRNELD,R3                                                        
PUTTRN32 CLI   0(R3),EOR                                                        
         BE    PUTTRNX                                                          
         CLI   0(R3),TRNELQ        X'44'                                        
         BNE   *+10                                                             
         ZAP   DUB,TRNAMNT                                                      
                                                                                
         USING TRSELD,R3                                                        
         CLI   0(R3),TRSELQ        X'60'                                        
         BNE   PUTTRN40                                                         
         CLC   TRSPMOS,PSTART      With in range                                
         BL    PUTTRN40                                                         
         CLC   TRSPMOS,PEND                                                     
         BNH   PUTTRN45                                                         
         B     PUTTRNX             Not in range                                 
                                                                                
PUTTRN40 ZIC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     PUTTRN32                                                         
                                                                                
PUTTRN45 MVC   WORK(2),TRSPMOS                                                  
         MVI   WORK+2,X'01'                                                     
         GOTOR DATCON,PARM,(1,WORK),(0,WORK+3)                                  
         GOTOR VPERVERT,PARM,CSTART,WORK+3                                      
         SR    R6,R6                                                            
         ICM   R6,3,PARM+14        Number of months processing                  
         SHI   R6,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
                                                                                
         MHI   R6,L'BUFACCM                                                     
         LA    RE,BUFACCM(R6)                                                   
         ZAP   0(L'BUFACCM,RE),DUB                                              
                                                                                
         GOTOR VBUFFRIN,BPARM,('BUFFAPUT',KEYBUFF),BUFFREC,ADCOMFAC             
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
PUTTRNX  B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* Compare history buckets and directory record totals records         *         
***********************************************************************         
COMPARE  NTR1                                                                   
         LA    R2,DIR1.BUFACCM                                                  
         LA    R3,HST1.BUFACCM                                                  
         LH    R0,MONTH#           Number of months to process                  
COMP010  CP    0(L'BUFACCM,R2),0(L'BUFACCM,R3)                                  
         BNE   COMP020                                                          
         AHI   R2,L'BUFACCM                                                     
         AHI   R3,L'BUFACCM                                                     
         BCT   R0,COMP010                                                       
         B     COMPXIT                                                          
                                                                                
COMP020  CLC   LASTACC,DIR1.BUFACC                                              
         BE    COMP030                                                          
         MVC   LASTACC,DIR1.BUFACC                                              
         MVI   FORCEHED,YES                                                     
         MVC   XHEAD6+12(L'BUFACC),DIR1.BUFACC                                  
         MVC   XHEAD7+10(36),=CL36'Dir Rec Doesn''t match buckets'              
         GOTOR HEDMNTHS                                                         
         GOTOR ACREPORT                                                         
                                                                                
COMP030  AP    ERROR3,=P'1'                                                     
         AP    ERRORS,=P'1'                                                     
         MVC   PMTHD,DIR1.BUFMTHD                                               
         MVC   POFF,DIR1.BUFOFF                                                 
         MVI   PCAC,C'1'                                                        
         MVC   PCAC+1(13),DIR1.BUFCLDG                                          
         CLI   QOPT1,OP1RONLY          Only put 1C/1R type                      
         BNE   *+10                                                             
         MVC   PCAC+2(1),DIR1.BUFANAL                                           
         MVC   PTYPE,=CL14'Dir record'                                          
         GOTOR PRTAMTS,PARM,DIR1.BUFACCM                                        
                                                                                
         MVC   PTYPE,=CL14'Bucket record'                                       
         GOTOR PRTAMTS,PARM,HST1.BUFACCM                                        
*&&DO                                                                           
***********************************************************************         
* Fix directory records                                               *         
***********************************************************************         
         CLI   QOPTX,OP4FIX                                                     
         BNE   COMPXNO                                                          
         LA    R2,DIR1.BUFACCM                                                  
         LA    R3,HST1.BUFACCM                                                  
         LH    R0,MONTH#           Number of months to process                  
         SR    R6,R6                                                            
COMP040  CP    0(L'BUFACCM,R2),0(L'BUFACCM,R3)                                  
         BNE   COMP044                                                          
         AHI   R6,1                Count which month up to                      
         AHI   R2,L'BUFACCM                                                     
         AHI   R3,L'BUFACCM                                                     
         BCT   R0,COMP010                                                       
                                                                                
DIR1     USING IODIR,PLCRECD                                                    
COMP044  XC    IODIR1,IODIR1                                                    
         MVI   PLCKTYP,PLCKTYPQ                                                 
         MVI   PLCKSUB,PLCKSUBQ                                                 
         MVC   PLCKCPY,RCCOMP                                                   
         MVC   PLCK1CAC,HST1.BUFACC                                             
         MVC   PLCKAGYO,BUFOFF                                                  
         MVC   PLCKCLDG,HST1.BUFLDG                                             
         MVC   PLCKCACT,HST1.BUFCAC                                             
         GOTOR VADDAY,PARM,(C'M',CSTART),WORK,(R6)                              
         GOTOR DATCON,PARM,(0,WORK),(1,WORK+6)                                  
         MVC   PLCKYYMM,WORK+6                                                  
         ZAP   PLCKAMT,0(L'BUFACCM,R3)                                          
         CLI   RCWRITE,YES                                                      
         BNE   COMPXNO                                                          
*&&                                                                             
         B     COMPXNO                                                          
                                                                                
COMPXIT  SR    RE,RE                                                            
         AP    MATCHS,=P'1'                                                     
COMPXNO  LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*        Print up to 3 years of amounts                                         
***********************************************************************         
                                                                                
PRTAMTS  NTR1                                                                   
         LA    R3,PACCMS                                                        
         ST    R3,FULL                                                          
         L     R4,0(,R1)           Accumlators to print                         
         LH    R5,MONTH#           Number of months to process                  
                                                                                
         LA    R0,3                Years                                        
PRTAMT10 LA    R1,12               Months                                       
                                                                                
PRTAMT12 EDIT  (P8,0(R4)),(L'PACCMS,0(R3)),2,MINUS=YES                          
         AHI   R4,L'BUFACCM                                                     
         AHI   R3,L'PACCMS                                                      
         SHI   R5,1                                                             
         BZ    PRTAMT20            No more to process                           
         SHI   R1,1                Less one month                               
         BP    PRTAMT12            Keep going                                   
         L     R3,FULL             Set of 12, reset current A(PACCMS)           
         AHI   R3,L'XP             Bump to next set of 12                       
         ST    R3,FULL                                                          
         BCT   R0,PRTAMT10                                                      
                                                                                
PRTAMT20 GOTOR ACREPORT            Print details                                
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
                                                                                
HSTERR1  NTR1                                                                   
         LA    RE,DIR1.BUFACCM                                                  
         LH    R0,MONTH#                                                        
HSTERR10 CP    0(L'BUFACCM,RE),=P'0'                                            
         BNE   HSTERR20                                                         
         AHI   RE,L'BUFACCM                                                     
         BCT   R0,HSTERR10                                                      
         B     HSTERRX             No error, no amounts                         
                                                                                
HSTERR20 CLC   LASTACC,DIR1.BUFACC                                              
         BE    HSTERR30                                                         
         MVC   LASTACC,DIR1.BUFACC                                              
         MVI   FORCEHED,YES                                                     
         MVC   XHEAD6+12(L'BUFACC),DIR1.BUFACC                                  
         MVC   XHEAD7+10(36),=CL36'Have Dir Rec, missing buckets'               
         CLI   BYTE,C'B'                                                        
         BNE   *+10                                                             
         MVC   XHEAD7+10(36),=CL36'Have Dir Rec, mis-match bucket'              
         GOTOR HEDMNTHS                                                         
         GOTOR ACREPORT                                                         
                                                                                
HSTERR30 MVC   PMTHD,DIR1.BUFMTHD                                               
         MVC   POTH,BYTE                                                        
         MVC   POFF,DIR1.BUFOFF                                                 
         MVI   PCAC,C'1'                                                        
         MVC   PCAC+1(13),DIR1.BUFCLDG                                          
         CLI   QOPT1,OP1RONLY          Only put 1C/1R type                      
         BNE   *+10                                                             
         MVC   PCAC+2(1),DIR1.BUFANAL                                           
                                                                                
HSTERR34 MVC   PTYPE,=CL14'Dir record'                                          
         GOTOR PRTAMTS,PARM,DIR1.BUFACCM                                        
*                                                                               
         CLI   BYTE,C'B'                                                        
         BNE   HSTERR40                                                         
         CLC   HST1.BUFACC,SPACES                                               
         BNH   HSTERR40                                                         
         MVC   PMTHD,HST1.BUFMTHD                                               
         MVC   POFF,HST1.BUFOFF                                                 
         MVI   PCAC,C'1'                                                        
         MVC   PCAC+1(13),HST1.BUFCLDG                                          
         MVC   PTYPE,=CL14'Hst record'                                          
         CLI   QOPT1,OP1RONLY          Only put 1C/1R type                      
         BNE   *+10                                                             
         MVC   PCAC+2(1),HST1.BUFANAL                                           
         GOTOR PRTAMTS,PARM,HST1.BUFACCM                                        
*                                                                               
HSTERR40 AP    ERROR2,=P'1'                                                     
         AP    ERRORS,=P'1'                                                     
                                                                                
HSTERRX  B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
                                                                                
ANLERR1  NTR1                                                                   
         LA    RE,DIR1.BUFACCM                                                  
         LA    RF,HST1.BUFACCM                                                  
         LH    R0,MONTH#                                                        
ANLERR10 CP    0(L'BUFACCM,RE),=P'0'                                            
         BNE   ANLERR20                                                         
         CP    0(L'BUFACCM,RF),=P'0'                                            
         BNE   ANLERR20                                                         
         AHI   RE,L'BUFACCM                                                     
         AHI   RF,L'BUFACCM                                                     
         BCT   R0,ANLERR10                                                      
         B     ANLERRX                                                          
                                                                                
ANLERR20 CLC   LASTACC,DIR1.BUFACC                                              
         BE    ANLERR30                                                         
         MVC   LASTACC,DIR1.BUFACC                                              
         MVI   FORCEHED,YES                                                     
         MVC   XHEAD6+12(L'BUFACC),DIR1.BUFACC                                  
         MVC   XHEAD7+10(36),=CL36'Analysis code mis-match'                     
         GOTOR HEDMNTHS                                                         
         GOTOR ACREPORT                                                         
                                                                                
ANLERR30 MVC   PMTHD,DIR1.BUFMTHD                                               
         MVC   POTH,BYTE                                                        
         MVC   POFF,DIR1.BUFOFF                                                 
         MVI   PCAC,C'1'                                                        
         MVC   PCAC+1(13),DIR1.BUFCLDG                                          
         MVC   PCAC+2(1),DIR1.BUFANAL                                           
         MVC   PTYPE,=CL14'Dir record'                                          
         GOTOR PRTAMTS,PARM,DIR1.BUFACCM                                        
*                                                                               
         MVC   PMTHD,HST1.BUFMTHD                                               
         MVC   POFF,HST1.BUFOFF                                                 
         MVI   PCAC,C'1'                                                        
         MVC   PCAC+1(13),HST1.BUFCLDG                                          
         MVC   PCAC+2(1),HST1.BUFANAL                                           
         MVC   PTYPE,=CL14'Hst record'                                          
         GOTOR PRTAMTS,PARM,HST1.BUFACCM                                        
*                                                                               
ANLERR40 AP    ERROR3,=P'1'                                                     
         AP    ERRORS,=P'1'                                                     
                                                                                
ANLERRX  B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* Print out account/contra buckets that weren't in bufferin           *         
***********************************************************************         
                                                                                
DIRERR1  NTR1                                                                   
         LA    RE,HST1.BUFACCM                                                  
         LH    R0,MONTH#                                                        
DIRERR10 CP    0(L'BUFACCM,RE),=P'0'                                            
         BNE   DIRERR20                                                         
         AHI   RE,L'BUFACCM                                                     
         BCT   R0,DIRERR10                                                      
         B     DIRERRX             No error, no amounts                         
                                                                                
DIRERR20 CLC   LASTACC,HST1.BUFACC                                              
         BE    DIRERR30                                                         
         MVC   LASTACC,HST1.BUFACC                                              
         MVI   FORCEHED,YES                                                     
         MVC   XHEAD6+12(L'BUFACC),HST1.BUFACC                                  
         MVC   XHEAD7+10(36),=CL36'Have Hst Rec, missing Dir Rec'               
         GOTOR HEDMNTHS                                                         
         GOTOR ACREPORT                                                         
                                                                                
DIRERR30 MVC   PMTHD,HST1.BUFMTHD                                               
         MVC   POFF,HST1.BUFOFF                                                 
         MVI   PCAC,C'1'                                                        
         MVC   PCAC+1(13),HST1.BUFCLDG                                          
         CLI   QOPT1,OP1RONLY          Only put 1C/1R type                      
         BNE   *+10                                                             
         MVC   PCAC+2(1),HST1.BUFANAL                                           
                                                                                
         MVC   PTYPE,=CL14'Hst record'                                          
         GOTOR PRTAMTS,PARM,HST1.BUFACCM                                        
         AP    ERROR1,=P'1'                                                     
         AP    ERRORS,=P'1'                                                     
                                                                                
DIRERRX  B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  Build month name headings                                          *         
***********************************************************************         
                                                                                
HEDMNTHS NTR1                                                                   
         SR    RF,RF                                                            
         IC    RF,PSTART+1         START MONTH NO.                              
         CLI   PSTART+1,X'10'                                                   
         BL    *+8                                                              
         AHI   RF,-6               X'10' -> X'0A', ETC                          
         MHI   RF,3                                                             
         LA    RF,MONTAB-3(RF)                                                  
         LH    R0,MONTH#           N'MONTHS IN REQUEST                          
         CHI   R0,12                                                            
         BNH   *+8                 12 OR LESS                                   
         LA    R0,12                                                            
         LA    RE,XHEAD7+40        RIGHT ALIGN HEADING OVER COLUMNS             
         MVC   0(3,RE),0(RF)                                                    
         LA    RE,L'PACCMS(RE)     AT THESE INTERVALS                           
         LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
                                                                                
         TM    CPYINDS,CPYFISRQ    TEST DATES BASED ON CPY FISCAL START         
         BZ    *+10                                                             
         MVC   XHEAD2,SVXHEAD2     POP DATES/TEXT INTO HEADING                  
HEDMNTHX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*  Get ledger and set level lengths, CC is set if failed              *         
***********************************************************************         
                                                                                
         USING LDGRECD,R2                                                       
GETLDGR  NTR1                                                                   
         LA    R2,IOKEY1                                                        
         L     R4,4(,R1)                                                        
         MVC   IOKEY1,SPACES                                                    
         MVC   LDGKCPY,RCCOMPFL                                                 
         L     RE,0(,R1)                                                        
         MVC   LDGKUNT(2),0(RE)                                                 
                                                                                
         L     R6,ALEDGERS         Search in table of saved values              
GETLDG04 CLI   0(R6),EOT           End of table                                 
         JE    GETLDG08                                                         
         CLC   LDGKUNT(2),0(R6)                                                 
         JE    GETLDG06                                                         
         AHI   R6,6                                                             
         J     GETLDG04                                                         
                                                                                
GETLDG06 MVC   0(4,R4),2(R6)       Move in level lengths                        
         J     GETLDGOK                                                         
                                                                                
GETLDG08 DS    0H                                                               
         GOTOR DATAMGR,DMCB,DMREAD,=C'ACCOUNT',IOKEY1,AIO1                      
         CLI   DMCB+8,0                                                         
         JNE   GETLDGNO                                                         
                                                                                
         USING ACLELD,R3                                                        
GETLDG20 L     R3,AIO1                                                          
         AH    R3,DATADISP                                                      
         SR    RF,RF                                                            
GETLDG40 CLI   0(R3),EOR                                                        
         JE    GETLDGNO                                                         
                                                                                
         CLI   0(R3),ACLELQ        X'16'                                        
         JE    GETLDG50                                                         
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     GETLDG40                                                         
                                                                                
GETLDG50 IC    RF,ACLLN                                                         
         SHI   RF,ACLLN1Q                                                       
         SRL   RF,4                Divide by 16, RF=# of levels                 
                                                                                
         LA    R5,ACLVALS                                                       
         DROP  R3                                                               
                                                                                
         USING ACLVALS,R5                                                       
         LR    R1,R4               Save off R4                                  
GETLDG60 MVC   0(1,R4),ACLVLEN     Save off level lengths                       
         AHI   R4,1                                                             
         AHI   R5,L'ACLVALS                                                     
         BRCT  RF,GETLDG60                                                      
         DROP  R5                                                               
                                                                                
         MVC   0(2,R6),LDGKUNT     Save unit/ledger code                        
         MVC   2(4,R6),0(R1)       Save off values                              
         MVI   6(R6),EOT           Mark end of table                            
         CLC   =C'1R',LDGKUNT                                                   
         JNE   GETLDGOK                                                         
         AHI   R6,6                Next entry                                   
         MVC   0(2,R6),=C'14'      14 to 16 is bases on 1R                      
         MVI   2(R6),1             Analysis level                               
         SR    RF,RF                                                            
         IC    RF,LDG1RLV1                                                      
         AHI   RF,1                                                             
         STC   RF,3(R6)                                                         
         IC    RF,LDG1RLV2                                                      
         AHI   RF,1                                                             
         STC   RF,4(R6)                                                         
         MVI   5(R6),12                                                         
         MVC   6(2,R6),=C'15'      14 and 15 are the same                       
         MVC   8(4,R6),2(R6)                                                    
         AHI   R6,6*2                                                           
         MVC   0(2,R6),=C'16'                                                   
         MVC   2(4,R6),LDG1R                                                    
         AHI   R6,6                                                             
         MVI   6(R6),EOT           Mark end of table                            
                                                                                
GETLDGOK SR    RE,RE                                                            
GETLDGNO LTR   RE,RE                                                            
         DROP  R2                                                               
         XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* SET MOS range of report based on fiscal                             *         
***********************************************************************         
         USING CPYELD,RF                                                        
SETMOS   NTR1                                                                   
         L     RF,ADCMPEL                                                       
         MVI   WORK+2,C'0'         CHAR MONTH: FIRST BYTE                       
         MVC   WORK+3(1),CPYSFST             : SECOND BYTE                      
         CLI   CPYSFST,0                                                        
         BNE   *+8                                                              
         MVI   WORK+3,C'1'         Default to JAN                               
         DROP  RF                                                               
                                                                                
         CLI   WORK+3,C'1'                                                      
         JNL   SETMOS02            MONTH IS C'1' THRU C'9'                      
         MVI   WORK+2,C'1'                                                      
         SR    R0,R0               ELSE CONVERT                                 
         IC    R0,WORK+3           FROM C'A' THRU C'C'                          
         AHI   R0,X'2F'            TO   C'10'     C'12'                         
         STC   R0,WORK+3                                                        
SETMOS02 MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+0(2),CTODAYYY  CHAR YEAR                                    
         SR    R0,R0                                                            
         AHI   R0,-2               GO BACK 2 YEARS                              
         CLC   WORK+2(2),CTODAYMM  TEST CPY FISCAL START MONTH > TODAY          
         JNH   *+8                                                              
         AHI   R0,-1               YES, GO BACK AN EXTRA YEAR                   
                                                                                
         GOTOR VADDAY,PARM,(C'Y',WORK),WORK+6,(R0)                              
         MVC   CSTART,WORK+6                                                    
         GOTOR DATCON,PARM,(0,WORK+6),(1,WORK)                                  
         MVC   PSTART,WORK         SET START MONTH                              
                                                                                
         GOTOR DATCON,PARM,(0,CTODAY),(1,PTODAY)                                
         MVC   PEND,PTODAY         SET END MONTH                                
         MVC   CEND,CTODAY                                                      
                                                                                
         GOTOR VPERVERT,PARM,CSTART,CEND                                        
         MVC   MONTH#,PARM+14      SET N'MONTHS                                 
         OI    CPYINDS,CPYFISRQ    SET DATES DERIVED FROM CPY FISCAL            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Send e-mail message of errors                                       *         
***********************************************************************         
         USING CPYELD,RF                                                        
SENDMAIL NTR1                                                                   
         CLI   QOPT4,C'N'                                                       
         BE    SENDMXIT                                                         
         MVC   WARNTO(L'MAILCALL),MAILCALL                                      
         MVC   WARNMSG2,XP                                                      
         MVC   EMSG,WARNMSG                                                     
         GOTOR DATAMGR,DMCB,=C'OPMSG',(L'EMSG,EMSG)                             
SENDMXIT XIT1                                                                   
                                                                                
MAILCALL DC    C'JSHA,DCUR,RGUP,JHIG,ABEA,GBOS,JSUL :'                          
         EJECT ,                                                                
VBUFFRIN DC    V(BUFFERIN)                                                      
VPERVERT DC    V(PERVERT)                                                       
VADDAY   DC    V(ADDAY)                                                         
VPRNTBL  DC    V(PRNTBL)                                                        
VPRINT   DC    V(PRINT)                                                         
AIO1     DC    A(IO1)                                                           
ALEDGERS DC    A(LDGRS)                                                         
                                                                                
HEXCHAR  DC    C'0123456789ABCDEF'                                              
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
MONTAB   DC    C'JanFebMarAprMayJunJulAugSepOctNovDec'                          
         DC    C'JanFebMarAprMayJunJulAugSepOctNov'  23 MONTHS DEFINED          
                                                                                
***********************************************************************         
* E-mail warning message block                                        *         
***********************************************************************         
WARNMSG  DS    0CL(L'EMSG)         SOME CONSTANT VALUES FOR E-MAIL              
WARNAUTO DC    C'AUTONOTE*'                                                     
WARNTO   DC    CL30' '             THIS IS VAR LEN, COMMA SEPARATED             
WARNMSG1 DC    CL12'ACBB report'                                                
WARNMSG2 DC    CL(L'EMSG-(*-WARNMSG))' '                                        
         EJECT ,                                                                
         LTORG                                                                  
         DROP  R9,RB                                                            
         EJECT ,                                                                
KEYBUFF  BUFFD TYPE=P,KEYLEN=BUFKLNQ,COLUMNS=36,BUFFERS=1000                    
         EJECT ,                                                                
***********************************************************************         
* Soft table work areas                                               *         
***********************************************************************         
LDGRS    DS    10XL6                                                            
IO1      DS    XL(2*K)                                                          
OFFDPT   DS    500CL(OFDLNQ)                                                    
         EJECT ,                                                                
BUFRECD  DSECT                                                                  
BUFKEY   DS    0C                                                               
BUFACC   DS    CL12                1C account                                   
BUFCLDG  DS    CL1                 Contra ledger                                
BUFCAC   DS    CL12                11-16 Contra account                         
BUFMTHD  DS    CL1                 Method                                       
BUFANAL  DS    CL1                 Analysis on 1C/1R and 1C/14                  
BUFOFF   DS    CL2                 Office code                                  
BUFBTYP  DS    CL1                 Bucket type                                  
BUFRTYP  DS    CL1                 Record type                                  
BUFRDIR  EQU   C'D'                Directory record                             
BUFRHST  EQU   C'H'                History   record                             
BUFKLNQ  EQU   *-BUFRECD                                                        
BUFACCM  DS    36PL8               Details by month                             
BUFRLNQ  EQU   *-BUFRECD                                                        
                                                                                
PREPORT  DSECT                                                                  
POTH     DS    CL1                                                              
PMTHD    DS    CL1                                                              
         DS    CL1                                                              
POFF     DS    CL2                                                              
         DS    CL1                                                              
PCAC     DS    CL14                                                             
         DS    CL1                                                              
PTYPE    DS    CL14                                                             
         DS    CL1                                                              
PACCMS   DS    12CL11                                                           
         EJECT ,                                                                
OFDD     DSECT                                                                  
OFDMTHD  DS    CL1                                                              
OFDCODE  DS    CL5                 Office/dept                                  
OFDANAL1 DS    CL1                                                              
OFDANAL2 DS    CL1                                                              
OFDLNQ   EQU   *-OFDD                                                           
         EJECT ,                                                                
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*         
* Equates                                                             *         
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*         
                                                                                
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
EOT      EQU   X'FF'                                                            
EOR      EQU   X'00'                                                            
K        EQU   1024                                                             
                                                                                
OP1RONLY EQU   C'R'                                                             
OP1CONLY EQU   C'C'                                                             
OP2OFFC  EQU   C'O'                                                             
         EJECT ,                                                                
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*         
* Application DSECT                                                   *         
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*         
                                                                                
ACBBD    DSECT                                                                  
SVRE     DS    A                                                                
AOFFDPT  DS    A                                                                
PARM     DS    6F                                                               
BPARM    DS    6F                                                               
DISP2    DS    H                   Displacement to elements                     
MONTH#   DS    H                                                                
REQACCLN DS    H                                                                
OFFDPT#  DS    H                                                                
COMMAND  DS    X                                                                
CTODAY   DS    0CL6                 Today      (EBCDIC)                         
CTODAYYY DS    CL2                                                              
CTODAYMM DS    CL2                                                              
CTODAYDD DS    CL2                                                              
PTODAY   DS    XL3                 Today      (packed)                          
CSTART   DS    CL6                 Start      (EBCDIC)                          
PSTART   DS    XL3                 Start YYMM (packed)                          
CEND     DS    CL6                 End        (EBCDIC)                          
PEND     DS    XL3                 End   YYMM (packed)                          
                                                                                
CPYINDS  DS    X                   COMPANY INDICATORS                           
CPYACPLQ EQU   X'80'               COMPANY HAS P&L RECORDS                      
CPYFISRQ EQU   X'40'               REQUEST BASED ON CPY FISCAL                  
CPYVLOGO DS    CL(L'CPYLOGO)                                                    
SVXHEAD2 DS    CL(L'XHEAD2)                                                     
ACCMFLGS DS    CL36                                                             
                                                                                
IOKEY1   DS    XL(ACCKLEN)                                                      
IODIR1   DS    XL(ACCKLEN)                                                      
IODIR2   DS    XL(ACCKLEN)                                                      
ADA      DS    XL4                                                              
DMWORK2  DS    XL96                                                             
                                                                                
BUFFREC  DS    XL(BUFRLNQ)                                                      
DIRREC   DS    XL(BUFRLNQ)         Directory type record                        
HSTREC   DS    XL(BUFRLNQ)         Directory type record                        
                                                                                
LASTACC  DS    CL12                                                             
                                                                                
LEVELS   DS    0XL4                                                             
LEVEL1   DS    XL1                                                              
LEVEL2   DS    XL1                                                              
LEVEL3   DS    XL1                                                              
LEVEL4   DS    XL1                                                              
                                                                                
*DG1C    DS    0XL4                                                             
*DG1CLV1 DS    XL1                                                              
*DG1CLV2 DS    XL1                                                              
*DG1CLV3 DS    XL1                                                              
*DG1CLV4 DS    XL1                                                              
                                                                                
LDG1R    DS    0XL4                                                             
LDG1RLV1 DS    XL1                                                              
LDG1RLV2 DS    XL1                                                              
LDG1RLV3 DS    XL1                                                              
LDG1RLV4 DS    XL1                                                              
                                                                                
*DG14    DS    0XL4                                                             
*DG14LV1 DS    XL1                                                              
*DG14LV2 DS    XL1                                                              
*DG14LV3 DS    XL1                                                              
*DG14LV4 DS    XL1                                                              
                                                                                
CONLEV   DS    XL1                 Significate level of contra                  
NEWOFF   DS    CL1                 Yes/No                                       
OFFCODE  DS    CL2                 Office code                                  
                                                                                
RECADD   DS    PL8                 Records added                                
RECCHG   DS    PL8                 Records changed                              
RECDEL   DS    PL8                 Records deleted                              
MATCHS   DS    PL6                 Total matched pairs                          
ERRORS   DS    PL6                 Total errors                                 
ERROR1   DS    PL6                 Missing directory record                     
ERROR2   DS    PL6                 Missing history record                       
ERROR3   DS    PL6                 Buckets didn't match                         
ERROR4   DS    PL6                                                              
ERROR9   DS    PL6                                                              
EMSG     DS    CL110               E-mail message to LOTUS NOTES                
ACBBLNQ  EQU   *-ACBBD                                                          
         EJECT ,                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACBIGPRNTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*DDBUFFD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPBB02 01/06/09'                                      
         END                                                                    
