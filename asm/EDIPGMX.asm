*          DATA SET EDIPGMX    AT LEVEL 234 AS OF 02/27/14                      
*PHASE EDIPGMXA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
         TITLE 'EDIPGMX - READ A HFS DIR AND CREATE A PQ REPORT (EDI)'          
EDIPGMX  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIPGMX,=A(R13CHAIN)                                          
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         LAY   RA,COMMON                                                        
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING COMMON,RA,R9                                                     
*                                                                               
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
*                                                                               
         SAM31                                                                  
         L     R1,16(,0)           CVT - COMMON VECTOR TABLE                    
         L     R1,544(R1)          CSRTABLE                                     
         L     R1,24(R1)           CSR SLOT                                     
         MVC   BPX1OPD,BPX1OPDQ(R1)     ADDRESS OF THE SERVICE BPX1OPD          
         MVC   BPX1CLD,BPX1CLDQ(R1)     ADDRESS OF THE SERVICE BPX1CLD          
         MVC   BPX1RDD,BPX1RDDQ(R1)     ADDRESS OF THE SERVICE BPX1RDD          
         MVC   BPX1STA,BPX1STAQ(R1)     ADDRESS OF THE SERVICE BPX1STA          
         SAM24                                                                  
*                                                                               
         MVC   P(30),=CL30'** START** READCARDS'                                
         GOTO1 =V(PRINTER)                                                      
         BRAS  RE,READCRDS                                                      
         MVC   P(30),=CL30'** DONE ** READCARDS'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(30),=CL30'** START** READDIR'                                  
         GOTO1 =V(PRINTER)                                                      
         BRAS  RE,READDIR                                                       
         MVC   P(30),=CL30'** DONE ** READDIR'                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(30),=CL30'** START** GETFSTA'                                  
         GOTO1 =V(PRINTER)                                                      
         BRAS  RE,GETFSTA                                                       
         MVC   P(30),=CL30'** DONE ** GETFSTA'                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LAY   R6,FILELIST         R6=A(FILE LIST ENTRY)                        
         USING FLISTD,R6                                                        
         OC    FILECNT,FILECNT     ANY FILE TO BE SENT AT ALL?                  
         BZ    EPMXNF              NO - DON'T CREATE PQ REPORT                  
*                                                                               
         MVC   P(30),=CL30'** START** WRITE TO PQ'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVC   QLSRCID,PQUID       USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,PQREPID                                                  
         MVC   QLCLASS,PQCLASS     MUST BE CLASS "G" REPORT FOR NOW             
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=CL12'PGMX BDE'                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    EPMX20                                                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(30),=CL30'COULD NOT OPEN PRTQUE REPORT'                     
         GOTO1 =V(PRINTER)                                                      
         B     EPMXX                                                            
*                                                                               
EPMX20   DS    0H                                                               
         MVC   P(L'USERID),USERID                                               
         MVC   P+12(L'QLSUBID),QLSUBID                                          
         EDIT  QLREPRNO,(5,P+20),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EPMX30   DS    0H                                                               
         OC    FLFLEN,FLFLEN       ANY MORE FILES?                              
         BZ    EPMX90              NO - EXIT                                    
*                                                                               
         CLI   FLFTYP,FT_REGFILE   IS THIS REG FILE?                            
         BNE   EPMX80              NO - NEXT FILE                               
*                                                                               
         MVI   R,X'89'             SET SKIP TO TOP OF PAGE                      
         MVC   R+1(132),SPACES                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
*1ST RECORD: HEADER WITH EDICT RECORD KEY                                       
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+5(50),=CL50'*HDR*EDICT=????????           WP H'                
         MVC   R+16(8),EDICTKEY                                                 
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
*2ND RECORD: ++DDS TRN CARD                                                     
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(30),=CL30'++DDS SPTDATRN '                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
*3RD RECORD: ++DDS SUB CARD                                                     
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(15),=CL15'++DDS      SUB '                                   
         MVC   R+16(L'SUBJECT),SUBJECT                                          
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
*4TH RECORD: ++DDS DSN CARD                                                     
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(15),=CL15'++DDS      DSN '                                   
         MVC   R+16(L'FLFNAM),FLFNAM                                            
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
         MVC   P+1(6),=CL6'FILE ='                                              
         MVC   P+10(L'FLFNAM),R+16                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*5TH RECORD: ++DDS FIL CARD                                                     
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(15),=CL15'++DDS      FIL '                                   
         L     RE,DIRLEN           RE=LEN(DIR PATH)                             
         LA    R1,FLFNAM(RE)                                                    
         L     RF,FLFLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   R+16(0),0(R1)       MVC JUST THE FILENAME W/O DIR                
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
*6TH RECORD: ++DDS EXT CARD  (USE .NONE TO SKIP ANY EXT)                        
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(20),=CL20'++DDS      EXT .NONE'                              
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(10),=CL10'*ANYTHING*'                                        
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(30),=CL30'*** END OF DDS MESSAGE ***'                        
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   EPMXERR                                                          
*                                                                               
EPMX80   DS    0H                                                               
         AHI   R6,L'FLIST          NEXT FILE NAME                               
         B     EPMX30                                                           
         DROP  R6                                                               
*                                                                               
EPMX90   MVI   R,X'FF'             SET END OF REPORT                            
         MVC   R+1(132),SPACES                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
         MVC   P(30),=CL30'** DONE ** WRITE TO PQ'                              
         GOTO1 =V(PRINTER)                                                      
         B     EPMXX                                                            
*                                                                               
EPMXNF   MVC   P+11(13),=C'***WARNING***'                                       
         MVC   P+30(40),=CL40'NO FILE, DID NOT CREATE PQ REPORT'                
         GOTO1 =V(PRINTER)                                                      
         B     EPMXX                                                            
*                                                                               
EPMXERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EPMXX    XBASE                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GETFSTA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R6,FILELIST         R6=A(FILE LIST ENTRY)                        
         USING FLISTD,R6                                                        
*                                                                               
GFSTA20  DS    0H                                                               
         OC    FLFLEN,FLFLEN       ANY MORE FILES?                              
         BZ    GFSTA90             NO - EXIT                                    
*                                                                               
         L     RF,BPX1STA                                                       
         SAM31                                                                  
         CALL  (15),                 GET FILE STATUS                   +        
               (FLFLEN,              INPUT: PATHNAME LENGTH            +        
               FLFNAM,               INPUT: PATHNAME                   +        
               MYSTATL,              INPUT: LENGTH OF BUFFER NEEDED    +        
               MYSTAT,               BUFFER, BPXYSTAT                  +        
               RETVAL,               RETURN VALUE: 0 OR -1             +        
               RETCODE,              RETURN CODE                       +        
               RSNCODE),             REASON CODE                       +        
               VL,MF=(E,PLIST)       ----------------------------------         
         SAM24                                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL                                
         BNL   GFSTA30               BRANCH IF NEGATIVE (-1 = FAILURE)          
         MVC   P(40),=CL40'** ERROR ** GET FILE STATUS FAILED'                  
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
GFSTA30  DS    0H                                                               
         MVC   FLFTYP,MYSTAT+ST_MODE-ST_BEGIN   SAVE ST_MODE IN TABLE           
*                                                                               
         CLI   FLFTYP,FT_REGFILE   IS THIS REG FILE?                            
         BNE   GFSTA40             NO - CONTINUE                                
         L     R1,FILECNT          ADD 1 TO REG FILE COUNT                      
         AHI   R1,1                                                             
         ST    R1,FILECNT                                                       
*                                                                               
GFSTA40  DS    0H                                                               
         AHI   R6,L'FLIST          NEXT FILE NAME                               
         B     GFSTA20                                                          
         DROP  R6                                                               
*                                                                               
GFSTA90  DS    0H                                                               
*                                                                               
GFSTAX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
READDIR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   RF,IO                                                            
         ST    RF,BUFADDR                                                       
         MVC   BUFLEN,=F'2000'                                                  
         L     RF,BPX1RDD                                                       
         SAM31                                                                  
         CALL  (15),                 READ ENTRIES FROM A DIRECTORY     +        
               (DIRDES,              INPUT: DIR DESCRIPTOR FROM OPENDIR+        
               BUFADDR,              OUTPUT: ->BUFFER         BPXYDIRE +        
               ALET,                 INPUT: BUFFER ALET                +        
               BUFLEN,               INPUT: BUFFER SIZE                +        
               RETVAL,               RETURN VALUE: 0, -1, ENTRIES READ +        
               RETCODE,              RETURN CODE                       +        
               RSNCODE),             REASON CODE                       +        
               VL,MF=(E,PLIST)       ----------------------------------         
         SAM24                                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL                                
         BNL   RDIR30                BRANCH IF NEGATIVE (-1 = FAILURE)          
         MVC   P(40),=CL40'** ERROR ** READDIR FAILED'                          
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
RDIR30   L     RF,BPX1CLD                                                       
         SAM31                                                                  
         CALL  (15),                 CLOSE A DIRECTORY                 +        
               (DIRDES,              INPUT: DIR DESCRIPTOR FROM OPENDIR+        
               RETVAL,               RETURN VALUE: 0 OR -1             +        
               RETCODE,              RETURN CODE                       +        
               RSNCODE),             REASON CODE                       +        
               VL,MF=(E,PLIST)       ----------------------------------         
         SAM24                                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL                                
         BNL   RDIR50                BRANCH IF NEGATIVE (-1 = FAILURE)          
         MVC   P(40),=CL40'** ERROR ** CLOSEDIR FAILED'                         
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
RDIR50   DS    0H                  RETURN FILENAME+LEN TO A TABLE               
*                                                                               
         L     R5,DIRLEN           R5=LEN(DIR PATH)                             
         LAY   R6,FILELIST         R6=A(FILE LIST ENTRY)                        
         USING FLISTD,R6                                                        
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LAY   R1,IO               R1=A(ENTRY)                                  
         USING DIRE,R1                                                          
RDIR60   ICM   RE,3,DIRENTLEN      RE=LEN OF ENTRY                              
         BZ    RDIR90              LEN=0, EOT                                   
         ICM   RF,3,DIRENTNAML     RE=LEN OF FILE NAME                          
         BZ    RDIR90              LEN=0, EOT                                   
*                                                                               
         MVC   FLFNAM(L'DIR),DIR   MOVE IN DIR NAME                             
         LA    R8,FLFNAM(R5)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),4(R1)       APPEND FILE NAME                             
*                                                                               
         LA    R8,1(RF,R5)         R8=WHOLE FILE NAME LENGTH                    
         ST    R8,FLFLEN                                                        
*                                                                               
         CHI   R8,L'FLFNAM                                                      
         BNH   RDIR70                                                           
         MVC   P(L'FLFNAM),FLFNAM                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(40),=CL40'** ERROR ** FILE NAME TOO LONG'                      
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
RDIR70   AR    R1,RE               NEXT ENTRY                                   
         AHI   R6,L'FLIST                                                       
         B     RDIR60                                                           
         DROP  R1,R6                                                            
*                                                                               
*                                                                               
RDIR90   DS    0H                                                               
*                                                                               
RDIRX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ PARAMETER CARDS                                                          
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   DSPACE=C         DSPACE CONTROL CARD                                        
*   DDSIO=DDSIO?     DDSIO  CONTROL CARD                                        
*   USER=CL10        PQ USER ID                                                 
*   REPORT=CL3       PQ REPORT ID                                               
*   CLASS=C          PQ CLASS                                                   
*   EDICTKEY=CL8     EDICT CONTROL RECORD KEY                                   
*   SUBJECT=CL60     SUBJECT OF THE BDE FILE                                    
*   DIR=CL25         HFS DIR WHERE THE FILES WILL BE OBTAIN FROM.               
*                                                                               
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RC200                                                            
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC10                YES                                          
*                                                                               
         CLC   =C'DSPACE=',CARD    DSPACE=                                      
         BNE   RC20                                                             
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     RC10                                                             
*                                                                               
RC20     CLC   =C'DDSIO=',CARD     DDSIO=                                       
         BNE   RC30                                                             
         ICM   RF,15,=V(DDSIO)                                                  
         MVC   0(8,RF),CARD+6                                                   
         B     RC10                                                             
*                                                                               
RC30     CLC   =C'USER=',CARD      USER=                                        
         BNE   RC40                                                             
         MVC   USERID,CARD+5                                                    
         B     RC10                                                             
*                                                                               
RC40     CLC   =C'REPORT=',CARD    REPORT=                                      
         BNE   RC50                                                             
         MVC   PQREPID,CARD+7                                                   
         B     RC10                                                             
*                                                                               
RC50     CLC   =C'CLASS=',CARD     CLASS=                                       
         BNE   RC60                                                             
         MVC   PQCLASS,CARD+6                                                   
         B     RC10                                                             
*                                                                               
RC60     CLC   =C'EDICTKEY=',CARD  EDICTKEY=                                    
         BNE   RC70                                                             
         MVC   EDICTKEY,CARD+9                                                  
         B     RC10                                                             
*                                                                               
RC70     CLC   =C'SUBJECT=',CARD   SUBJECT=                                     
         BNE   RC80                                                             
         MVC   SUBJECT,CARD+8                                                   
         B     RC10                                                             
*                                                                               
RC80     CLC   =C'DIR=',CARD       DIR=                                         
         BNE   RC90                                                             
         MVC   DIR,CARD+4                                                       
         B     RC10                                                             
*                                                                               
RC90     DS    0H                                                               
         MVC   P(30),=CL30'** ERROR ** UNKNOWN PARM'                            
         B     RCERROR                                                          
*                                                                               
*                                                                               
RC200    DS    0H                                                               
         CLC   USERID,SPACES                                                    
         BH    RC210                                                            
         MVC   P(30),=CL30'MISSING USERID'                                      
         B     RCERROR                                                          
*                                                                               
RC210    CLC   PQREPID,SPACES                                                   
         BH    RC220                                                            
         MVC   P(30),=CL30'MISSING PQ REPORT ID'                                
         B     RCERROR                                                          
*                                                                               
RC220    CLC   PQCLASS,SPACES                                                   
         BH    RC230                                                            
         MVC   P(30),=CL30'MISSING PQ CLASS'                                    
         B     RCERROR                                                          
*                                                                               
RC230    CLC   EDICTKEY,SPACES                                                  
         BH    RC240                                                            
         MVC   P(30),=CL30'MISSING EDICT KEY'                                   
         B     RCERROR                                                          
*                                                                               
RC240    CLC   SUBJECT,SPACES                                                   
         BH    RC250                                                            
         MVC   P(30),=CL30'MISSING SUBJECT'                                     
         B     RCERROR                                                          
*                                                                               
RC250    CLC   SUBJECT,SPACES                                                   
         BH    RC260                                                            
         MVC   P(30),=CL30'MISSING PQ CLASS'                                    
         B     RCERROR                                                          
*                                                                               
RC260    CLI   DIR,C'/'                                                         
         BE    RC270                                                            
         MVC   P(30),=CL30'MISSING DIR'                                         
         B     RCERROR                                                          
*                                                                               
RC270    DS    0H                                                               
*                                                                               
*                                                                               
RC300    DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE NGENDIR NGENFIL X',A(IO),0                            
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),USERID   ALPHA USER-ID                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0           
         CLI   DMCB+8,0                                                         
         BE    RC310               ID RECORD FOUND                              
         MVC   P(30),=CL30'** ERROR ** USERID NOT FOUND'                        
         B     RCERROR                                                          
*                                                                               
RC310    L     RF,=A(IO)                                                        
         LA    RF,28(RF)           FIND ID ELEMENT (X'02')                      
RC320    CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                IT'S NOT THERE                               
         CLI   0(RF),X'02'                                                      
         BE    RC330                                                            
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     RC320                                                            
RC330    MVC   PQUID,2(RF)         USERID #                                     
*                                                                               
RC340    DS    0H                                                               
         LA    R5,KEY                                                           
         USING EDIKEYD,R5                                                       
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,EDICTKEY    INPUT EDICT KEY                              
*                                  FORCE TO READ FROM DISK                      
         GOTO1 =V(DATAMGR),DMCB,(X'24',=C'DMREAD'),=C'CTFILE',         +        
               (R5),A(IO),0                                                     
*                                                                               
         L     R5,=A(IO)                                                        
         CLC   EDIKEY,KEY                                                       
         BE    RC350                                                            
         MVC   P(40),=CL40'** ERROR ** EDICT KEY NOT FOUND'                     
         B     RCERROR                                                          
         DROP  R5                                                               
*                                                                               
RC350    DS    0H                                                               
         AHI   R5,28                                                            
         XR    RF,RF                                                            
         USING EDILNKD,R5                                                       
*                                                                               
RC360    CLI   EDILNKEL,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   EDILNKEL,EDILNKEQ                                                
         BE    RC370                                                            
         IC    RF,EDILNKLN                                                      
         BXH   R5,RF,RC360                                                      
*                                                                               
RC370    CLI   EDIMETHR,EDIBDFQ                                                 
         BE    RC380                                                            
         MVC   P(40),=CL40'** ERROR ** EDICT KEY CAN''T SEND BDE'               
         B     RCERROR                                                          
*                                                                               
RC380    DS    0H                                                               
         LAY   RF,DIR                                                           
         LAY   RE,DIR+L'DIR-1      FIND THE LAST '/'                            
RC382    CLI   0(RE),C'/'                                                       
         JE    RC385                                                            
         BCT   RE,RC382                                                         
*                                                                               
RC385    CLI   1(RE),C' '                                                       
         BNH   RC386                                                            
         MVC   P(40),=CL40'** ERROR ** INVALID HFS DIRECTORY'                   
         B     RCERROR                                                          
RC386    SR    RE,RF                                                            
         ST    RE,BUFLEN           LENGTH OF DIR W/O LAST '/'                   
         AHI   RE,1                                                             
         ST    RE,DIRLEN           LENGTH OF DIR WITH LAST '/'                  
*                                                                               
         MVC   IO(L'DIR),DIR                                                    
         L     RF,BPX1OPD                                                       
         SAM31                                                                  
         CALL  (15),                 OPEN A DIRECTORY                  +        
               (BUFLEN,              INPUT: DIRECTORY NAME LENGTH      +        
               IO,                   INPUT: DIRECTORY NAME             +        
               RETVAL,               RETURN VALUE:-1 OR DIRECTORY F.D. +        
               RETCODE,              RETURN CODE                       +        
               RSNCODE),             REASON CODE                       +        
               VL,MF=(E,PLIST)       ----------------------------------         
         SAM24                                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL                                
         BNL   RC388                 BRANCH IF NEGATIVE (-1 = FAILURE)          
         MVC   P(40),=CL40'** ERROR ** INVALID HFS DIRECTORY'                   
         B     RCERROR                                                          
RC388    ST    RF,DIRDES             STORE THE DIRECTORY DESCRIPTOR             
*                                                                               
*                                                                               
RCX      XIT1                                                                   
*                                                                               
RCERROR  GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
COMMON   DS    0D                                                               
         DC    CL8'**SSB***'                                                    
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
         DC    C'**UTL***'                                                      
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
FILECNT  DC    F'0'                                                             
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
USERID   DS    CL10                                                             
PQUID    DS    XL2                                                              
PQREPID  DS    CL3                                                              
PQCLASS  DS    CL1                                                              
*                                                                               
EDICTKEY DS    CL8                                                              
SUBJECT  DS    CL60                                                             
DIR      DS    CL25                LEAVE 45 BYTES ROOM FOR FILENAME             
*                                                                               
KEY      DS    CL32                FOR READS                                    
CARD     DS    CL80                                                             
WORK     DS    CL80                                                             
*                                                                               
BPX1OPDQ EQU   152                                                              
BPX1RDDQ EQU   168                                                              
BPX1CLDQ EQU   68                                                               
BPX1STAQ EQU   192                                                              
BPX1OPD  DS    A                                                                
BPX1CLD  DS    A                                                                
BPX1RDD  DS    A                                                                
BPX1STA  DS    A                                                                
*                                                                               
BUFADDR  DS    A                                                                
BUFLEN   DS    F                                                                
DIRLEN   DS    F                                                                
ALET     DC    F'0'                                                             
PLIST    DS    13A              CALL PARMLIST WORK AREA                         
RETCODE  DS    F                RETURN CODE (ERRNO)                             
RSNCODE  DS    F                REASON CODE                                     
RETVAL   DS    F                RETURN VALUE (0, -1 OR OTHER)                   
DIRDES   DS    F                DIR DESCRIPTOR                                  
MYSTATL  DC    A(STAT#LENGTH)                                                   
MYSTAT   DS    (STAT#LENGTH)X                                                   
*                                                                               
R        DS    CL133                                                            
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DC    2000X'00'           CTFILE/GENFIL I/O AREA                       
*                                                                               
         DS    0D                                                               
         DC    C'*FILES**'                                                      
FILELIST DC    (FLISTMXQ*L'FLIST)X'00'                                          
         DC    X'FFFF'                                                          
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
FLISTD   DSECT                                                                  
FLIST    DS    0XL80                                                            
FLFLEN   DS    XL4                                                              
FLFTYP   DS    X                                                                
FLFNAM   DS    CL60                                                             
FLISTMXQ EQU   200                                                              
*                                                                               
         BPXYSTAT   ,                                                           
         BPXYMODE   ,                                                           
         BPXYFTYP   ,                                                           
         BPXYDIRE   ,                                                           
*                                                                               
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'234EDIPGMX   02/27/14'                                      
         END                                                                    
