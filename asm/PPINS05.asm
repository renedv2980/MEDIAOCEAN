*          DATA SET PPINS05    AT LEVEL 036 AS OF 09/02/14                      
*PHASE T41F05A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41F05 - INSERTION ORDERS 2, WEB IO ROUTINES'                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 06/19/14 VENDOR CONTACT LIST - HONOR ALL PUB LEVEL                       
*                                                                               
* KWAN 07/08/11 EIO BY PERIOD FIX - DATE RANGE IS LESS THAN 30 DAYS             
*                                                                               
* KWAN 06/01/11 DATE RANGE PASSIVE KEY FIX FOR EIO BY EST OR BY PERIOD          
*                                                                               
* KWAN 05/18/11 REQUEST DATES NOT STARTING 1ST & LAST DAY OF MONTH FIX          
*                                                                               
* KWAN 03/21/11 EIO BY ESTIMATE PERIOD                                          
*                                                                               
* KWAN 11/25/08 AB2 PROFILE FOR PUB E-MAILS AND FAXES                           
*                                                                               
* KWAN 01/09/08 AGENCY CONTACT LIST CORRECTION FOR ALL PUB                      
*                                                                               
* KWAN 03/01/07 IO# CHANGES FOR EIO BY ESTIMATE                                 
*                                                                               
* KWAN 05/16/06 IO# CHANGES FOR STEWARD INSERTION ORDERS                        
*                                                                               
* KWAN 07/20/05 SUPPRESS COST OPTION FROM ACC AND VCC RECORDS                   
*                                                                               
* KWAN 06/28/05 DON'T DUMP IS FAX CODE IS INVALID                               
*                                                                               
* KWAN 05/25/05 ADD PUB INFO TO VENDOR CONTACT LIST                             
*                                                                               
* KWAN 11/08/04 IGNORE SERIAL# IN TABLE IF NO COUNTER PRESENT                   
*                                                                               
* KWAN 07/22/04 CREATE WEB IO RECORD FROM DATA STREAM                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T41F05   CSECT                                                                  
*                                                                               
         NMOD1 PPINS05X-PPINS05D,T41F05,RR=RE,CLEAR=YES                         
*                                                                               
         LR    R9,RC                                                            
         USING PPINS05D,R9                                                      
*                                                                               
         ST    RE,RELO05                                                        
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         LR    R8,RC                                                            
         A     R8,=A(POLWRKX-POLWRKD)                                           
         USING IOWORKD,R8                                                       
         USING T41FFFD,RA                                                       
*                                                                               
         BRAS  RE,INITWKST                                                      
*                                                                               
         CLI   8(R1),ACCLISTQ      AGENCY CONTACT LIST?                         
         BE    ACCLISTR                                                         
         CLI   8(R1),VCCLISTQ      VENDOR CONTACT LIST?                         
         BE    VCCLISTR                                                         
         CLI   8(R1),SETUPRCQ      SETUP RECORD?                                
         BE    SETUPRCR                                                         
         CLI   8(R1),PRCWIO#Q      WEB IO NUMBER AND REVISION?                  
         BE    PRCWIO#R                                                         
         CLI   8(R1),PUTWIO#Q      WRITE BACK WEB IO RECORD?                    
         BE    PUTWIO#R                                                         
*                                                                               
         DC    H'0'                INVALID CALL                                 
*                                                                               
ACCLISTR BRAS  RE,GETACCLS         GET FYI (AGENCY) CONTACT LIST                
         B     EXXMODX                                                          
*                                                                               
VCCLISTR BRAS  RE,GETVCCLS         GET VENDOR CONTACT LIST                      
         B     EXXMODX                                                          
*                                                                               
SETUPRCR BRAS  RE,GETSETUP         GET SETUP RECORD                             
         B     EXXMODX                                                          
*                                                                               
PRCWIO#R BRAS  RE,GETWIO#R         GET WEB IO NUMBER AND REVISION               
         B     EXXMODX                                                          
*                                                                               
PUTWIO#R BRAS  RE,WRIWIO#R         WRITE WEB IO RECORD                          
         B     EXXMODX                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXXMODX  XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTEL                                                            
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WKSVMODE,8(R1)      SAVE CALLING MODE                            
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CMINIO-COMFACSD)(RF)                                         
         ST    RF,VMINIO                                                        
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO1-PPINS05D)                                           
         ST    RE,AWKAIO1                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO2-PPINS05D)                                           
         ST    RE,AWKAIO2                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO3-PPINS05D)                                           
         ST    RE,AWKAIO3                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO4-PPINS05D)                                           
         ST    RE,AWKAIO4                                                       
*                                                                               
         OC    QSTART,QSTART       DATES ARE SET?                               
         BZ    INITWKX                                                          
         GOTOR DATCON,DMCB,(0,QSTART),(3,QPERSTR)                               
         GOTOR DATCON,DMCB,(0,QEND),(3,QPEREND)                                 
         GOTOR DATCON,DMCB,(0,QSTART),(2,QPERCSTR)                              
         GOTOR DATCON,DMCB,(0,QEND),(2,QPERCEND)                                
*                                                                               
INITWKX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETACCLS NTR1  BASE=*,LABEL=*       GET FYI (AGENCY) CONTACT LIST               
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVDMIN,DMINBTS                                                 
         NI    DMINBTS,X'FF'-X'08' NO DELETED RECORDS                           
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CCAKEY,R2                                                        
         MVC   CCAKAGY,QAGENCY      CK FOR MED/CLT/PRD/PUB                      
         MVC   CCAKMED,QMEDIA                                                   
         MVI   CCAKRCD,CCAKRCDQ                                                 
         MVC   CCAKCLT,QCLIENT                                                  
         MVC   CCAKPRD,QPRODUCT                                                 
         MVC   CCAKPUB,BPUB                                                     
         MVC   WKTMP1(L'KEY),KEY                                                
*                                                                               
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          MED/CLT/PRD/ALL PUB (X'FF')                  
         MVC   CCAKPUB,=6X'FF'                                                  
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
         MVC   KEY,WKTMP1          MED/CLT/PRD/ALL PUB (X'00')                  
         XC    CCAKPUB,CCAKPUB                                                  
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          CK MED/CLT/ALL PRD/PUB                       
         MVC   CCAKPRD,=6X'FF'                                                  
         MVC   CCAKPUB,BPUB                                                     
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          MED/CLT/ALL PRD/ALL PUB (X'FF')              
         MVC   CCAKPRD,=6X'FF'                                                  
         MVC   CCAKPUB,=6X'FF'                                                  
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
         MVC   KEY,WKTMP1          MED/CLT/ALL PRD/ALL PUB (X'00')              
         MVC   CCAKPRD,=6X'FF'                                                  
         XC    CCAKPUB,CCAKPUB                                                  
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          CK MED/ALL CLT/ALL PRD/PUB                   
         MVC   CCAKCLT,=6X'FF'                                                  
         MVC   CCAKPRD,=6X'FF'                                                  
         MVC   CCAKPUB,BPUB                                                     
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          MED/ALL CLT/ALL PRD/ALL PUB (X'FF')          
         MVC   CCAKCLT,=6X'FF'                                                  
         MVC   CCAKPRD,=6X'FF'                                                  
         MVC   CCAKPUB,=6X'FF'                                                  
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
         MVC   KEY,WKTMP1          MED/ALL CLT/ALL PRD/ALL PUB (X'FF')          
         MVC   CCAKCLT,=6X'FF'                                                  
         MVC   CCAKPRD,=6X'FF'                                                  
         XC    CCAKPUB,CCAKPUB                                                  
         GOTOR HIGH                                                             
         CLC   KEY(CCAKPID-CCAKEY),KEYSAVE                                      
         BNE   GTACCX                                                           
*                                                                               
GTACC30  BRAS  RE,RPYCCLST         REPLY CONTACT LIST                           
*                                                                               
GTACCX   MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
         MVC   DMINBTS,WKSVDMIN                                                 
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETVCCLS NTR1  BASE=*,LABEL=*      GET VENDOR CONTACT LIST                      
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING CCVKEY,RE                                                        
         MVC   CCVKMED,QMEDIA                                                   
         MVC   CCVKPUB,BPUB                                                     
         MVC   CCVKAGY,QAGENCY                                                  
         MVI   CCVKRCD,CCVKRCDQ                                                 
         MVC   CCVKCLT,QCLIENT                                                  
         MVC   CCVKPRD,QPRODUCT                                                 
*                                                                               
         GOTOR HIGHPB                                                           
         CLC   KEY(CCVKPID-CCVKEY),KEYSAVE                                      
         BE    GTVCC30                                                          
*                                                                               
         LA    RE,KEY              CK FOR ALL PRODUCT                           
         XC    KEY,KEY                                                          
         MVC   CCVKMED,QMEDIA                                                   
         MVC   CCVKPUB,BPUB                                                     
         MVC   CCVKAGY,QAGENCY                                                  
         MVI   CCVKRCD,CCVKRCDQ                                                 
         MVC   CCVKCLT,QCLIENT                                                  
         MVC   CCVKPRD,=X'FFFFFF'                                               
         GOTOR HIGHPB                                                           
         CLC   KEY(CCVKPRD-CCVKEY),KEYSAVE                                      
         BE    GTVCC30                                                          
*                                                                               
         LA    RE,KEY              CK FOR ALL CLIENT                            
         XC    KEY,KEY                                                          
         MVC   CCVKMED,QMEDIA                                                   
         MVC   CCVKPUB,BPUB                                                     
         MVC   CCVKAGY,QAGENCY                                                  
         MVI   CCVKRCD,CCVKRCDQ                                                 
         MVC   CCVKCLT,=X'FFFFFF'                                               
         GOTOR HIGHPB                                                           
         CLC   KEY(CCVKCLT-CCVKEY),KEYSAVE                                      
         JE    GTVCC30                                                          
*                                                                               
         LA    RE,KEY              CK FOR ALL PUB, CLT LEVEL                    
         XC    KEY,KEY                                                          
         MVC   CCVKMED,QMEDIA                                                   
         MVC   CCVKPUB,=6X'FF'                                                  
         MVC   CCVKAGY,QAGENCY                                                  
         MVI   CCVKRCD,CCVKRCDQ                                                 
         MVC   CCVKCLT,QCLIENT                                                  
         GOTOR HIGHPB                                                           
         CLC   KEY(CCVKPRD-CCVKEY),KEYSAVE                                      
         JE    GTVCC30                                                          
*                                                                               
         JNE   GTVCC50                                                          
*                                                                               
GTVCC30  BRAS  RE,RPYCCLST         REPLY CONTACT LIST                           
*                                                                               
GTVCC50  MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
*                                                                               
         BRAS  RE,RPYRPINF         REPLY REP/PUB INFO                           
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RE                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYRPINF NTR1  BASE=*,LABEL=*      REPLY REP/PUB INFORMATION                    
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         CLC   LIOBPCV1,=X'03020011'                                            
         JL    EXIT                                                             
*                                                                               
         LR    R6,R8                                                            
         A     R6,=A(IOWORKX-IOWORKD)                                           
         LA    R7,1(R6)                                                         
         LA    R7,4095(R7)                                                      
         USING POLFILE,R6,R7                                                    
*                                                                               
         XC    WORK,WORK           GET PROFILE                                  
         MVC   WORK(4),=C'PAB2'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,WKTMP1,VDATAMGR                               
         CLI   WKTMP1+2,C'S'                                                    
         JE    EXIT                NO NEED TO REPLY PUB INFO                    
         DROP  RF                                                               
*                                                                               
         L     R4,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,R4           WEB IO STORAGE BLOCK                         
*                                                                               
         CLC   MYFAX,SPACES        FAX NUMBER PRESENT?                          
         BNH   RPINF30                                                          
         MVC   WKRPNAM,SPACES                                                   
         MVC   WKRPNAM(L'RPUB_NAM),RPUB_NAM                                     
         MVI   WKRPNAML,L'RPUB_NAM                                              
         MVI   WKRPTYP,C'F'                                                     
         MVC   WKRPADR(L'MYFAX),MYFAX                                           
         MVI   WKRPADRL,L'MYFAX                                                 
         BRAS  RE,CKCTFFAX                                                      
         BE    RPINF26                                                          
         MVC   WKRPADR(08),=C'ERROR - '                                         
         MVC   WKRPADR+08(L'CTFXCODE),MYFAX+3                                   
         MVI   WKRPADRL,08+L'CTFXCODE                                           
RPINF26  BRAS  RE,RPINF_LK                                                      
*                                                                               
RPINF30  OC    RPUB_EML,SPACES                                                  
         CLC   RPUB_EML,SPACES     HAVE E-MAIL ADDRESS?                         
         BNH   RPINF90                                                          
         MVC   WKRPNAM,SPACES                                                   
         MVC   WKRPNAM(L'RPUB_NAM),RPUB_NAM                                     
         MVI   WKRPNAML,L'RPUB_NAM                                              
         MVI   WKRPTYP,C'E'                                                     
         MVC   WKRPADR(L'RPUB_EML),RPUB_EML                                     
         MVI   WKRPADRL,L'RPUB_EML                                              
         BRAS  RE,RPINF_LK                                                      
*                                                                               
RPINF90  J     EXIT                                                             
*                                                                               
RPINF_LK ST    RE,WKFULL2                                                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#VNDCON)              
         SR    RF,RF                                                            
         IC    RF,WKRPNAML                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTNAME),    +        
               ('LD_CHARQ',WKRPNAM),((RF),0)                                    
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_1),    +        
               ('LD_CHARQ',WKRPTYP),(L'WKRPTYP,0)                               
         SR    RF,RF                                                            
         IC    RF,WKRPADRL                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTADDR),    +        
               ('LD_CHARQ',WKRPADR),((RF),0)                                    
         L     RE,WKFULL2                                                       
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R7,R4,R3                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCTFFAX NTR1  BASE=*,LABEL=*      CKING FOR FAX CODE ON CONTROL FILE           
*                                                                               
         CLC   WKRPADR(3),=C'FX='                                               
         JNE   SETCCEQ                                                          
*                                                                               
         MVC   CKCTFWK,KEY         SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGYALPHA                                                 
         MVC   CTFXCODE,WKRPADR+3                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,CKCTFIO               
*                                                                               
         MVC   KEY,CKCTFWK         RESTORE KEY                                  
*                                                                               
         CLC   CKCTFIO(18),KEYSAVE                                              
         JNE   SETCCNEQ            FAX CODE IS NOT ON FILE                      
         LA    R4,CKCTFIO                                                       
         LA    R6,CTFXEL1                                                       
         ST    R6,WKFULL2                                                       
         B     CKCTF40                                                          
*                                                                               
CKCTF20  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
CKCTF40  CLI   0(R6),0                                                          
         JE    SETCCNEQ            FAX CODE IS NOT ON FILE                      
         CLI   0(R6),CTFX1ELQ                                                   
         BNE   CKCTF20                                                          
         XC    WKRPADR,WKRPADR                                                  
         SR    RE,RE                                                            
         IC    RE,CTFX1LEN                                                      
         SHI   RE,2+1                                                           
         CHI   RE,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKRPADR(0),CTFX1NUM                                              
         AHI   RE,1                                                             
         STC   RE,WKRPADRL                                                      
*                                                                               
         L     R6,WKFULL2          NOW LOOK FOR ATTENTION NAME                  
         USING CTFXATT,R6                                                       
         B     *+12                                                             
CKCTF60  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    CKCTF90             ATTENTION NAME NOT FOUND                     
         CLI   0(R6),CTFX2ELQ                                                   
         BNE   CKCTF60                                                          
         XC    WKRPNAM,WKRPNAM                                                  
         SR    RE,RE                                                            
         IC    RE,CTFX2LEN                                                      
         SHI   RE,2+1                                                           
         CHI   RE,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKRPNAM(0),CTFX2ATT                                              
         AHI   RE,1                                                             
         STC   RE,WKRPNAML                                                      
*                                                                               
CKCTF90  J     SETCCEQ             GOT FAX NUMBER FROM FAX CODE                 
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETSETUP NTR1  BASE=*,LABEL=*      GET SETUP RECORD                             
*                                                                               
         L     R4,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,R4                                                        
         MVC   H_TIMACC,=X'010000' DEFAULT RESPONSE PERIOD TO 1 DAY             
*                                                                               
GTSUP80  L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         MVC   WKBYTE1,H_TIMACC+0                                               
         EDIT  (B1,WKBYTE1),(2,WKTMP1),0,ALIGN=LEFT,FILL=0                      
         MVC   WKTMP1+2(4),=C'0000'                                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RSPPER),    +        
               ('LD_CHARQ',WKTMP1),(6,0)                                        
*                                                                               
GTSUP_X  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RDSETUPR NTR1  BASE=*,LABEL=*      RETRIEVE & SAVE SETUP REC INFO               
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVAREC,AREC                                                    
         XC    SVEACMOS,SVEACMOS                                                
         MVI   SVEIOESW,0                                                       
         XC    KEY,KEY             CK SETUP RECORD AT CLIENT LEVEL              
         LA    R2,KEY                                                           
         USING SCHKKEY,R2                                                       
         MVC   SCHKAGY,QAGENCY                                                  
         MVC   SCHKMED,QMEDIA                                                   
         MVI   SCHKRCD,SCHKRCDQ                                                 
         MVC   SCHKCLT,QCLIENT                                                  
         GOTOR HIGH                                                             
         CLC   KEY(L'SCHKKEY),KEYSAVE                                           
         BE    RDSETR30                                                         
         XC    KEY,KEY             CK SETUP RECORD FOR ALL CLIENT               
         MVC   SCHKAGY,QAGENCY                                                  
         MVC   SCHKMED,QMEDIA                                                   
         MVI   SCHKRCD,SCHKRCDQ                                                 
         GOTOR HIGH                                                             
         CLC   KEY(L'SCHKKEY),KEYSAVE                                           
         BNE   RDSETR_X                                                         
*                                                                               
RDSETR30 MVC   AREC,AWKAIO1                                                     
         GOTOR GETPRT                                                           
         L     R5,AWKAIO1                                                       
         LA    R5,(SCHFIRST-SCHREC)(R5)                                         
         USING SCHHDRD,R5                                                       
         CLI   SCHHDRCD,SCHHDRQ                                                 
         BNE   RDSETR_X                                                         
         MVC   SVEACMOS,SCHEACD    EIO BY EST ACTIVATION DATE                   
         MVC   SVEIOESW,SCHEIOE    C'Y' - ALLOW EIO BY EST                      
*                                                                               
RDSETR_X MVC   KEY,WKSVKEY         RESTORE ORIGINAL KEY AND AIO                 
         MVC   AREC,WKSVAREC                                                    
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R5                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NOTE: VENDOR AND AGENCY CONTACT ELEMS ARE IDENTICAL                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYCCLST NTR1  BASE=*,LABEL=*      REPLY CONTACT LIST                           
*                                                                               
         MVC   WKSVAREC,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         CLI   WKSVMODE,ACCLISTQ   DOING AGENCY CONTACT LIST?                   
         BE    RPYCLS12                                                         
         CLI   WKSVMODE,VCCLISTQ   DOING VENDOR CONTACT LIST?                   
         BE    RPYCLS10                                                         
         DC    H'0'                INVALID CALLING MODE                         
RPYCLS10 GOTOR GETPUB                                                           
         B     RPYCLS14                                                         
RPYCLS12 GOTOR GETPRT                                                           
RPYCLS14 MVC   AREC,WKSVAREC       RESTORE PREVIOUS AREC POINTER                
*                                                                               
         L     R5,AWKAIO1          POINT TO CONTACT RECORD                      
         LA    R5,CCVFIRST-CCVREC(R5)                                           
         CLI   0(R5),CCVEBIDQ                                                   
         BE    *+6                                                              
         DC    H'0'                BASE ELEM IS NOT THERE                       
         USING CCVEBELM,R5                                                      
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         SR    RE,RE                                                            
         IC    RE,CCVEBLEN                                                      
         SHI   RE,CCVEBLQ          MINUS OVERHEAD                               
         STC   RE,WKBYTE1          SAVE LENGTH OF E-MAIL BASE                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP1(0),CCVEBASE  SAVE E-MAIL BASE                             
*                                                                               
         MVI   ELCODE,CCVLIDQ      VENDOR CONTACT LIST ELEM CODE                
RPYCLS30 BRAS  RE,NXTEL                                                         
         BNE   RPYCLSX                                                          
         USING CCVLELM,R5                                                       
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         LHI   RF,E#AGYCON                                                      
         CLI   WKSVMODE,VCCLISTQ   VENDOR CONTACT LIST?                         
         BNE   *+8                                                              
         LHI   RF,E#VNDCON                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(RF))                  
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTNAME),    +        
               ('LD_CHARQ',CCVLNAME),(L'CCVLNAME,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_1),    +        
               ('LD_CHARQ',CCVLTYPE),(L'CCVLTYPE,0)                             
*                                                                               
         CLI   CCVLTYP2,0                                                       
         BE    RPYCLS34                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_2),    +        
               ('LD_CHARQ',CCVLTYP2),(L'CCVLTYP2,0)                             
*                                                                               
RPYCLS34 XC    WKTMP2,WKTMP2                                                    
*                                                                               
         CLI   CCVLTYPE,C'F'       FAX?                                         
         BE    RPYCLS40                                                         
         SR    RF,RF                                                            
         IC    RF,CCVLLEN                                                       
         SHI   RF,CCVLSTLQ         MINUS OVERHEAD                               
         LR    R0,RF                                                            
         LA    RE,CCVLADDR                                                      
RPYCLS36 CLI   0(RE),C'@'                                                       
         BE    RPYCLS38            FOUND @, NO NEED TO USE E-MAIL BASE          
         LA    RE,1(RE)            POINT TO NEXT CHARACTER                      
         BCT   RF,RPYCLS36                                                      
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP2(0),CCVLADDR                                               
         LA    RE,WKTMP2                                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'@'                                                       
         AHI   R0,1                                                             
         SR    RF,RF                                                            
         IC    RF,WKBYTE1          LENGTH OF E-MAIL BASE                        
         AR    R0,RF               ADD IT TO CREATE LENGTH                      
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WKTMP1      GET SAVED E-MAIL BASE                        
         LR    RF,R0               TOTAL LENGTH                                 
         B     RPYCLS60                                                         
*                                                                               
RPYCLS38 LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP2(0),CCVLADDR                                               
         LR    RF,R0               TOTAL LENGTH                                 
         B     RPYCLS60                                                         
*                                                                               
RPYCLS40 SR    RE,RE                                                            
         IC    RE,CCVLLEN                                                       
         SHI   RE,CCVLSTLQ         MINUS OVERHEAD                               
         LR    RF,RE               LENGTH OF FAX NUMBER                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP2(0),CCVLADDR                                               
*                                                                               
RPYCLS60 GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTADDR),    +        
               ('LD_CHARQ',WKTMP2),((RF),0)                                     
*                                                                               
         CLC   LIOBPCV1,=X'03030002'                                            
         BL    RPYCLS70                                                         
         CLI   CCVLSUP,C'Y'        SUPPRESS COST?                               
         BNE   RPYCLS70                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SUPCOS),    +        
               ('LD_CHARQ',CCVLSUP),(L'CCVLSUP,0)                               
*                                                                               
RPYCLS70 DS    0H                                                               
*                                                                               
         B     RPYCLS30            NEXT VENDOR CONTACT LIST ELEM                
*                                                                               
RPYCLSX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R5                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETWIO#R NTR1  BASE=*,LABEL=*      GET WEB IO # AND REVISION                    
*                                                                               
         BRAS  RE,RDSETUPR         RETRIEVE & SAVE SETUP REC INFO               
*                                                                               
         BRAS  RE,VFNDIO#          CK FOR WEB IO #                              
         BE    GETW#R40                                                         
         BRAS  RE,VNXTIO#          RESERVE A WEB IO #                           
         B     GETW#R50                                                         
*                                                                               
GETW#R40 OC    QIOKEY,QIOKEY       PASSIVE KEY FOUND?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    QIOKEY+25,X'80'     DELETED?                                     
         BNZ   GETW#R50            NO NEED TO BUMP REV# IF DELETED              
         SR    RE,RE               WEB IO # FOUND, BUMP UP REV#                 
         IC    RE,SVW#REV#                                                      
         AHI   RE,1                                                             
         STC   RE,SVW#REV#                                                      
*                                                                               
GETW#R50 OC    SVWIO#,SVWIO#                                                    
         BNZ   *+6                                                              
         DC    H'0'                WEB IO # IS NOT SET!                         
*                                                                               
GETW#RX  J     EXIT                SET RETURN CODE                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WRIWIO#R NTR1  BASE=*,LABEL=*      GET WEB IO NUMBER AND REVISION               
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         JNE   EXIT                                                             
*                                                                               
         MVC   WKSVKEY,KEY                                                      
*                                                                               
         GOTOR VMININIT            INIT MINIO BLOCK                             
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         LA    RE,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING WIOKEY,RE                                                        
         MVC   WIOKAGY,QAGENCY     SET AGENCY                                   
         MVC   WIOKMED,QMEDIA      SET MEDIA                                    
         MVI   WIOKRCD,WIOKRCDQ    SET RECORD CODE                              
         MVC   WIOKCLT,QCLIENT     SET CLIENT                                   
         MVC   WIOKPUB,BPUB        SET PUB CODE                                 
         MVC   WIOKIOYR,SVW#IOYR   SET PERIOD YEAR                              
         MVC   WIOKIOSQ,SVW#IOSQ   SET SEQUENCE NUMBER                          
         MVC   WIOKRV#,SVW#REV#    SET REVISION NUMBER                          
         MVI   WIOKELMK,X'FF'      SET FOR MASTER KEY                           
         MVC   WIOKELMK+1(L'WIOKELMK-1),WIOKELMK                                
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)                               
*                                                                               
         BRAS  RE,BLDX10EL         ADD HEADER ELEM                              
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO                                
         BE    WRIW#30                                                          
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)                               
*                                                                               
WRIW#30  BRAS  RE,BLDX20EL         ADD WEB IO STATUS ELEM                       
         BRAS  RE,BLDX28EL         ADD WEB IO INSERTION ELEM                    
         BRAS  RE,BLDX30EL         ADD ACTIVITY ELEM                            
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,VPSSVS           BUILD ALL POINTERS                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RE,R7                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX10EL NTR1  BASE=*,LABEL=*      ADD HEADER ELEM                              
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING WIOHDRD,R2                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   HEADER ELEM CODE                             
         MVI   WIOHKLEN,WIOHDRLQ   HEADER ELEM LENGTH                           
         MVC   WIOHDATE,BTODAY                                                  
         MVI   WIOHSTAT,WIOSGENQ   GENERATED                                    
         MVC   WIOHSTRT,QPERSTR    PERIOD START                                 
         MVC   WIOHEND,QPEREND     PERIOD END                                   
         MVC   WIOHPRD,QPRODUCT    PRODUCT                                      
         MVC   WIOHEST,BEST        BINARY ESTIMATE                              
         MVC   WIOHBUY#,NUMPRCIN   NUMBER OF INSERTIONS PROCESSED               
*                                                                               
         CLI   INSORTYP,IOTSTEWQ   STEWARD ORDER?                               
         BE    BLDX10_8                                                         
         CLI   INSORTYP,IOTSTWEQ   STEWARD ORDER?                               
         BE    BLDX10_8                                                         
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         BNE   *+8                                                              
BLDX10_8 MVI   WIOHSTEW,C'S'       STEWARD ORDER                                
*                                                                               
         CLI   INSORTYP,IOT1ESTQ   EIO BY ESTIMATE?                             
         BE    *+12                                                             
         CLI   INSORTYP,IOTSTWEQ   EIO BY ESTIMATE?                             
         BNE   *+8                                                              
         OI    WIOHOTYP,WHOTESTQ   EIO BY ESTIMATE                              
*                                                                               
         CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         BE    *+12                                                             
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         BNE   *+8                                                              
         OI    WIOHOTYP,WHOTEPRQ   EIO BY ESTIMATE PERIOD                       
*                                                                               
         L     R4,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,R4                                                        
         MVC   WIOHTACC,H_TIMACC   ACCESS TIME OUT                              
         MVC   WIOHTANS,H_TIMACT   TIME TO REMIND                               
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)                               
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2,R7                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX20EL NTR1  BASE=*,LABEL=*      ADD WEB IO STATUS ELEM                       
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING WIOSTATD,R2                                                      
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   HEADER ELEM CODE                             
         MVI   WIOSKLEN,WIOSDRLQ   HEADER ELEM LENGTH                           
         MVI   WIOSKSQN,1          STARTS AT ONE                                
*                                                                               
         GOTOR DATCON,DMCB,(5,0),(25,WIOSDATE)                                  
         SR    RE,RE                                                            
         IC    RE,WIOSTIME                                                      
         AHI   RE,6                ADJUSTMENT FOR DDS TIME                      
         STC   RE,WIOSTIME                                                      
*                                                                               
         MVI   WIOSSTAT,WIOSGENQ   GENERATED                                    
         MVC   WIOSPID,SVIORPID                                                 
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX28EL NTR1  BASE=*,LABEL=*      ADD WEB IO INSERTION ELEM                    
*                                                                               
         SR    R5,R5               COUNTER                                      
         L     R4,ASER#TAB         POINT TO SERIAL# TABLE                       
         USING SER#TABD,R4                                                      
         SR    R3,R3                                                            
         ICM   R3,3,NUMSER#S       NUMBER OF SERIAL#S IN TABLE                  
*                                                                               
BX28_20  CLI   S#STATUS,S#NOTU_Q   SERIAL# NOT USED IN INS ORDER?               
         BNE   BX28_40                                                          
BX28_30  LA    R4,SER#TBLQ(R4)     POINT TO NEXT ENTRY IN TABLE                 
         BCT   R3,BX28_20                                                       
         B     BX28_X                                                           
*                                                                               
BX28_40  OC    S#PRCCNT,S#PRCCNT                                                
         BZ    BX28_30             USED, BUT NEVER PROCESSED                    
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING WIOBUYD,R2                                                       
*                                                                               
         MVI   WIOBKCDE,WIOBKIDQ   HEADER ELEM CODE                             
         MVI   WIOBKLEN,WIOBBUYL   HEADER ELEM LENGTH                           
         MVC   WIOBKSQN,S#PRCCNT                                                
         ZAP   WIOBSER#,S#SERIAL                                                
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BX28_30             CK FOR MORE TO BE ADDED                      
*                                                                               
BX28_X   J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX30EL NTR1  BASE=*,LABEL=*      ADD ACTIVITY ELEM                            
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING WIOACTHD,R2                                                      
*                                                                               
         MVI   WIOAKCDE,WIOAKACQ   HEADER ELEM CODE                             
         MVI   WIOAKLEN,WIOACTLQ   HEADER ELEM LENGTH                           
         LHI   RE,1                                                             
         STCM  RE,3,WIOAKSQN       STARTS AT ONE                                
*                                                                               
         MVC   WIOAPID,SVIORPID                                                 
         MVC   WIOADTE,BTODAY                                                   
         OI    WIOACH1,WIOAADD                                                  
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FIND IO# FOR A GIVEN DATE USUALLY THAT OF AN INSERTION                        
*                                                                               
* PERIOD DATES WILL BE UPDATED TO THAT OF INSERTION ORDER                       
*                                                                               
* EXIT    CC NOT EQUAL - IO NOT FOUND                                           
*                                                                               
*         CC EQUAL     - IO FOUND (FOLLOWING DATA WILL BE RETURNED)             
*                                                                               
*         QIOKEY   = PASSIVE KEY                                                
*         SVWIO#   = FOUND NEW SERIAL NUMBER                                    
*         SVW#REV# = CORRECT REVISION NUMBER                                    
*         WIODISKA = DISK ADDRESS OF MASTER MINIO RECORD                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VFNDIO#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVAREC,AREC                                                    
         XC    KEY,KEY             INIT KEY                                     
         XC    QIOKEY,QIOKEY       INIT IOKEY SAVEAREA                          
         MVI   PREVIOTY,0          INIT PREVIOUS INSERTION ORDER TYPE           
         XC    PREVIOES,PREVIOES   INIT PREVIOUS INSERTION ORDER EST            
         GOTOR VMININIT            INIT MINIO BLOCK                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS PERIOD PASSIVE              
         USING WIO1KEYD,R4                                                      
         MVC   WIO1AGY,QAGENCY     SET AGENCY                                   
         MVC   WIO1MED,QMEDIA      SET MEDIA                                    
         MVI   WIO1RCD,WIO1RCDQ    SET RECORD TYPE                              
         MVC   WIO1CLT,QCLIENT     SET CLIENT                                   
         MVC   WIO1PRD,QPRODUCT    SET PRODUCT                                  
         MVC   WIO1PUB,BPUB        SET PUB                                      
         MVC   WIO1END,QPERCEND    SET END DATE                                 
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
         MVC   AREC,AWKAIO2        READ FIRST RECORD INTO IOA2                  
*                                                                               
FNDIO#LP DS    0H                                                               
         CLC   WIO1KEY(WIO1END-WIO1KEYD),KEYSAVE                                
         BNE   FNDIO#DN                                                         
*                                                                               
         CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         JE    FNDIO#14                                                         
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JE    FNDIO#14                                                         
         SR    RE,RE                                                            
         ICM   RE,3,WIO1STRT                                                    
         SR    RF,RF                                                            
         ICM   RF,3,WIO1END                                                     
         SR    RF,RE               GET NUMBER OF DAYS IN DATE RANGE             
         CHI   RF,30                                                            
         JH    FNDIO#C6            SKIP - PASSIVE KEY IS BY PERIOD              
*                                                                               
FNDIO#14 CLC   QPERCEND,WIO1END    DATE MUST BE IN IO PERIOD                    
         BH    FNDIO#DN                                                         
         CLC   QPERCEND,WIO1STRT                                                
         BL    FNDIO#DN                                                         
*                                                                               
         CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         JE    *+12                                                             
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JNE   FNDIO#16                                                         
         CLC   QPERCSTR,WIO1STRT   PERIOD START MATCH?                          
         JNE   FNDIO#C6                                                         
         CLC   QPERCEND,WIO1END    PERIOD END MATCH?                            
         JNE   FNDIO#C6                                                         
*                                                                               
FNDIO#16 OC    SVWIO#,SVWIO#       SKIP IF NO IO# GIVEN                         
         BZ    *+14                                                             
         CLC   WIO1IO#,SVWIO#      FILTER ON IO#                                
         BNE   FNDIO#CN                                                         
*                                                                               
         MVC   WKTMPKEY,WIO1KEY    SAVE KEY                                     
         GOTOR GETPRT              READ IN FIRST RECORD IN MINIO SET            
         L     RF,AREC             POINT TO FOUND RECORD                        
         MVC   MINMKEY,0(RF)       GET MASTER KEY FOR MINIO SET                 
         MVI   MINDELSW,C'Y'       PROCESS DELETES                              
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD) OPEN MINIO SET                
*                                                                               
         TM    MINSTAT,MINDELQ     DELETED MINIO SET?                           
         BZ    FNDIO#L2                                                         
         CLI   INSORTYP,IOTSTEWQ   STEWARD ORDER?                               
         BE    FNDIO#L1                                                         
*                                                                               
         CLI   INSORTYP,IOT1ESTQ   SINGLE EST ORDER?                            
         BE    FNDIO#20                                                         
         CLI   INSORTYP,IOTSTWEQ   SINGLE EST STEWARD ORDER?                    
         BE    FNDIO#20                                                         
         CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         BE    FNDIO#20                                                         
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         BE    FNDIO#20                                                         
*                                                                               
         B     FNDIO#L1            NO NEED TO MATCH ESTIMATE                    
*                                                                               
FNDIO#20 TM    PREVIOTY,WHOTESTQ   INSERTION ORDER BY EST PREVIOUSLY?           
         JNZ   FNDIO#24                                                         
         TM    PREVIOTY,WHOTEPRQ   EIO BY ESTIMATE PERIOD PREVIOUSLY?           
         JNZ   FNDIO#24                                                         
         J     FNDIO#CN                                                         
FNDIO#24 BRAS  RE,GET_BEST         GET BINARY EST                               
         CLC   PREVIOES,HALF       MATCH ON EST?                                
         BNE   FNDIO#CN                                                         
         CLI   WKTMPKEY+(WIO1RV#-WIO1KEY),0                                     
         BE    FNDIO#CN                                                         
         B     FNDIO#L6                                                         
*                                                                               
FNDIO#L1 TM    PREVIOTY,WHOTESTQ   INSERTION ORDER BY EST PREVIOUSLY?           
         BNZ   FNDIO#CN                                                         
         TM    PREVIOTY,WHOTEPRQ   INSERTION ORDER BY EST PREVIOUSLY?           
         JNZ   FNDIO#CN                                                         
         OC    PREVIOES,PREVIOES   INSERTION ORDER BY EST PREVIOUSLY?           
         BNZ   FNDIO#CN                                                         
         CLI   WKTMPKEY+(WIO1RV#-WIO1KEY),0                                     
         BE    FNDIO#CN                                                         
         B     FNDIO#L6                                                         
*                                                                               
FNDIO#L2 XC    WKTMP1,WKTMP1       ESTABLISH HEADER ELEMENT                     
         LA    R6,WKTMP1                                                        
         USING WIOHDRD,R6                                                       
         MVI   WIOHKCDE,WIOHKIDQ   SET HEADER ELEMENT ID                        
         GOTOR VGETELM,DMCB,WKTMP1 READ HEADER ELEMENT                          
         BNE   FNDIO#CN            SKIP - ELEMENT NOT FOUND                     
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   PREVIOTY,WIOHOTYP                                                
         MVC   PREVIOES,WIOHEST                                                 
*                                                                               
         CLI   INSORTYP,IOTSTEWQ   STEWARD ORDER?                               
         BE    FNDIO#L8                                                         
         CLI   INSORTYP,IOT1ESTQ   SINGLE EST ORDER?                            
         BE    FNDIO#42                                                         
         CLI   INSORTYP,IOTSTWEQ   SINGLE EST STEWARD ORDER?                    
         BE    FNDIO#42                                                         
         CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         BE    FNDIO#42                                                         
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         BE    FNDIO#42                                                         
*                                                                               
         B     FNDIO#L8            NO NEED TO MATCH ESTIMATE                    
*                                                                               
FNDIO#42 TM    WIOHOTYP,WHOTESTQ   INSERTION ORDER BY EST PREVIOUSLY?           
         JNZ   FNDIO#44                                                         
         TM    WIOHOTYP,WHOTEPRQ   EIO BY ESTIMATE PERIOD PREVIOUSLY?           
         JNZ   FNDIO#44                                                         
         J     FNDIO#CN                                                         
*                                                                               
FNDIO#44 CLI   PRQESTH+5,0         HAVE ESTIMATE?                               
         BNH   FNDIO#CN                                                         
         TM    PRQESTH+4,X'08'     VALID NUMERIC?                               
         BZ    FNDIO#CN                                                         
         BRAS  RE,GET_BEST         GET BINARY EST                               
         CLC   WIOHEST,HALF        MATCH ON ESTIMATE?                           
         BNE   FNDIO#CN                                                         
*                                                                               
         TM    WIOHOTYP,WHOTESTQ   INSERTION ORDER BY EST PREVIOUSLY?           
         JZ    FNDIO#46                                                         
         CLI   INSORTYP,IOT1ESTQ   SINGLE EST ORDER?                            
         JE    FNDIO#50                                                         
         CLI   INSORTYP,IOTSTWEQ   SINGLE EST STEWARD ORDER?                    
         JE    FNDIO#50                                                         
         J     FNDIO#CN            KEEP LOOKING FOR NEXT MATCH                  
*                                                                               
FNDIO#46 TM    WIOHOTYP,WHOTEPRQ   EIO BY ESTIMATE PERIOD PREVIOUSLY?           
         JZ    FNDIO#50                                                         
         CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         JE    FNDIO#50                                                         
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JE    FNDIO#50                                                         
         J     FNDIO#CN            KEEP LOOKING FOR NEXT MATCH                  
*                                                                               
FNDIO#50 B     FNDIO#L6                                                         
*                                                                               
FNDIO#L4 CLC   INSORTYP,WIOHSTEW   MATCH ON STEWARDSHIP?                        
         BNE   FNDIO#CN                                                         
FNDIO#L6 MVC   QIOKEY,WKTMPKEY     SAVE KEY IF A MATCH                          
         B     FNDIO#CN                                                         
*                                                                               
FNDIO#L8 TM    WIOHOTYP,WHOTESTQ   EIO BY ESTIMATE?                             
         BNZ   FNDIO#CN                                                         
         TM    WIOHOTYP,WHOTEPRQ   EIO BY ESTIMATE PERIOD?                      
         BNZ   FNDIO#CN                                                         
         OC    WIOHEST,WIOHEST     HAVE ESTIMATE?                               
         BNZ   FNDIO#CN                                                         
         B     FNDIO#L4                                                         
*                                                                               
FNDIO#CN MVC   KEY,WKTMPKEY        RESTORE CURRENT KEY                          
         GOTOR HIGH                RESTORE DIRECTORY POINTER                    
FNDIO#C6 OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR SEQ                 GET NEXT POINTER                             
         B     FNDIO#LP                                                         
*                                                                               
FNDIO#DN DS    0H                                                               
         MVC   AREC,WKSVAREC       RESTORE ORIGINAL AIO POINTER                 
         MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
         MVI   MINDELSW,0          TURN OFF PROCESS DELETES                     
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         OC    QIOKEY,QIOKEY       SKIP IF KEY FOUND                            
         BNZ   *+8                                                              
         J     SETCCNEQ                                                         
*                                                                               
         LA    R4,QIOKEY           POINT TO FOUND KEY                           
         MVC   WIODISKA,WIO1DISK   SAVE MASTER RECORD DISK ADDR                 
         MVC   SVWIO#,WIO1IO#      SET IO#                                      
         MVC   SVW#REV#,WIO1RV#    SET REVISION #                               
         J     SETCCEQ             SET RETURN CODE                              
*                                                                               
GET_BEST PACK  DUB,PRQEST                                                       
         CVB   R1,DUB                                                           
         STH   R1,HALF                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FIND AND RESERVE NEXT AVAILABLE INSERTION ORDER SERIAL #                      
*                                                                               
* IF THERE ARE NO INSORDS  ON FILE, START NUMBERING AT ONE                      
*                                                                               
* ROUTINE READS FILE FOR PASSIVE POINTER THAT HAS SERIAL #S IN                  
* COMPLEMENT. READS LOWEST NUMBER (REALLY HIGHEST) FOR UPDATE                   
* AND THEN ADDS POINTER FOR NEXT NUMBER. THIS RESERVES NEXT                     
* NUMBER FOR THIS CALL TO SUBROUTINE.                                           
*                                                                               
* IF THIS RESULTS IN A DUPLICATE KEY THE PROCESS IS REPEATED                    
*                                                                               
* SCHEMA RECORD DESCRIBES RANGE WHERE NUMBER IS UNIQUE                          
*                                                                               
* EXIT    SVWIO#  =  WILL RETURN NEW SERIAL # (SVW#IOYR & SVW#IOSQ)             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VNXTIO#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WKSVKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS INV IO# PASSIVE             
         USING WIO#IO#D,R4                                                      
*                                                                               
NXTSERLP DS    0H                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   WIO#AGY,QAGENCY     SET AGENCY                                   
         MVC   WIO#MED,QMEDIA      SET MEDIA                                    
         MVI   WIO#RCD,WIO#RCDQ    SET RECORD TYPE                              
*                                                                               
* CK SCHEMA HERE TO SEE IF CLIENT/PUB INCLUDED IN KEY                           
* FOR NOW ASSUME CLIENT IS, BY PASS PUB FOR NOW                                 
*                                                                               
         MVC   WIO#CLT,QCLIENT     SET CLIENT                                   
         MVI   WIO#PUB,X'FF'                                                    
         MVC   WIO#PUB+1(L'WIO#PUB-1),WIO#PUB                                   
*                                                                               
         MVC   WIO#IOYR,QPER       SET YEAR OF INSORD PERIOD                    
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTOR  HIGH               READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLC   WIO#KEY(WIO#IOSQ-WIO#IO#D),KEYSAVE                               
         BE    NXTSER1                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE STARTING KEY                         
*                                                                               
         MVC   WIO#IOSQ,=X'000001' START SEQUENCE AT 1                          
         XC    WIO#IOSQ,=X'FFFFFF' 2'S COMPLEMENT                               
         B     NXTSER2                                                          
*                                                                               
NXTSER1  DS    0H                  READ RECORD AND RESERVE NEXT IO#             
         OI    DMINBTS,X'88'       READ FOR UPDATE AND DELETED                  
*                                                                               
         GOTOR READ                READ FOR UPDATE TO LOCK BLK OF REC'D         
*                                                                               
         NI    DMINBTS,X'FF'-X'88' RESET                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,WIO#IOSQ       GET CURRENT SEQ NUMBER                       
         SHI   RF,1                DECREMENT BY ONE                             
         STCM  RF,7,WIO#IOSQ       RESET SEQUENCE NUMBER                        
*                                                                               
NXTSER2  DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   NXTSERDN                                                         
         GOTOR ADD                 ADD TO FILE                                  
*                                                                               
         CLI   DMCB+8,0            DONE IF NO DMGR ERRORS                       
         BE    NXTSERDN                                                         
*                                                                               
         TM    DMCB+8,X'20'        OKAY IF DUPE KEY FOUND                       
         BO    *+6                                                              
         DC    H'0'                DUPE RECORD ONLY ERROR ALLOWED               
*                                                                               
NXTSERCN DS    0H                                                               
         B     NXTSERLP            REPEAT SEARCH FOR NEXT #                     
*                                                                               
NXTSERDN DS    0H                  RETURN DATA                                  
         MVC   SVW#IOYR,WIO#IOYR                                                
         MVC   SVW#IOSQ,WIO#IOSQ                                                
         XC    SVW#IOSQ,=X'FFFFFF'                                              
*                                                                               
         MVC   KEY,WKSVKEY         RESTORE CURRENT KEY                          
         GOTOR HIGH                RESTORE PRTDIR SEQUENCE                      
*                                                                               
VNXTSERX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CREATE PASSIVE POINTERS                                                       
*                                                                               
* NTRY    R7 ==> MINIO SET GETTING PASSIVES                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VPSSVS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         MVC   WKSVKEY,KEY                                                      
*                                                                               
         IC    R0,DMINBTS          SAVE SETTING                                 
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO SET OPEN                       
         BE    VPSS10                                                           
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD) OPEN MINIO SET                
*                                                                               
* FIND DISK ADDRESS OF MASTER RECORD                                            
*                                                                               
VPSS10   XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS MASTER KEY                      
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKEY,MINMKEY      COPY MINIO MASTER KEY                        
         MVI   WIOKELMK,X'FF'      SET TO MAX ELEMENT KEY                       
         MVC   WIOKELMK+1(L'WIOKELMK-1),WIOKELMK                                
*                                                                               
         GOTOR HIGH                READ MASTER KEY                              
*                                                                               
         CLC   WIOKEY,KEYSAVE      CHECK IF KEY FOUND                           
         BE    *+6                                                              
         DC    H'0'                MUST FIND KEY                                
*                                                                               
         MVC   WIODISKA,WIODDISK   SAVE DISK ADDR OF MASTER REC                 
*                                                                               
* READ HEADER ELEMENT - USED FOR MAJOR KEY FIELDS                               
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R6,WKTMP1           ESTABLISH WORK ELM AS HEADER                 
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET FOR HEADER ELEMENT                       
*                                                                               
         GOTOR VGETELM,DMCB,WKTMP1 READ HEADER ELEMENT                          
         BZ    *+6                 MUST FIND IT                                 
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
* ADD FIRST PASSIVE KEY                                                         
*                                                                               
                                                                                
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS 1ST PASSIVE                     
         USING WIO1KEY,R4                                                       
*                                                                               
         MVC   WIO1AGY,WIOKAGY-WIOKEY+MINMKEY                                   
         MVC   WIO1MED,WIOKMED-WIOKEY+MINMKEY                                   
         MVI   WIO1RCD,WIO1RCDQ                                                 
         MVC   WIO1CLT,WIOKCLT-WIOKEY+MINMKEY                                   
         MVC   WIO1PRD,WIOHPRD                                                  
         MVC   WIO1PUB,WIOKPUB-WIOKEY+MINMKEY                                   
         GOTOR DATCON,DMCB,(3,WIOHEND),(2,WIO1END)                              
         GOTOR DATCON,DMCB,(3,WIOHSTRT),(2,WIO1STRT)                            
         MVC   WIO1IO#,WIOKIO#-WIOKEY+MINMKEY     SET IO#                       
         MVC   WIO1RV#,WIOKRV#-WIOKEY+MINMKEY     SET IO#                       
*                                                                               
         GOTOR HIGH                READ PASSIVE                                 
                                                                                
         CLC   WIO1KEY,KEYSAVE     IF PASSIVE ON FILE                           
         BNE   PSVPS1NF                                                         
                                                                                
         TM    WIO1CNTL,WIODDELQ   RESTORE IF DELETED                           
         BO    *+14                                                             
         CLC   WIO1DISK,WIODISKA   DONE IF DISK ADDR SAME                       
         BE    PSVPS1X                                                          
                                                                                
         MVC   WIO1DISK,WIODISKA   ELSE SET NEW DISK ADDR                       
         NI    WIO1CNTL,X'FF'-WIODDELQ                                          
                                                                                
         GOTOR WRITE               RE-WRITE THE PASSIVE                         
                                                                                
         B     PSVPS1X                                                          
                                                                                
PSVPS1NF DS    0H                  PASSIVE NOT ON FILE                          
                                                                                
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVC   WIO1DISK,WIODISKA   SET DISK ADDRESS OF MASTER REC               
                                                                                
         GOTOR ADD                 ADD PASSIVE TO FILE                          
                                                                                
PSVPS1X  DS    0H                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)                               
*                                                                               
         STC   R0,DMINBTS          RESTORE SETTING                              
*                                                                               
VPSSVSX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VMININIT NTR1  BASE=*,LABEL=*      INITIALIZE MINIO BLOCK                       
*                                                                               
         LA    R0,MNBLKCB          CLEAR MINBLOCK AREA                          
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    MNRTAB,MNRTAB       CLEAR RECORD  TABLE                          
         XC    MNELEM,MNELEM       CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINCOMF,VCOMFACS                                                 
         MVC   MINRECUP,VRECUP                                                  
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=C'PRTFIL '  FILE NAME                                    
         MVC   MINDIR,=C'PRTDIR '  DIR NAME                                     
         MVI   MINFKLEN,25         LENGTH OF KEY                                
*                                                                               
* FULL RECORD RETAINS 75% OF IT'S ELEMENTS ON SPLITING                          
*                                                                               
         LA    R1,75               SET SPLIT PERCENTAGE TO 75%                  
         STCM  R1,3,MINSPCT                                                     
*                                                                               
         MVI   MINNCTL,L'WIODCNTL  2 CONTROL BYTES                              
         LHI   R1,2976                                                          
         STCM  R1,3,MINFRCLM       MAX RECORD LENGTH                            
*                                                                               
         MVI   MINEKLEN,L'WIOKELMK                                              
         MVI   MINEKDSP,WIOKELMK-WIOKEY                                         
*                                                                               
         MVC   MINBUFF,AWKAIO3     A(FIRST MINIO BUFFER)                        
         MVI   MINNBUF,2           NUMBER OF BUFFERS                            
*                                                                               
         LA    R1,MNELEM           A(ELEMENT AREA)                              
         ST    R1,MINELEM                                                       
         LHI   R1,L'MNELEM         MAX ELEMENT/CLUSTER LENGTH                   
         STCM  R1,3,MINMAXEL                                                    
*                                                                               
         LA    R1,MNRTAB           A(RECORD TABLE)                              
         ST    R1,MINRTAB                                                       
         LHI   R1,L'MNRTAB                                                      
         STCM  R1,3,MINRTABL       LENGTH OF RECORD TABLE                       
*                                                                               
VMININIX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO ADD AN ELEMENT TO A RECORD                                         
*                                                                               
* NTRY - PARM 1 A(ELEMENT TO BE ADDED)                                          
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT ADDED                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VADDELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE ADDED                 
*                                                                               
         OC    0(2,R6),0(R6)                                                    
         BNZ   *+6                                                              
         DC    H'0'                CONTRUCTED ELEM IS BAD                       
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE ADDED)                       
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINADD',MINBLKD)                               
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
VADDELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO ADD AN ELEMENT TO A RECORD                                         
*                                                                               
* NTRY - PARM 1 A(ELEMENT TO BE DELETED)                                        
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT DELETED                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VDELELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE DELETED               
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE DELETED)                     
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,MINEKLEN       GET KEY LENGTH                               
*                                                                               
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SET REST OF ELEMENT KEY                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINDEL',MINBLKD)                               
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
DELELSX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO GET AN ELEMENT IN A RECORD                                         
*                                                                               
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE FOUND)                                   
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE              
*                  FOR ONLY THAT AMOUNT OF KEY                                  
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT FOUND                                              
*                                                                               
*        MINELEM     A(ELEMENT FOUND)                                           
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VGETELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINRD            DEFAULT TO DIRECT READ                       
*                                                                               
         CLI   1(R6),0             USE DEFAULT IF NO LENGTH GIVEN               
         BE    VGETELM1                                                         
*                                                                               
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R6)                                                         
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       USE READ IF GREATER THAN KEY LENGTH          
         BNL   VGETELM1                                                         
*                                                                               
         LA    R0,MINHI            SET FOR READ HI/EQUAL                        
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VGETELM1 DS    0H                                                               
*                                                                               
         SR    RF,RF               GET KEY LENGTH                               
         IC    RF,MINEKLEN                                                      
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTOR VMINIO,WIOPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VGETELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO REPLACE ELEMENT IN A MINIO SET                                     
* DELETES AND ADDS NEW ELEMENT (IT'S OKAY IF NO PRIOR ELEM IS FOUND)            
*                                                                               
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE REPLACED)                                
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT REPLACED                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VWRTELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE REPLACED              
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE ADDED)                       
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINWRT',MINBLKD)                               
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VWRTELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO GET NEXT ELEMENT IN A RECORD                                       
*                                                                               
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                              
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE              
*                  FOR ONLY THAT AMOUNT OF KEY                                  
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT FOUND                                              
*                                                                               
*        MINELEM     A(ELEMENT FOUND)                                           
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VNXTELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINSEQ           SEQUENTIAL READ                              
*                                                                               
         CLI   1(R6),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    VNXTELM1                                                         
*                                                                               
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VNXTELM1 DS    0H                                                               
*                                                                               
         SR    RF,RF               GET KEY LENGTH                               
         IC    RF,MINEKLEN                                                      
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTOR VMINIO,WIOPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VNXTELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO GET PREVIOUS ELEMENT IN THE RECORD                                 
*                                                                               
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                              
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE              
*                  FOR ONLY THAT AMOUNT OF KEY                                  
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT FOUND                                              
*                                                                               
*        MINELEM     A(ELEMENT FOUND)                                           
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VPRVELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINBSQ           BACKWARD SEQUENTIAL READ                     
*                                                                               
         CLI   1(R6),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    VPRVELM1                                                         
*                                                                               
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R6)                                                         
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VPRVELM1 DS    0H                                                               
*                                                                               
         SR    RF,RF               GET KEY LENGTH                               
         IC    RF,MINEKLEN                                                      
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTOR VMINIO,WIOPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VPRVELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPINS05D DSECT                                                                  
*                                                                               
RELO05   DS    F                                                                
*                                                                               
AWKAIO1  DS    A                                                                
AWKAIO2  DS    A                                                                
AWKAIO3  DS    A                                                                
AWKAIO4  DS    A                                                                
*                                                                               
VMINIO   DS    A                                                                
*                                                                               
         DS    0D                  ALIGNMENT                                    
WIOPARMS DS    6F                  PARAMETER LIST                               
*                                                                               
QIOKEY   DS    CL32                CURRENT MINIO MASTER KEY                     
QPER     DS    0XL6                PERIOD                                       
QPERSTR  DS    XL3                 PERIOD START DATE (BINARY)                   
QPEREND  DS    XL3                 PERIOD END DATE (BINARY)                     
QPERCSTR DS    XL2                 PERIOD START DATE (COMPRESSED)               
QPERCEND DS    XL2                 PERIOD END DATE (COMPRESSED)                 
*                                                                               
MNBLKCB  DS    XL(MINBLKL)         MINIO CONTROL BLOCK                          
         DS    0D                  ALIGNMENT                                    
MNRTAB   DS    CL256               MINIO RECORD TABLE                           
MNELEM   DS    CL256               MINIO ELEMENT AREA                           
*                                                                               
PREVIOTY DS    XL(L'WIOHOTYP)      PREVIOUS INSERTION ORDER TYPE                
PREVIOES DS    XL(L'WIOHEST)       PREVIOUS INSERTION ORDER EST                 
*                                                                               
WKSVMODE DS    X                   SAVE CALLING MODE                            
WKSVKEY  DS    XL(L'KEY)                                                        
WKTMPKEY DS    XL(L'KEY)                                                        
WKSVAREC DS    XL(L'AREC)                                                       
*                                                                               
WKTMP1   DS    XL256                                                            
WKTMP2   DS    XL256                                                            
WKFULL1  DS    F                                                                
WKFULL2  DS    F                                                                
WKBYTE1  DS    X                                                                
WKBYTE2  DS    X                                                                
WKSVDMIN DS    X                   SAVE DATA MANAGER IN BITS                    
*                                                                               
WKRPNAM  DS    CL(L'PGADNAME)      REP/PUB NAME                                 
WKRPNAML DS    X                                                                
WKRPTYP  DS    X                   TYPE OF CONTACT                              
WKRPADR  DS    CL(L'PGADEADD)      FAX NUMBER OR E-MAIL ADDRESS                 
WKRPADRL DS    X                                                                
*                                                                               
CKCTFWK  DS    CL32                LOCAL WORKING STORAGE AREA                   
CKCTFIO  DS    600C                CONTROL FILE IO AREA                         
*                                                                               
WKAIO1   DS    XL4096                                                           
WKAIO2   DS    XL4096                                                           
WKAIO3   DS    XL4096                                                           
WKAIO4   DS    XL4096                                                           
*                                                                               
PPINS05X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPINSWRK1                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE POLFILE                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPINSWRK2                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036PPINS05   09/02/14'                                      
         END                                                                    
