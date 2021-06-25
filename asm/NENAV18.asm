*          DATA SET NENAV18    AT LEVEL 002 AS OF 03/24/16                      
*PHASE T31818A                                                                  
NENAV18  TITLE 'T7818 - Netpak various downloads'                               
         PRINT NOGEN                                                            
                                                                                
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* JBAS  03JAN16 001 INITIAL DEVELOPMENT - NET COMTEXT DOWNLOAD                  
*                                         NET COMCLASS DOWNLOAD                 
*                                         NET FEED DOWNLOAD                     
*                                         NET PATTERN DOWNLOAD                  
*                                         NET PGROUP DOWNLOAD                   
* JBAS  24MAR16 002                       NET PGROUP RQPGRPGN FILTER            
*=====================================================================*         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,CODE=CODE,REQUEST=*,SYSTEM=NETSYSQ,              *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                             
SEQTAB   EQU   (2000*L'NPTS3QNO)+1                                              
CLTTAB   EQU   (20*CTLNQ)+1                                                     
PRDTAB   EQU   (2000*PTLNQ)+1                                                   
                                                                                
CODE     NMOD1 SEQTAB+CLTTAB+PRDTAB,**NN18**                                    
         LR    RF,RC                                                            
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
                                                                                
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK 1              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK 2              
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         MVC   RUNMODE,RUNPMODE    EXTRACT CALLING MODE                         
                                                                                
         ST    RF,ASEQTAB          SAVE A(SEQUENCE TABLE)                       
         AHI   RF,SEQTAB                                                        
         ST    RF,ACLTTAB          SAVE A(CLIENT TABLE)                         
         AHI   RF,CLTTAB                                                        
         ST    RF,APRDTAB          SAVE A(PRODUCT TABLE)                        
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE FOR RUNNING                                       *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST 'FIRST FOR RUN' MODE                    
         JNE   PRCWRK                                                           
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         GOTOR (RF),DMCB,0,X'D9000AFE'                                          
         MVC   VTRPACK,0(R1)           SET ADDRESS OF TRPACK                    
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A0F'                                           
         GOTOR (RF),DMCB                                                        
         MVC   VDYUNPK,0(R1)           SET ADDRESS OF DYUNPK                    
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTOR (RF),DMCB                                                        
         MVC   VUNTIME,0(R1)           SET ADDRESS OF UNTIME                    
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        FIRST FOR NEW WORK                                           *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         LA    R0,REQVALS                                                       
         LHI   R1,REQVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        RUN A DOWNLOAD REQUEST                                       *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   YES                                                              
                                                                                
         LA    R0,OUTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,OUTVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL TEXT RECORD                                *         
***********************************************************************         
                                                                                
NXTCTT   J     *+12                                                             
         DC    C'*NXTCTT*'                                                      
         LR    RB,RF                                                            
         USING NXTCTT,RB                                                        
                                                                                
         USING CMXRECD,R3                                                       
         LA    R3,IOKEY            INITIALIZE                                   
         XC    ANXTELEM,ANXTELEM                                                
                                                                                
***********************************************************************         
                                                                                
         CLI   LP_RMODE,LP_RFRST   FIRST TIME IN ...                            
         JNE   NCTT40                                                           
                                                                                
         GOTOR (#VALMED,AVALMED),DMCB,RQCTXMED,L'RQCTXMED-1,QMEDX               
         JNE   NOMORE              TRANSLATE MEDIA                              
                                                                                
         GOTOR (#VALCLT,AVALCLT),DMCB,RQCTXCLT,L'RQCTXCLT-1,QCLTX               
         JNE   NOMORE              TRANSLATE CLIENT                             
                                                                                
         CLI   RQCTXACT,0          ENSURE THAT ACTION IS PROVIDED               
         JE    NOMORE                                                           
                                                                                
         MVC   DATADISP,=AL2(CMTTXTEL-CMXRECD)                                  
                                                                                
         CLI   RQCTXCML+8,0        IF COMMERCIAL LENGTH EXCEEDS 12              
         JE    NCTT10              PACK IT                                      
         CLI   RQCTXCML+8,C' '                                                  
         JE    NCTT10                                                           
         GOTO1 VTRPACK,DMCB,(C'P',RQCTXCML),RQCTXCML                            
                                                                                
NCTT10   XC    CMXKEY,CMXKEY       BUILD COMTEXT KEY                            
         MVC   CMXKID,=X'0A35'                                                  
         MVC   CMXKAM,QMEDX        WITH MEDIA                                   
         MVC   CMXKCLT,QCLTX       AND CLIENT                                   
         MVI   KEYLEN,CMXKPROD-CMXKEY-1                                         
                                                                                
         CLI   RQCTXACT,C'D'       IF ACTION IS DISPLAY, FILL IN                
         JNE   NCTT20              REST OF KEY AND READ FOR IT                  
         MVC   CMXKPROD,RQCTXPRD                                                
         MVC   CMXKNET,RQCTXNET                                                 
         MVC   CMXKCML,RQCTXCML                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOXSPDIR+IO3'                            
         JE    NCTT80                                                           
         J     NOMORE                                                           
                                                                                
NCTT20   OC    RQCTXPRD,RQCTXPRD   IF ACTION IS LIST, POSSIBLY                  
         JZ    NCTT30              FILL IN PRODUCT                              
         MVC   CMXKPROD,RQCTXPRD                                                
         MVI   KEYLEN,CMXKNET-CMXKEY-1                                          
         OC    RQCTXNET,RQCTXNET   POSSIBLY WITH NETWORK                        
         JZ    NCTT30                                                           
         MVC   CMXKNET,RQCTXNET                                                 
         MVI   KEYLEN,CMXKCML-CMXKEY-1                                          
         OC    RQCTXCML,RQCTXCML   POSSIBLY WITH COMMERCIAL                     
         JZ    NCTT30                                                           
         MVC   CMXKCML,RQCTXCML                                                 
         MVI   KEYLEN,CMXKCML+L'CMXKCML-CMXKEY-1                                
NCTT30   GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO3'                            
         J     NCTT60                                                           
                                                                                
***********************************************************************         
                                                                                
NCTT40   CLI   RQCTXACT,C'D'       IF NOT 1ST TIME IN AND ACTION IS             
         JE    NOMORE              NOT DISPLAY, READ FOR NEXT KEY               
NCTT50   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOXSPDIR+IO3'                            
                                                                                
NCTT60   ZIC   RE,KEYLEN                                                        
         EX    RE,*+8              ENSURE KEY MATCHES SEARCH CRITERIA           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JNE   NOMORE                                                           
                                                                                
         OC    RQCTXNET,RQCTXNET   IF FILTERING ON NETWORK                      
         JZ    NCTT70                                                           
         CLC   CMXKNET,RQCTXNET    ENSURE NETWORK MATCHES                       
         JNE   NCTT50                                                           
                                                                                
NCTT70   OC    RQCTXCML,RQCTXCML   IF FILTERING ON COMMERCIAL                   
         JZ    NCTT80                                                           
         CLC   CMXKCML,RQCTXCML    ENSURE COMMERCIAL MATCHES                    
         JNE   NCTT50                                                           
                                                                                
***********************************************************************         
                                                                                
NCTT80   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL+IO3'                           
                                                                                
         MVC   CTTTX1,SPACES                                                    
         MVC   CTTTX2,SPACES                                                    
         MVC   CTTTX3,SPACES                                                    
         MVC   CTTTX4,SPACES                                                    
         MVC   CTTTX5,SPACES                                                    
         MVC   CTTTX6,SPACES                                                    
         MVC   CTTTX7,SPACES                                                    
         MVC   CTTTX8,SPACES                                                    
         MVC   CTTTX9,SPACES                                                    
                                                                                
         USING CMTTXTEL,R4                                                      
         L     R4,AIO3                                                          
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL            READ FOR FIRST TEXT ELEMENT                  
         JE    NCTT90                                                           
         CLI   RQCTXACT,C'D'                                                    
         JE    NOMORE                                                           
         J     NCTT50                                                           
                                                                                
NCTT90   LA    R2,CTTTX1                                                        
         ZIC   RE,CMTLNNUM                                                      
         BCTR  RE,0                                                             
         MHI   RE,L'CTTTX1                                                      
         AR    R2,RE                                                            
                                                                                
         ZIC   RE,CMTTXTLN                                                      
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),CMTTXT      OUTPUT TEXT LINE                             
                                                                                
         CLI   RQCTXACT,C'L'                                                    
         JE    NCTT100                                                          
         BRAS  RE,NEXTEL           BUMP TO NEXT ELEMENT                         
         JNE   NCTT100                                                          
         J     NCTT90                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
NCTT100  MVC   CTTMED,RQCTXMED     OUTPUT MEDIA                                 
         MVC   CTTCLI,RQCTXCLT     CLIENT                                       
         MVC   CTTPRD,CMXKPROD     PRODUCT                                      
         MVC   CTTNET,CMXKNET      NETWORK                                      
                                                                                
         MVC   SVIOKEY,IOKEY       SAVE COMMERCIAL COMMENT KEY                  
         DROP  R3                                                               
                                                                                
         USING CMXRECD,R4                                                       
         L     R4,AIO3                                                          
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,CMXRECD+L'CMXKEY,CTTCKS              
         DROP  R4                                                               
                                                                                
         USING CMLRECD,R3                                                       
         XC    CMLKEY,CMLKEY       BUILD COMMERCIAL RECORD                      
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,QMEDX        WITH MEDIA                                   
         MVC   CMLKCLT,QCLTX       CLIENT AND COMMERCIAL                        
         MVC   CMLKCML,SVIOKEY+CMXKCML-CMXRECD                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO4'                           
                                                                                
         MVC   CTTCML,SPACES                                                    
         MVC   CTTCML(8),CMLKCML                                                
         L     R4,AIO4                                                          
         TM    15(R4),X'01'        IF COMMERCIAL ID IS PACKED                   
         JZ    NCTT110             UNPACK IT NOW                                
         GOTO1 VTRPACK,DMCB,(C'U',CTTCML),CTTCML                                
                                                                                
***********************************************************************         
                                                                                
NCTT110  MVC   IOKEY,SVIOKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO3'                            
         DROP  R3                                                               
                                                                                
NCTTX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CTTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL CLASS RECORD                               *         
***********************************************************************         
                                                                                
NXTCCL   J     *+12                                                             
         DC    C'*NXTCCL*'                                                      
         LR    RB,RF                                                            
         USING NXTCCL,RB                                                        
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
***********************************************************************         
                                                                                
         CLI   LP_RMODE,LP_RFRST   FIRST TIME IN ...                            
         JNE   NCCL40                                                           
                                                                                
         GOTOR (#VALMED,AVALMED),DMCB,RQCCLMED,L'RQCCLMED-1,QMEDX               
         JNE   NOMORE              TRANSLATE MEDIA                              
                                                                                
         MVC   DATADISP,=AL2(CLSDSCEL-CLSRECD)                                  
                                                                                
         GOTOR FMTLIST,DMCB,RQCCLLIN                                            
         XC    RQCCLCLA,RQCCLCLA   SET TO PROCESS FIRST CLASS                   
                                                                                
         GOTOR FMTLIST,DMCB,RQCCLCIN                                            
         GOTOR FMTLIST,DMCB,RQCCLPIN                                            
                                                                                
***********************************************************************         
                                                                                
NCCL10   XC    IOKEY,IOKEY         SET TO PROCESS FIRST/NEXT CLASS              
         ZICM  R0,RQCCLLIN,1                                                    
         JZ    NCCL40                                                           
         ZICM  R2,ARQCLCLA,3                                                    
NCCL20   CLC   RQCCLCLA,0(R2)                                                   
         JL    NCCL30                                                           
         LA    R2,4(R2)                                                         
         JCT   R0,NCCL20                                                        
         J     NOMORE                                                           
NCCL30   MVC   RQCCLCLA,0(R2)                                                   
                                                                                
NCCL40   BAS   RE,NXTCCLLA         PROCESS CURRENT CLASS                        
         JE    NCCLX                                                            
         CLI   RQCCLLIN,1                                                       
         JNH   NOMORE                                                           
         J     NCCL10                                                           
                                                                                
NCCLX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CTTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        PROCESS COMMERCIAL CLASSES FOR CURRENT CLASS                 *         
*        ON ENTRY ... RQCCLCLA = (CLASS CODE TO PROCESS)              *         
*                     R3       = A(IOKEY)                             *         
***********************************************************************         
                                                                                
NXTCCLLA NTR1                                                                   
         OC    IOKEY,IOKEY         IF FIRST TIME READING FOR CLASS              
         JNZ   NCCLLA40                                                         
         XC    RQCCLCLT,RQCCLCLT   SET TO PROCESS FIRST CLIENT                  
         XC    QCLTX,QCLTX                                                      
                                                                                
***********************************************************************         
                                                                                
NCCLLA10 XC    IOKEY,IOKEY         SET TO PROCESS FIRST/NEXT CLIENT             
         ZICM  R0,RQCCLCIN,1                                                    
         JZ    NCCLLA40                                                         
         ZICM  R2,ARQCLCLT,3                                                    
NCCLLA20 CLC   RQCCLCLT,0(R2)                                                   
         JL    NCCLLA30                                                         
         LA    R2,4(R2)                                                         
         JCT   R0,NCCLLA20                                                      
         J     NO                                                               
NCCLLA30 MVC   RQCCLCLT,0(R2)                                                   
         OC    RQCCLCLT,RQCCLCLT   IF CLIENT IS PROVIDED                        
         JZ    NCCLCL10                                                         
         OC    RQCCLCLT,SPACES                                                  
         GOTOR (#VALCLT,AVALCLT),DMCB,RQCCLCLT,L'RQCCLCLT-1,QCLTX               
         JNE   NO                                                               
                                                                                
NCCLLA40 BAS   RE,NXTCCLCL         PROCESS CURRENT CLIENT                       
         JE    YES                                                              
         CLI   RQCCLCIN,1                                                       
         JH    NCCLLA10                                                         
         J     NO                                                               
                                                                                
***********************************************************************         
*        PROCESS COMMERCIAL CLASSES FOR CURRENT CLIENT                *         
*        ON ENTRY ... RQCCLCLT = (CLIENT CODE TO PROCESS)             *         
*                     R3       = A(IOKEY)                             *         
***********************************************************************         
                                                                                
NXTCCLCL NTR1                                                                   
         OC    IOKEY,IOKEY         IF FIRST TIME READING FOR CLIENT             
         JNZ   NCCLCL40                                                         
         XC    RQCCLPRD,RQCCLPRD   SET TO PROCESS FIRST PRODUCT                 
                                                                                
***********************************************************************         
                                                                                
NCCLCL10 XC    IOKEY,IOKEY         SET TO PROCESS FIRST/NEXT PRODUCT            
         ZICM  R0,RQCCLPIN,1                                                    
         JZ    NCCLCL40                                                         
         ZICM  R2,ARQCLPRD,3                                                    
NCCLCL20 CLC   RQCCLPRD,0(R2)                                                   
         JL    NCCLCL30                                                         
         LA    R2,L'CMXKPROD(R2)                                                
         JCT   R0,NCCLCL20                                                      
         J     NO                                                               
NCCLCL30 MVC   RQCCLPRD,0(R2)                                                   
                                                                                
NCCLCL40 BAS   RE,NXTCCLPR         PROCESS CURRENT PRODUCT                      
         JE    YES                                                              
         CLI   RQCCLPIN,1                                                       
         JH    NCCLCL10                                                         
         J     NO                                                               
                                                                                
***********************************************************************         
*        PROCESS COMMERCIAL CLASSES FOR CURRENT PRODUCT               *         
*        ON ENTRY ... RQCCLPRD = (PRODUCT CODE TO PROCESS)            *         
*                     R3       = A(IOKEY)                             *         
***********************************************************************         
                                                                                
NXTCCLPR NTR1                                                                   
         OC    IOKEY,IOKEY         IF FIRST TIME READING FOR PRODUCT...         
         JNZ   NCCLPR20                                                         
                                                                                
         USING CLSRECD,R3                                                       
         XC    CLSKEY,CLSKEY       BUILD COMCLASS KEY                           
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,QMEDX        WITH MEDIA                                   
         MVI   KEYLEN,CLSKCLAS-CLSKEY-1                                         
                                                                                
         OC    RQCCLCLA,RQCCLCLA   POSSIBLY FILL IN CLASS CODE                  
         JZ    NCCLPR10                                                         
         MVC   CLSKCLAS,RQCCLCLA                                                
         MVI   KEYLEN,CLSKCLT-CLSKEY-1                                          
                                                                                
         OC    RQCCLCLT,RQCCLCLT   POSSIBLY FILL IN CLIENT CODE                 
         JZ    NCCLPR10                                                         
         MVC   CLSKCLT,QCLTX                                                    
         MVI   KEYLEN,CLSKPROD-CLSKEY-1                                         
                                                                                
         OC    RQCCLPRD,RQCCLPRD   POSSIBLY FILL IN PRODUCT CODE                
         JZ    NCCLPR10                                                         
         MVC   CLSKPROD,RQCCLPRD                                                
         MVI   KEYLEN,L'CLSKEY-1                                                
                                                                                
NCCLPR10 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         J     NCCLPR30                                                         
                                                                                
NCCLPR20 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR+IO3'                            
                                                                                
NCCLPR30 ZIC   RE,KEYLEN                                                        
         EX    RE,*+8              ENSURE KEY MATCHES SEARCH CRITERIA           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JNE   NO                                                               
                                                                                
         OC    RQCCLCLT,RQCCLCLT   IF FILTERING ON CLIENT CODE                  
         JZ    NCCLPR40                                                         
         CLC   CLSKCLT,QCLTX       ENSURE CLIENT MATCHES                        
         JNE   NCCLPR20                                                         
                                                                                
NCCLPR40 OC    RQCCLPRD,RQCCLPRD   IF FILTERING ON PRODUCT CODE                 
         JZ    NCCLPR50                                                         
         CLC   CLSKPROD,RQCCLPRD   ENSURE PRODUCT MATCHES                       
         JNE   NCCLPR20                                                         
                                                                                
NCCLPR50 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
                                                                                
         MVC   CCLAMED,RQCCLMED    OUTPUT MEDIA                                 
         MVC   CCLACLA,CLSKCLAS    CLASS CODE                                   
         GOTO1 VCLUNPK,DMCB,CLSKCLT,CCLACLT                                     
         MVC   CCLAPRD,CLSKPROD    AND PRODUCT                                  
         DROP  R3                                                               
                                                                                
         USING CLSRECD,R4                                                       
         L     R4,AIO3                                                          
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,CLSRECD+L'CLSKEY,CCLACKS             
         DROP  R4                                                               
                                                                                
         USING CLSDSCEL,R4                                                      
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL            OUTPUT DESCRIPTION                           
         JNE   YES                                                              
         MVC   CCLADES,CLSDESC                                                  
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT FEED RECORD                                           *         
***********************************************************************         
                                                                                
NXTFED   J     *+12                                                             
         DC    C'*NXTFED*'                                                      
         LR    RB,RF                                                            
         USING NXTFED,RB                                                        
                                                                                
         USING FEEDKEY,R3                                                       
         LA    R3,IOKEY            INITIALIZE                                   
         XC    ANXTELEM,ANXTELEM                                                
                                                                                
***********************************************************************         
                                                                                
         CLI   LP_RMODE,LP_RFRST   FIRST TIME IN ...                            
         JNE   NFED40                                                           
                                                                                
         GOTOR (#VALMED,AVALMED),DMCB,=C'N',0,QMEDX                             
         JE    *+6                 ASSUME MEDIA NETWORK                         
         DC    H'00'                                                            
                                                                                
         OC    RQFEDNET,RQFEDNET   ENSURE NETWORK IS PROVIDED                   
         JZ    NOMORE                                                           
         CLI   RQFEDACT,0          ENSURE THAT ACTION IS PROVIDED               
         JE    NOMORE                                                           
                                                                                
         OC    RQFEDCLT,RQFEDCLT   IF CLIENT IS PROVIDED                        
         JZ    NFED10                                                           
         GOTOR (#VALCLT,AVALCLT),DMCB,RQFEDCLT,L'RQFEDCLT-1,QCLTX               
         JNE   NOMORE              TRANSLATE CLIENT                             
                                                                                
NFED10   MVC   DATADISP,=AL2(FEEDELEM-FEEDKEY)                                  
                                                                                
         XC    FEEDKEY,FEEDKEY     BUILD FEED KEY                               
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,QMEDX       WITH AGENCY/MEDIA                            
         MVC   FEEDKNET,RQFEDNET   AND NETWORK                                  
         MVI   KEYLEN,FEEDKCLT-FEEDKEY-1                                        
                                                                                
         CLI   RQFEDACT,C'D'       IF ACTION IS DISPLAY, FILL IN                
         JNE   NFED20              REST OF KEY AND READ FOR IT                  
         MVC   FEEDKCLT,QCLTX                                                   
         MVC   FEEDKFD,RQFEDFED                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JE    NFED70                                                           
         J     NOMORE                                                           
                                                                                
NFED20   OC    RQFEDCLT,RQFEDCLT   IF ACTION IS LIST, POSSIBLY                  
         JZ    NFED30              FILL IN CLIENT                               
         MVC   FEEDKCLT,QCLTX                                                   
         MVI   KEYLEN,FEEDKFD-FEEDKEY-1                                         
                                                                                
         OC    RQFEDFED,RQFEDFED   POSSIBLY FILL IN FEED CODE                   
         JZ    NFED30                                                           
         MVC   FEEDKFD,RQFEDFED                                                 
         MVI   KEYLEN,L'FEEDKEY-1                                               
                                                                                
NFED30   GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         J     NFED60                                                           
                                                                                
***********************************************************************         
                                                                                
NFED40   CLI   RQFEDACT,C'D'       IF NOT 1ST TIME IN AND ACTION IS             
         JE    NOMORE              NOT DISPLAY, READ FOR NEXT KEY               
NFED50   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR+IO3'                            
                                                                                
NFED60   ZIC   RE,KEYLEN                                                        
         EX    RE,*+8              ENSURE KEY MATCHES SEARCH CRITERIA           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JNE   NOMORE                                                           
                                                                                
         OC    RQFEDFED,RQFEDFED   IF FILTERING ON FEED CODE                    
         JZ    NFED70                                                           
         CLC   FEEDKFD,RQFEDFED    ENSURE FEED CODE MATCHES                     
         JNE   NFED50                                                           
                                                                                
NFED70   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
                                                                                
         MVC   FEEDNET,RQFEDNET    OUTPUT NETWORK                               
         GOTO1 VCLUNPK,DMCB,FEEDKCLT,FEEDCLT                                    
         MVC   FEEDFED,FEEDKFD     AND FEED CODE                                
         DROP  R3                                                               
                                                                                
         USING FEEDKEY,R4                                                       
         L     R4,AIO3             OUTPUT RECORD LENGTH                         
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,FEEDKEY+L'FEEDKEY,FEEDCKS            
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING FEEDELEM,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL            REJECT IF TEXT ELEMENT IS NOT                
         JNE   NFED50              FOUND                                        
                                                                                
         LA    R2,FDTXT            R2=A(TEXT TABLE)                             
         LHI   R3,L'FDTXT                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
                                                                                
         USING FDTXD,R2                                                         
         LA    R2,FDTXT                                                         
         LHI   R6,1                                                             
                                                                                
NFED80   CLI   FEEDELLN,63                                                      
         JNE   NFED90                                                           
         EDIT  FEEDELNO,FDTXLIN,ALIGN=LEFT                                      
         MVC   FDTXTXT,FEEDELDS                                                 
         J     NFED100                                                          
                                                                                
NFED90   EDIT  (R6),FDTXLIN,ALIGN=LEFT                                          
         MVC   FDTXTXT,FEEDELNO                                                 
         AHI   R6,1                                                             
                                                                                
NFED100  LA    R2,FTLNQ(R2)                                                     
         CLI   RQFEDACT,C'L'                                                    
         JE    NFEDX                                                            
                                                                                
         BRAS  RE,NEXTEL                                                        
         JE    NFED80                                                           
         DROP  R4                                                               
                                                                                
NFEDX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,FEEDVALS                                                      
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PATTERN RECORD                                        *         
***********************************************************************         
                                                                                
NXTPAT   J     *+12                                                             
         DC    C'*NXTPAT*'                                                      
         LR    RB,RF                                                            
         USING NXTPAT,RB                                                        
                                                                                
         USING NPTXKEY,R3                                                       
         LA    R3,IOKEY                                                         
                                                                                
***********************************************************************         
                                                                                
         CLI   LP_RMODE,LP_RFRST   FIRST TIME IN ...                            
         JNE   NPAT170                                                          
                                                                                
         GOTOR FMTLIST,DMCB,RQPATCIN                                            
         GOTOR FMTLIST,DMCB,RQPATNIN                                            
         MVI   NETIND,0                                                         
         MVC   DATADISP,=AL2(42)   INITIALIZE VARIABLES                         
         L     RE,ASEQTAB                                                       
         MVI   0(RE),X'FF'                                                      
         L     RE,ACLTTAB                                                       
         MVI   0(RE),X'FF'                                                      
         L     RE,APRDTAB                                                       
         MVI   0(RE),X'FF'                                                      
                                                                                
         CLI   RQPATACT,0          ENSURE THAT ACTION IS PROVIDED               
         JE    NOMORE                                                           
         CLI   RQPATCIN,1          ENSURE AT LEAST 1 CLIENT IS                  
         JL    NOMORE              PROVIDED                                     
                                                                                
         GOTOR (#VALMED,AVALMED),DMCB,=C'N',0,QMEDX                             
         JE    *+6                 ASSUME MEDIA NETWORK                         
         DC    H'00'                                                            
                                                                                
         CLI   RQPATDPT,0          TRANSLATE DAYPART                            
         JE    NPAT10                                                           
         GOTOR (#VALDPT,AVALDPT),DMCB,(0,RQPATDPT)                              
                                                                                
NPAT10   OC    RQPATREF,RQPATREF   TRANSLATE REFERENCE NUMBER                   
         JZ    NPAT20                                                           
         XC    RQPATREF,=3X'FF'                                                 
                                                                                
***********************************************************************         
                                                                                
NPAT20   ZIC   RE,RQPATCIN         FIND NEXT POPULATED CLIENT IN                
         ZICM  RF,ARQPACLT,3       REQUEST LIST                                 
NPAT30   OC    0(L'RQPATCLT,RF),0(RF)                                           
         JNZ   NPAT40                                                           
         LA    RF,L'RQPATCLT(RF)                                                
         JCT   RE,NPAT30                                                        
         J     NOMORE                                                           
                                                                                
NPAT40   MVC   RQPATCLT,0(RF)                                                   
         XC    0(L'RQPATCLT,RF),0(RF)                                           
         GOTOR (#VALCLT,AVALCLT),DMCB,RQPATCLT,L'RQPATCLT-1,QCLTX               
         JNE   NPAT20              TRANSLATE CLIENT                             
                                                                                
         L     RE,ASEQTAB                                                       
         MVI   0(RE),X'FF'                                                      
                                                                                
NPAT50   XC    NPTXKEY,NPTXKEY                                                  
         MVC   NPTXKID,=X'0A61'    INTITIALIZE PATTERN KEY WITH                 
         MVC   NPTXAM,QMEDX        MEDIA AND CLIENT                             
         MVC   NPTXCLT,QCLTX                                                    
                                                                                
***********************************************************************         
                                                                                
         CLI   RQPATACT,C'D'       IF ACTION IS DISPLAY                         
         JNE   NPAT110                                                          
                                                                                
         CLI   RQPATCIN,1          ENSURE NUMBER OF CLIENTS DOES                
         JH    NOMORE              NOT EXCEED 1                                 
         CLI   RQPATNIN,1          ENSURE NUMBER OF NETWORKS DOES               
         JH    NOMORE              NOT EXCEED 1                                 
                                                                                
         ZICM  RE,RQPATNIN         IF NETWORK IS PROVIDED                       
         JZ    NPAT60                                                           
         CLI   RQPATNMD,0          ENSURE NETWORK MEDIA                         
         JNE   NOMORE                                                           
         CLI   RQPATNTS,C'Y'       AND NETWORKS (Y) IS NOT PROVIDED             
         JE    NOMORE                                                           
         ZICM  RE,ARQPANET,3       AND ADD NETWORK                              
         MVC   NPTXNET,0(RE)                                                    
         J     NPAT70                                                           
NPAT60   OC    RQPATNMD,RQPATNMD   OR NETWORK MEDIA TO KEY                      
         JZ    NPAT70                                                           
         MVC   NPTXNET+1(1),RQPATNMD                                            
         MVI   NPTXNET+2,X'FF'                                                  
                                                                                
NPAT70   CLI   RQPATNTS,C'Y'       IF NETWORKS (Y) IS PROVIDED                  
         JNE   NPAT75                                                           
         MVI   NPTXNET,C'$'        ADD NETWORKS (Y) TO KEY                      
                                                                                
NPAT75   OC    RQPATPRO,RQPATPRO   IF PROGRAM IS PROVIDED                       
         JZ    NPAT80                                                           
         OC    RQPATDPT,RQPATDPT   ENSURE DAYPART                               
         JNZ   NOMORE                                                           
         OC    RQPATFED,RQPATFED   AND FEED ARE NOT PROVIDED                    
         JNZ   NOMORE                                                           
         MVC   NPTXPROG,RQPATPRO   AND ADD PROGRAM                              
         J     NPAT100                                                          
NPAT80   OC    RQPATDPT,RQPATDPT                                                
         JZ    NPAT90                                                           
         MVI   NPTXPROG,X'FF'                                                   
         MVC   NPTXPROG+1(1),QDPT  OR DAYPART                                   
NPAT90   OC    RQPATFED,RQPATFED                                                
         JZ    NPAT100             AND FEED TO KEY                              
         MVI   NPTXPROG,X'FF'                                                   
         MVC   NPTXPROG+2(4),RQPATFED                                           
                                                                                
NPAT100  MVC   NPTXPRD,RQPATPRD    ADD PRODUCT                                  
         MVC   NPTXSLN,RQPATPRL    PRODUCT LENGTH                               
         MVC   NPTXPRD2,RQPATPPR   PRODUCT PARTNER                              
         MVC   NPTXSLN2,RQPATPPL   PRODUCT PARTNER LENGTH                       
         MVC   NPTXR3F,RQPATREF    AND REFERENCE NUMBER TO KEY                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOXSPDIR+IO3'                            
         JE    NPAT300                                                          
         J     NPAT20                                                           
                                                                                
***********************************************************************         
                                                                                
NPAT110  MVI   KEYLEN,NPTXNET-NPTXKEY-1                                         
                                                                                
         CLI   RQPATNIN,0          IF ACTION IS LIST                            
         JE    NPAT120             CONTINUE TO BUILD KEY                        
         ZICM  RE,ARQPANET,3       WITH NETWORK                                 
         ZIC   RF,NETIND                                                        
         MHI   RF,4                                                             
         AR    RE,RF                                                            
         MVC   NPTXNET,0(RE)                                                    
         ZIC   RF,NETIND                                                        
         AHI   RF,1                                                             
         STC   RF,NETIND                                                        
         MVI   KEYLEN,NPTXPROG-NPTXKEY-1                                        
         J     NPAT130                                                          
NPAT120  CLI   NPTXNET,C'Y'        OR NETWORKS (Y/N)                            
         JNE   NPAT125                                                          
         MVI   NPTXNET,C'$'                                                     
         MVI   KEYLEN,NPTXPROG-NPTXKEY-1                                        
         J     NPAT130                                                          
NPAT125  CLI   RQPATNMD,0          OR NETWORK MEDIA                             
         JE    NPAT160                                                          
         MVC   NPTXNET+1(1),RQPATNMD                                            
         MVI   NPTXNET+2,X'FF'                                                  
         MVI   KEYLEN,NPTXPROG-NPTXKEY-1                                        
                                                                                
NPAT130  OC    RQPATPRO,RQPATPRO   AND POSSIBLY PROGRAM                         
         JZ    NPAT140                                                          
         MVC   NPTXPROG,RQPATPRO                                                
         MVI   KEYLEN,NPTXPRD-NPTXKEY-1                                         
         J     NPAT150                                                          
NPAT140  OC    RQPATDPT,RQPATDPT   OR DAYPART-FEED                              
         JZ    NPAT160                                                          
         MVI   NPTXPROG,X'FF'                                                   
         MVC   NPTXPROG+1(1),QDPT                                               
         MVI   KEYLEN,NPTXPROG+2-NPTXKEY-1                                      
         OC    RQPATFED,RQPATFED   AND FEED                                     
         JZ    NPAT160                                                          
         MVC   NPTXPROG+2(4),RQPATFED                                           
         MVI   KEYLEN,NPTXPRD-NPTXKEY-1                                         
                                                                                
NPAT150  OC    RQPATPRD,RQPATPRD   AND POSSIBLY PRODUCT                         
         JZ    NPAT160                                                          
         MVC   NPTXPRD,RQPATPRD                                                 
         MVI   KEYLEN,NPTXSLN-NPTXKEY-1                                         
                                                                                
         CLI   RQPATPRL,0          AND POSSIBLY PRODUCT LENGTH                  
         JE    NPAT160                                                          
         MVC   NPTXSLN,RQPATPRL                                                 
         MVI   KEYLEN,NPTXPRD2-NPTXKEY-1                                        
                                                                                
         OC    RQPATPPR,RQPATPPR   AND POSSIBLY PRODUCT PARTNER                 
         JZ    NPAT160                                                          
         MVC   NPTXPRD2,RQPATPPR                                                
         MVI   KEYLEN,NPTXPRD2-NPTXKEY-1                                        
                                                                                
         CLI   RQPATPPL,0          AND POSSIBLY PRODUCT LENGTH                  
         JE    NPAT160                                                          
         MVC   NPTXSLN2,RQPATPPL                                                
         MVI   KEYLEN,NPTXR3F-NPTXKEY-1                                         
                                                                                
         OC    RQPATREF,RQPATREF   AND POSSIBLY REFERENCE NUMBER                
         JE    NPAT160                                                          
         MVC   NPTXR3F,RQPATREF                                                 
         MVI   KEYLEN,NPTXOR3G-NPTXKEY-1                                        
                                                                                
NPAT160  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO3'                            
         J     NPAT190                                                          
                                                                                
NPAT170  CLI   RQPATACT,C'D'       IF NOT 1ST TIME IN AND ACTION IS             
         JE    NOMORE              NOT DISPLAY, READ FOR NEXT KEY               
NPAT180  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOXSPDIR+IO3'                            
                                                                                
NPAT190  ZIC   RE,KEYLEN                                                        
         EX    RE,*+8              ENSURE KEY MATCHES SEARCH CRITERIA           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JE    NPAT200                                                          
                                                                                
         CLI   NETIND,0            IF KEY NO LONGER MATCHES                     
         JE    NPAT20                                                           
         CLC   NETIND,RQPATNIN     AND REQUEST CONTAINS MORE THAN               
         JNE   NPAT50              1 NETWORK, GO READ FOR NEXT                  
         J     NPAT20                                                           
                                                                                
NPAT200  OC    NPTXNET(27),NPTXNET SKIP PATTERN SEQUENCE NUMBER KEYS            
         JZ    NPAT180                                                          
         CLI   NPTXR3F,X'A0'       AND OLDER VERSIONS OF PATTERNS               
         JL    NPAT180                                                          
                                                                                
         CLI   RQPATNIN,0          IF FILTERING ON NETWORK                      
         JE    NPAT210                                                          
         CLI   NPTXNET+2,X'FF'     SKIP NETWORK MEDIA KEYS                      
         JNE   NPAT220                                                          
         J     NPAT180                                                          
                                                                                
NPAT210  CLI   NPTXPSSV,C'$'       IF NOT FILTERING ON NETWORK                  
         JE    NPAT180             SKIP NETWORK LIST KEYS                       
                                                                                
NPAT220  OC    RQPATPRO,RQPATPRO   IF FILTERING ON PROGRAM                      
         JZ    NPAT230                                                          
         CLC   RQPATPRO,NPTXPROG   ENSURE PROGRAM MATCHES                       
         JNE   NPAT180                                                          
                                                                                
NPAT230  OC    RQPATDPT,RQPATDPT   IF FILTERING ON DAYPART                      
         JZ    NPAT240                                                          
         CLI   NPTXPROG,X'FF'                                                   
         JNE   NPAT180                                                          
         CLC   QDPT,NPTXPROG+1     ENSURE DAYPART MATCHES                       
         JNE   NPAT180                                                          
                                                                                
NPAT240  OC    RQPATFED,RQPATFED   IF FILTERING ON FEED                         
         JZ    NPAT250                                                          
         CLI   NPTXPROG,X'FF'                                                   
         JNE   NPAT180                                                          
         CLC   RQPATFED,NPTXPROG+2 ENSURE FEED MATCHES                          
         JNE   NPAT180                                                          
                                                                                
NPAT250  OC    RQPATPRD,RQPATPRD   IF FILTERING ON PRODUCT                      
         JZ    NPAT260                                                          
         CLC   RQPATPRD,NPTXPRD    ENSURE PRODUCT MATCHES                       
         JNE   NPAT180                                                          
                                                                                
NPAT260  CLI   RQPATPRL,0          IF FILTERING ON PRODUCT LENGTH               
         JE    NPAT270                                                          
         CLC   RQPATPRL,NPTXSLN    ENSURE PRODUCT LENGTH MATCHES                
         JNE   NPAT180                                                          
                                                                                
NPAT270  OC    RQPATPPR,RQPATPPR   IF FILTERING ON PRODUCT PARTNER              
         JZ    NPAT280                                                          
         CLC   RQPATPPR,NPTXPRD2   ENSURE PRODUCT PARTNER MATCHES               
         JNE   NPAT180                                                          
                                                                                
NPAT280  CLI   RQPATPPL,0          IF FILTERING ON PRODUCT PARTNER              
         JE    NPAT290             LENGTH                                       
         CLC   RQPATPPL,NPTXSLN2   ENSURE PRODUCT PARTNER LENGTH                
         JNE   NPAT180             MATCHES                                      
                                                                                
NPAT290  OC    RQPATREF,RQPATREF   IF FILTERING ON REFERENCE NUMBER             
         JZ    NPAT300                                                          
         CLC   RQPATREF,NPTXR3F    ENSURE REFERENCE NUMBER MATCHES              
         JNE   NPAT180                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
NPAT300  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL+IO3'                           
                                                                                
         USING NPTXKEY,R4                                                       
         L     R4,AIO3                                                          
                                                                                
         BAS   RE,CKSEQTAB         ENSURE WE HAVEN'T RETURNED                   
         JE    NPAT180             THIS PATTERN PREVIOUSLY                      
                                                                                
         BAS   RE,FLTPATAI         ENSURE ACTIVE/INACTIVE FILTER                
         JNE   NPAT180             IS SATISFIED                                 
                                                                                
         BAS   RE,ADDCLT           ADD CLIENT TO CLIENT TABLE                   
         GOTO1 ADDPRD,DMCB,NPTXPRD ADD PRODUCT TO PRODUCT TABLE                 
         GOTO1 ADDPRD,DMCB,NPTXPRD2                                             
                                                                                
         LA    R0,PTRNVALS         CLEAR OUTPUT VARIABLES                       
         LHI   R1,PTRNVALL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,PNETS            CLEAR NETWORK LIST                           
         LHI   R1,L'PNETS                                                       
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,PDNETS           CLEAR DELETED NETWORK LIST                   
         LHI   R1,L'PDNETS                                                      
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,PCOMS            CLEAR COMMERCIALS                            
         LHI   R1,L'PCOMS                                                       
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,PCMTS            CLEAR COMMENTS                               
         LHI   R1,L'PCMTS                                                       
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,PHIAS            CLEAR HIATUS                                 
         LHI   R1,L'PHIAS                                                       
         MVCL  R0,RE                                                            
                                                                                
         MVI   PTRNMED,C'N'        PASS BACK MEDIA                              
         MVC   PTRNCLT,RQPATCLT    AND CLIENT                                   
                                                                                
         CLI   NPTXNET,C'$'                                                     
         JE    NPAT320                                                          
         CLI   NPTXNET+2,X'FF'                                                  
         JNE   NPAT310                                                          
         MVC   PTRNNMD,NPTXNET+1   PASS BACK NETWORK MEDIA                      
         J     NPAT320                                                          
NPAT310  MVC   PTRNNET,NPTXNET     OR NETWORK                                   
                                                                                
NPAT320  CLI   NPTXPROG,X'FF'      PASS BACK FEED                               
         JNE   NPAT330                                                          
         MVC   PTRNFED,NPTXPROG+2                                               
         J     NPAT340                                                          
NPAT330  MVC   PTRNPRO,NPTXPROG    OR PROGRAM                                   
                                                                                
NPAT340  MVC   PTRNPRD,NPTXPRD     PASS BACK PRODUCT                            
         MVC   PTRNPRL,NPTXSLN     PRODUCT LENGTH                               
         MVC   PTRNPPR,NPTXPRD2    PARTNER PRODUCT                              
         MVC   PTRNPPL,NPTXSLN2    PARTNER PRODUCT LENGTH                       
         ZICM  RE,NPTXR3F,3                                                     
         X     RE,=X'00FFFFFF'                                                  
         STCM  RE,7,PTRNREF        AND REFERENCE NUMBER                         
                                                                                
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,NPTRECD+L'NPTXKEY,PTRNCKS            
         DROP  R4                                                               
                                                                                
         USING NPTDATA,R4                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL            GET PATTERN DATA ELEMENT                     
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   PTRNDSC,NPTDESC     PASS BACK DESCRIPTION                        
         GOTOR VDATCON,DMCB,(3,NPTSTART),(5,PTRNPST)                            
         MVC   PTRNDPT,NPTDPART    DAYPART CODE                                 
         MVC   SVTSTAT,NPTSTAT     AND SAVE PATTERN STATUS BYTE                 
                                                                                
         MVI   PTRNDEL,C'N'                                                     
         TM    NPTSTAT,NPTS_DEL    PASS BACK DELETED STATUS                     
         JZ    NPAT350                                                          
         MVI   PTRNDEL,C'Y'                                                     
                                                                                
NPAT350  MVI   PTRNUFN,C'Y'        PASS BACK UFN DATE?                          
         CLC   NPTEND,=3X'FF'      AND END DATE                                 
         JE    NPAT360                                                          
         MVI   PTRNUFN,C'N'                                                     
         GOTOR VDATCON,DMCB,(3,NPTEND),(5,PTRNPEN)                              
                                                                                
NPAT360  TM    NPTSTAT,NPTS_TIME   IF PATTERN HAS TIME                          
         JZ    NPAT370                                                          
         OC    NPTSTIM,NPTSTIM     PASS BACK START TIME                         
         JZ    NPAT370                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),NPTSTIM                                                  
         GOTOR VUNTIME,DMCB,WORK,PTRNSTM                                        
         OC    NPTETIM,NPTETIM                                                  
         JZ    NPAT370             AND END TIME                                 
         XC    WORK,WORK                                                        
         MVC   WORK(2),NPTETIM                                                  
         GOTOR VUNTIME,DMCB,WORK,PTRNETM                                        
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING NPTXKEY,R4                                                       
NPAT370  L     R4,AIO3                                                          
         CLI   NPTXNET,C'$'        IF PATTERN HAS A NETWORK LIST                
         JNE   NPAT400                                                          
         DROP  R4                                                               
                                                                                
         LA    R2,PNETS            PREPARE TO BUILD NETWORK LIST                
         LA    R6,PDNETS           AND DELETED NETWORK LIST                     
                                                                                
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL            READ FOR FIRST/NEXT NETWORK                  
         J     *+8                 ELEMENT                                      
NPAT380  BRAS  RE,NEXTEL                                                        
         JNE   NPAT400                                                          
                                                                                
         TM    6(R4),X'80'         IF DELETED, ADD TO DELETED LIST              
         JZ    NPAT390                                                          
         MVC   0(L'PTRNNET,R6),2(R4)                                            
         LA    R6,L'PTRNNET(R6)                                                 
         J     NPAT380                                                          
                                                                                
NPAT390  MVC   0(L'PTRNNET,R2),2(R4)                                            
         LA    R2,L'PTRNNET(R2)    IF NOT DELETED, ADD TO NETWORK               
         J     NPAT380             LIST                                         
                                                                                
***********************************************************************         
                                                                                
NPAT400  LA    R2,PTRNDRO          PREPARE TO DISPLAY DERIVED ROTATION          
         L     R4,AIO3             OR PROVIDED ROTATION                         
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JE    NPAT410                                                          
         LA    R2,PTRNROT                                                       
                                                                                
         USING NPTPTNEL,R4                                                      
NPAT410  L     R4,AIO3                                                          
         MVI   ELCODE,X'32'                                                     
         BRAS  RE,GETEL                                                         
         JNE   NPAT420                                                          
         ZIC   RE,NPTPTNLN         PASS BACK ROTATION                           
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),NPTPTN                                                   
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING NPTCMLEL,R4                                                      
NPAT420  L     R4,AIO3                                                          
         MVI   ELCODE,X'30'        GET COMMERCIAL LIST ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         LLC   R2,NPTCMLLN                                                      
         SRL   R2,4                R2=# OF COMMERCIALS                          
         LA    R4,NPTCML           R4=A(COMMERCIALS)                            
         DROP  R4                                                               
                                                                                
         LA    RE,LETTAB                                                        
                                                                                
         USING PCOMSD,R1                                                        
         LA    R1,PCOMS                                                         
NPAT430  MVC   PCLET,0(RE)         ADD LETTER                                   
         MVC   PCCOM,0(R4)         COMMERCIAL ID                                
         MVC   PCPIG,8(R4)         AND PIGGYBACK ID TO COMMERCIAL LIST          
         LA    RE,1(RE)                                                         
         LA    R1,PCLNQ(R1)                                                     
         LA    R4,16(R4)                                                        
         JCT   R2,NPAT430                                                       
         DROP  R1                                                               
                                                                                
         L     R4,AIO3             FOR PERCENTAGES, FIRST ATTEMPT               
         MVI   ELCODE,X'36'        TO GET ABSURD PERCENTAGE ELEMENT             
         BRAS  RE,GETEL                                                         
         JE    NPAT440                                                          
         L     R4,AIO3             IF NOT PRESENT, TRY TO GET                   
         MVI   ELCODE,X'34'        COMMERCIAL PATTERN ELEMENT                   
         BRAS  RE,GETEL            INSTEAD                                      
         JNE   NPAT460                                                          
                                                                                
NPAT440  XR    R0,R0                                                            
         LLC   R1,1(R4)                                                         
         D     R0,=F'3'                                                         
         LR    R2,R1               R2=# OF PERCENTAGES                          
         LA    R4,2(R4)            R4=A(PERCENTAGES)                            
                                                                                
         USING PCOMSD,R1                                                        
         LA    R1,PCOMS            ADD PERCENTAGE TO COMMERCIAL LIST            
NPAT450  EDIT  (2,1(R4)),PCPCT,ALIGN=LEFT                                       
         LA    R1,PCLNQ(R1)                                                     
         LA    R4,3(R4)                                                         
         JCT   R2,NPAT450                                                       
         DROP  R1                                                               
                                                                                
***********************************************************************         
                                                                                
         USING PCMTSD,R2                                                        
NPAT460  LA    R2,PCMTS            PREPARE TO BUILD COMMENT LIST                
                                                                                
         USING NPTCMTEL,R4                                                      
         L     R4,AIO3                                                          
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL            READ FOR FIRST/NEXT COMMENT                  
         J     *+8                 ELEMENT                                      
NPAT470  BRAS  RE,NEXTEL                                                        
         JNE   NPAT480                                                          
         MVC   PCMTCMT,SPACES                                                   
         ZIC   RE,NPTCMTLN                                                      
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   PCMTCMT(0),NPTCMT    ADD COMMENT TO COMMENT LIST                 
         LA    R2,PCMTLNQ(R2)                                                   
         J     NPAT470                                                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING PHIASD,R2                                                        
NPAT480  LA    R2,PHIAS            PREPARE TO BUILD HIATUS LIST                 
                                                                                
         USING NPTHIAEL,R4                                                      
         L     R4,AIO3                                                          
         MVI   ELCODE,X'5C'                                                     
         BRAS  RE,GETEL            READ FOR FIRST/NEXT HIATUS                   
         J     *+8                 ELEMENT                                      
NPAT490  BRAS  RE,NEXTEL                                                        
         JNE   NPATX                                                            
                                                                                
         CLI   NPTHIADT+1,0                                                     
         JNE   NPAT500                                                          
         GOTO1 VDYUNPK,DMCB,NPTHIADT+2,PHDAY                                    
         J     NPAT510                                                          
                                                                                
NPAT500  GOTO1 VDATCON,DMCB,(3,NPTHIADT),(8,PHDATE)                             
                                                                                
NPAT510  OC    NPTHIATM,NPTHIATM                                                
         JZ    NPAT520                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),NPTHIATM                                                 
         GOTO1 VUNTIME,DMCB,WORK,PHSTIM                                         
         XC    WORK,WORK                                                        
         MVC   WORK(2),NPTHIATM+2                                               
         GOTO1 VUNTIME,DMCB,WORK,PHETIM                                         
                                                                                
NPAT520  LA    R2,PHIALNQ(R2)                                                   
         J     NPAT490                                                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
NPATX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,PTRNVALS                                                      
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE REJECTS PATTERN IF ITS BEEN ENCOUNTERED PREVIOUSLY   *         
*        ON ENTRY ... R4 = A(PATTERN RECORD)                          *         
***********************************************************************         
                                                                                
CKSEQTAB NTR1                                                                   
         USING NPTDATA,R4                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     RE,ASEQTAB                                                       
CST10    CLI   0(RE),X'FF'         ENSURE WE HAVEN'T ALREADY                    
         JE    CST20               RETURNED THIS PATTERN                        
         CLC   NPTS3QNO,0(RE)                                                   
         JE    YES                                                              
         LA    RE,L'NPTS3QNO(RE)                                                
         J     CST10                                                            
CST20    MVC   0(L'NPTS3QNO,RE),NPTS3QNO                                        
         MVI   L'NPTS3QNO(RE),X'FF'                                             
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES ACTIVE/INACTIVE FILTER IS SATISIFIED         *         
*        ON ENTRY ... R4 = A(PATTERN RECORD)                          *         
***********************************************************************         
                                                                                
FLTPATAI NTR1                                                                   
         USING NPTDATA,R4                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   RQPATACV,C'Y'       IF FILTERING TO RETURN ONLY ACTIVE           
         JNE   FPAI10              PATTERNS                                     
         TM    NPTSTAT,NPTS_DEL    ENSURE PATTERN IS ACTIVE                     
         JZ    YES                                                              
         J     NO                                                               
                                                                                
FPAI10   CLI   RQPATACV,C'N'       IF FILTERING TO RETURN ONLY                  
         JNE   YES                 INACTIVE PATTERNS                            
         TM    NPTSTAT,NPTS_DEL    ENSURE PATTERN IS INACTIVE                   
         JO    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADDS CLIENT TO CLIENT TABLE                          *         
*        ON ENTRY ... R4 = A(PATTERN RECORD)                          *         
***********************************************************************         
                                                                                
         USING NPTRECD,R4                                                       
ADDCLT   NTR1                                                                   
         USING CLTTABD,RE                                                       
         L     RE,ACLTTAB                                                       
ACLT10   CLI   0(RE),X'FF'         ADD CLIENT TO CLIENT TABLE                   
         JE    ACLT20                                                           
         CLC   CLTCLT,NPTXCLT                                                   
         JE    XIT                                                              
         LA    RE,CTLNQ(RE)                                                     
         J     ACLT10                                                           
ACLT20   MVC   CLTCLT,NPTXCLT                                                   
         MVI   CTLNQ(RE),X'FF'                                                  
         J     XIT                                                              
         DROP  R4,RE                                                            
                                                                                
***********************************************************************         
*        ROUTINE ADDS PRODUCT TO PRODUCT TABLE                        *         
*        ON ENTRY ... R4 = A(PATTERN RECORD)                          *         
*                     P1 = A(PRODUCT CODE)                            *         
***********************************************************************         
                                                                                
         USING NPTRECD,R4                                                       
ADDPRD   NTR1                                                                   
         L     R1,0(R1)            R1 = A(PRODUCT CODE TO ADD)                  
         OC    0(L'PRDPRD,R1),0(R1)                                             
         JZ    XIT                                                              
                                                                                
         USING PRDTABD,RE                                                       
         L     RE,APRDTAB                                                       
APRD10   CLI   0(RE),X'FF'         ADD PRODUCT TO PRODUCT TABLE                 
         JE    APRD30                                                           
         CLC   PRDCLT,NPTXCLT                                                   
         JNE   APRD20                                                           
         CLC   PRDPRD,0(R1)                                                     
         JE    XIT                                                              
APRD20   LA    RE,PTLNQ(RE)                                                     
         J     APRD10                                                           
APRD30   MVC   PRDCLT,NPTXCLT                                                   
         MVC   PRDPRD,0(R1)                                                     
         MVI   PTLNQ(RE),X'FF'                                                  
         J     XIT                                                              
         DROP  R4,RE                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
LETTAB   DC    C'ABCDEFGHIJKL'                                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PATTERN CLIENT RECORD                                 *         
***********************************************************************         
                                                                                
NXTPAC   J     *+12                                                             
         DC    C'*NXTPAC*'                                                      
         LR    RB,RF                                                            
         USING NXTPAC,RB                                                        
                                                                                
         USING CLTTABD,R2                                                       
         L     R2,ANXTELEM                                                      
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NPAC10                                                           
         L     R2,ACLTTAB                                                       
                                                                                
NPAC10   CLI   0(R2),X'FF'         IF ANY MORE CLIENTS TO OUTPUT ...            
         JE    NOMORE                                                           
                                                                                
         LA    R0,PTRNCLTS         CLEAR OUTPUT VARIABLES                       
         LHI   R1,PTCLVALL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING CLTHDR,R3                                                        
         LA    R3,IOKEY            READ CLIENT KEY/RECORD                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,CKEYTYPQ                                                
         MVC   CKEYAM,QMEDX                                                     
         MVC   CKEYCLT,CLTCLT                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO2'                            
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO2'                           
         DROP  R3                                                               
                                                                                
         USING CLTHDR,R4                                                        
         L     R4,AIO2                                                          
         GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),PTCLTCLT                          
         MVC   PTCLTNAM,CNAME      OUTPUT CLIENT CODE AND NAME                  
         DROP  R4                                                               
                                                                                
         LA    R2,CTLNQ(R2)        BUMP TO NEXT CLIENT                          
         ST    R2,ANXTELEM                                                      
         DROP  R2                                                               
                                                                                
NPACX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,PTRNCLTS                                                      
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PATTERN PRODUCT RECORD                                *         
***********************************************************************         
                                                                                
NXTPAP   J     *+12                                                             
         DC    C'*NXTPAP*'                                                      
         LR    RB,RF                                                            
         USING NXTPAP,RB                                                        
                                                                                
         USING PRDTABD,R2                                                       
         L     R2,ANXTELEM                                                      
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NPAP10                                                           
         L     R2,APRDTAB                                                       
                                                                                
NPAP10   CLI   0(R2),X'FF'         IF ANY MORE PRODUCTS TO OUTPUT ...           
         JE    NOMORE                                                           
                                                                                
         LA    R0,PTRNPRDS         CLEAR OUTPUT VARIABLES                       
         LHI   R1,PTPRVALL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
***********************************************************************         
                                                                                
         USING CLTHDR,R3                                                        
         LA    R3,IOKEY            BUILD CLIENT KEY                             
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,CKEYTYPQ                                                
         MVC   CKEYAM,QMEDX                                                     
         MVC   CKEYCLT,PRDCLT                                                   
                                                                                
         L     R4,AIO2             IF WE DON'T ALREADY HAVE CLIENT              
         CLC   CKEY,0(R4)          RECORD,READ CLIENT KEY/RECORD                
         JE    NPAP20                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO2'                            
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO2'                           
         DROP  R3                                                               
                                                                                
         USING CLTHDR,R4                                                        
NPAP20   GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),PTPRDCLT                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         MVC   PTPRDPRD,PRDPRD     OUTPUT PRODUCT CODE                          
                                                                                
         USING PRDHDR,R3                                                        
         XC    PKEY,PKEY           READ PRODUCT KEY/RECORD                      
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,PRDCLT                                                   
         MVC   PKEYPRD,PRDPRD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING PRDHDR,R4                                                        
         L     R4,AIO3                                                          
         MVC   PTPRDNAM,PNAME      OUTPUT PRODUCT NAME                          
         DROP  R4                                                               
                                                                                
         LA    R2,PTLNQ(R2)        BUMP TO NEXT PRODUCT                         
         ST    R2,ANXTELEM                                                      
         DROP  R2                                                               
                                                                                
NPAPX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,PTRNPRDS                                                      
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PRODUCT GROUP RECORD                                  *         
***********************************************************************         
                                                                                
NXTPGR   J     *+12                                                             
         DC    C'*NXTPGR*'                                                      
         LR    RB,RF                                                            
         USING NXTPGR,RB                                                        
                                                                                
         USING PRGRECD,R3                                                       
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
***********************************************************************         
                                                                                
         CLI   LP_RMODE,LP_RFRST   FIRST TIME IN ...                            
         JNE   NPGR80                                                           
                                                                                
         CLI   RQPGRACT,0          ENSURE THAT ACTION IS PROVIDED               
         JE    NOMORE                                                           
         CLI   RQPGRACT,C'D'       AND IS EITHER D(ISPLAY)                      
         JE    NPGR10                                                           
         CLI   RQPGRACT,C'L'       OR L(IST)                                    
         JNE   NOMORE                                                           
                                                                                
NPGR10   GOTOR FMTLIST,DMCB,RQPGRCIN                                            
         CLI   RQPGRCIN,1          ENSURE AT LEAST 1 CLIENT IS                  
         JL    NOMORE              PROVIDED                                     
                                                                                
         CLI   RQPGRACT,C'D'       IF ACTION IS DISPLAY                         
         JNE   NPGR20                                                           
         CLI   RQPGRCIN,1          ENSURE ONLY 1 CLIENT IS PROVIDED             
         JNE   NOMORE                                                           
         OC    RQPGRPGR,RQPGRPGR   ENSURE PRODUCT GROUP IS PROVIDED             
         JZ    NOMORE                                                           
                                                                                
NPGR20   GOTOR (#VALMED,AVALMED),DMCB,RQPGRMED,L'RQPGRMED-1,QMEDX               
         JNE   NOMORE                                                           
                                                                                
         MVC   DATADISP,=AL2(24)   INITIALIZE VARIABLES                         
                                                                                
***********************************************************************         
                                                                                
NPGR30   ZIC   RE,RQPGRCIN         FIND NEXT POPULATED CLIENT IN                
         ZICM  RF,ARQPGCLT,3       REQUEST LIST                                 
NPGR40   OC    0(L'RQPGRCLT,RF),0(RF)                                           
         JNZ   NPGR50                                                           
         LA    RF,L'RQPGRCLT(RF)                                                
         JCT   RE,NPGR40                                                        
         J     NOMORE                                                           
                                                                                
NPGR50   MVC   RQPGRCLT,0(RF)                                                   
         XC    0(L'RQPGRCLT,RF),0(RF)                                           
         GOTOR (#VALCLT,AVALCLT),DMCB,RQPGRCLT,L'RQPGRCLT-1,QCLTX               
         JNE   NPGR30              TRANSLATE CLIENT                             
                                                                                
         XC    PRGKEY,PRGKEY                                                    
         MVC   PRGKTYP,=X'0D01'    INITIALIZE PRODUCT GROUP KEY WITH            
         MVC   PRGKAGMD,QMEDX                                                   
         MVC   PRGKCLT,QCLTX       CLIENT                                       
         MVC   PRGKID,RQPGRPGR     AND PRODUCT GROUP                            
                                                                                
***********************************************************************         
                                                                                
NPGR60   CLI   RQPGRACT,C'D'       IF ACTION IS DISPLAY                         
         JNE   NPGR70              READ FOR PRODUCT GROUP                       
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(3),RQPGRPGR+1                                               
         PACK  WORK+10(3),WORK(5)                                               
         MVC   PRGKGRP,WORK+10                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JE    NPGR110                                                          
         J     NOMORE                                                           
                                                                                
***********************************************************************         
                                                                                
NPGR70   GOTOR SETLENZ,DMCB,KEYLEN,PRGKEY,L'PRGKEY                              
         ZIC   RE,KEYLEN                                                        
         SHI   RE,1                                                             
         STC   RE,KEYLEN                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         J     NPGR100                                                          
                                                                                
NPGR80   CLI   RQPGRACT,C'D'       IF NOT 1ST TIME IN AND ACTION IS             
         JE    NOMORE              NOT DISPLAY, READ FOR NEXT KEY               
NPGR90   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR+IO3'                            
                                                                                
NPGR100  ZIC   RE,KEYLEN                                                        
         EX    RE,*+8              ENSURE KEY MATCHES SEARCH CRITERIA           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JNE   NPGR30                                                           
         OC    PRGKGRP,PRGKGRP                                                  
         JZ    NPGR90                                                           
                                                                                
NPGR110  BAS   RE,GETPGDEF         GET PRODUCT GROUP DEFINITION RECORD          
         JNE   NPGR90              INTO AIO2                                    
                                                                                
***********************************************************************         
                                                                                
         LA    R0,PGRPVALS         CLEAR OUTPUT VARIABLES                       
         LHI   R1,PGRPVALL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING PRGEL01,R4                                                       
         L     R4,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   PGRPB1,PRGBK1                                                    
         MVC   PGRPB2,PRGBK2                                                    
         DROP  R4                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING PRGRECD,R4                                                       
         L     R4,AIO3                                                          
         MVC   PGRPMED,RQPGRMED    PASS BACK MEDIA                              
         MVC   PGRPCLT,RQPGRCLT    AND CLIENT                                   
                                                                                
         MVC   PGRPPGR,SPACES                                                   
         MVC   PGRPPGR(1),PRGKID   PASS BACK ID                                 
         XC    FULL,FULL                                                        
         MVC   FULL(2),PRGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
         ZIC   RE,PGRIDLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   PGRPPGR+1(0),WORK                                                
                                                                                
         OC    RQPGRPGR,RQPGRPGR                                                
         JZ    NPGR120                                                          
         GOTOR SETLEN,DMCB,BYTE,RQPGRPGR,L'RQPGRPGR                             
         ZIC   RE,BYTE                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   RQPGRPGR(0),PGRPPGR                                              
         JNE   NPGR90                                                           
                                                                                
NPGR120  OC    RQPGRPGN,RQPGRPGN                                                
         JZ    NPGR130                                                          
         GOTOR SETLEN,DMCB,BYTE,RQPGRPGN,L'RQPGRPGN                             
         ZIC   RE,BYTE                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   RQPGRPGN(0),PGRPPGR+1                                            
         JNE   NPGR90                                                           
         DROP  R4                                                               
                                                                                
         USING PRGEL10,R4                                                       
NPGR130  MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   PGRPB1N,PRGNAM1                                                  
         MVC   PGRPB2N,PRGNAM2                                                  
         DROP  R4                                                               
                                                                                
         CLI   RQPGRACT,C'D'                                                    
         JNE   NPGRX                                                            
                                                                                
         USING PRGEL30,R4                                                       
         L     R4,AIO3                                                          
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         JNE   NPGR140                                                          
         MVC   PGRPUSER,PRGUSER                                                 
         DROP  R4                                                               
                                                                                
         USING PRGEL20,R4                                                       
NPGR140  L     R4,AIO3                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         JNE   NPGR150                                                          
         MVC   PGRPBTO,PRGADDR1                                                 
         MVC   PGRPAD1,PRGADDR2                                                 
         MVC   PGRPAD2,PRGADDR3                                                 
         MVC   PGRPAD3,PRGADDR4                                                 
         DROP  R4                                                               
                                                                                
         USING PRGRECD,R4                                                       
NPGR150  L     R4,AIO3                                                          
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,PRGRECD+L'PRGKEY,PGRPCKS             
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
NPGRX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,PGRPVALS                                                      
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE GETS PRODUCT GROUP DEFINITION RECORD INTO AIO2       *         
*        AND SAVES ID LENGTH INTO PGRIDLEN                            *         
*        ON ENTRY ... R3=A(IOKEY INITAILIZED WITH PRODUCT GROUP)      *         
***********************************************************************         
                                                                                
         USING PRGRECD,R3                                                       
GETPGDEF NTR1                                                                   
         MVC   SVIOKEY,IOKEY       SAVE PRODUCT GROUP KEY                       
         XC    PRGKGRP,PRGKGRP     CLEAR GROUP NUMBER                           
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO2'                            
         JE    GPGD10                                                           
         MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         CLC   =X'0D01F38803',IOKEY                                             
         JE    *+6                                                              
         DC    H'00'                                                            
         J     NO                                                               
                                                                                
GPGD10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO2'                           
                                                                                
         USING PRGEL01,R4                                                       
         L     R4,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,PRGBK1LN                                                      
         ZIC   RF,PRGBK2LN                                                      
         AR    RE,RF                                                            
         STC   RE,PGRIDLEN         SAVE ID LENGTH                               
         DROP  R4                                                               
                                                                                
         MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SET LENGTH OF INPUT IN PROVIDED FIELD             *         
*        ON ENTRY ... P1=A(LENGTH FIELD)                              *         
*                     P2=A(PROVIDED FIELD)                            *         
*                     P3=L'PROVIDED FIELD                             *         
***********************************************************************         
                                                                                
SETLEN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(LENGTH FIELD)                           
         ZIC   R3,11(R1)           R3=L'PROVIDED FIELD                          
                                                                                
         L     RE,4(R1)            RE=A(PROVIDED FIELD)                         
                                                                                
         LR    RF,R3               COPY LENGTH                                  
         LA    R4,0(RF,RE)         POINT R4 TO LAST CHAR OF FIELD               
         SHI   R4,1                                                             
                                                                                
SLEN10   CLI   0(R4),C' '          FINDS LAST NON-BLANK CHAR                    
         JH    SLEN20              THAT'S THE NEW LENGTH                        
         AHI   R4,-1               PREVIOUS CHAR                                
         AHI   RF,-1               DECREMENT LENGTH                             
         JZ    SLEN20                                                           
         J     SLEN10                                                           
                                                                                
SLEN20   STC   RF,0(R2)                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SET LENGTH OF INPUT IN PROVIDED FIELD             *         
*        ON ENTRY ... P1=A(LENGTH FIELD)                              *         
*                     P2=A(PROVIDED FIELD)                            *         
*                     P3=L'PROVIDED FIELD                             *         
***********************************************************************         
                                                                                
SETLENZ  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(LENGTH FIELD)                           
         ZIC   R3,11(R1)           R3=L'PROVIDED FIELD                          
                                                                                
         L     RE,4(R1)            RE=A(PROVIDED FIELD)                         
                                                                                
         LR    RF,R3               COPY LENGTH                                  
         LA    R4,0(RF,RE)         POINT R4 TO LAST CHAR OF FIELD               
         SHI   R4,1                                                             
                                                                                
SLENZ10  CLI   0(R4),0             FINDS LAST NON-BLANK CHAR                    
         JH    SLENZ20             THAT'S THE NEW LENGTH                        
         AHI   R4,-1               PREVIOUS CHAR                                
         AHI   RF,-1               DECREMENT LENGTH                             
         JZ    SLENZ20                                                          
         J     SLENZ10                                                          
                                                                                
SLENZ20  STC   RF,0(R2)                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE FORMATS LIST                                         *         
*        ON ENTRY ... P1=A(LIST FIELD)                                *         
***********************************************************************         
                                                                                
FMTLIST  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         OC    0(4,R2),0(R2)                                                    
         JZ    XIT                                                              
                                                                                
         ZICM  RF,1(R2),3                                                       
         LHI   RE,9                                                             
         AR    RF,RE                                                            
         MVC   0(1,R2),0(RF)                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,7,1(R2)                                                       
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
DDACT    DC    C'Action'                                                        
DDCLA    DC    C'Class'                                                         
DDCLT    DC    C'Client'                                                        
DDCML    DC    C'Commercial'                                                    
DDFED    DC    C'Feed'                                                          
DDFLT    DC    C'Filter'                                                        
DDMED    DC    C'Media'                                                         
DDNET    DC    C'Network'                                                       
DDPRD    DC    C'Product'                                                       
DDPRL    DC    C'Product Length'                                                
DDDPT    DC    C'Daypart Code'                                                  
DDREF    DC    C'Reference Number'                                              
DDPPR    DC    C'Product Partner'                                               
DDPPL    DC    C'Product Partner Length'                                        
DDPRO    DC    C'Program'                                                       
DDNMD    DC    C'Network Media'                                                 
DDACV    DC    C'Active'                                                        
DDPGR    DC    C'Product Group'                                                 
DDPGN    DC    C'Product Group Numeric'                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
YES      LHI   RE,1                                                             
         J     *+8                                                              
NO       LHI   RE,0                                                             
         CHI   RE,1                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR COMMERCIAL TEXT DOWNLOAD                            *         
***********************************************************************         
                                                                                
REQCTXT  LKREQ H,M#TRCTXT,OUTCTXT                                               
RqMed    LKREQ F,001,(D,B#SAVED,RQCTXMED),CHAR,TEXT=(*,DDMED),COL=*             
RqClt    LKREQ F,002,(D,B#SAVED,RQCTXCLT),CHAR,TEXT=(*,DDCLT),COL=*             
RqPrd    LKREQ F,003,(D,B#SAVED,RQCTXPRD),CHAR,TEXT=(*,DDPRD),COL=*             
RqCml    LKREQ F,004,(D,B#SAVED,RQCTXCML),CHAR,TEXT=(*,DDCML),COL=*             
RqNet    LKREQ F,005,(D,B#SAVED,RQCTXNET),CHAR,TEXT=(*,DDNET),COL=*             
RqAct    LKREQ F,100,(D,B#SAVED,RQCTXACT),CHAR,TEXT=(*,DDACT),COL=*             
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR COMMERCIAL CLASS DOWNLOAD                           *         
***********************************************************************         
                                                                                
REQCCLA  LKREQ H,M#TRCCLA,OUTCCLA                                               
RqMed    LKREQ F,001,(D,B#SAVED,RQCCLMED),CHAR,TEXT=(*,DDMED),COL=*             
RqCla    LKREQ F,002,(I,B#SAVED,RQCCLLIN),CHAR,LIST=F,OLEN=4,          +        
               TEXT=(*,DDCLA),COL=*                                             
RqClt    LKREQ F,003,(I,B#SAVED,RQCCLCIN),CHAR,LIST=F,OLEN=4,          +        
               TEXT=(*,DDCLT),COL=*                                             
RqPrd    LKREQ F,004,(I,B#SAVED,RQCCLPIN),CHAR,LIST=F,                 +        
               OLEN=L'CMXKPROD,TEXT=(*,DDPRD),COL=*                             
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR FEED DOWNLOAD                                       *         
***********************************************************************         
                                                                                
REQFEED  LKREQ H,M#TRFEED,OUTFEED                                               
RqNet    LKREQ F,001,(D,B#SAVED,RQFEDNET),CHAR,TEXT=(*,DDNET),COL=*             
RqClt    LKREQ F,002,(D,B#SAVED,RQFEDCLT),CHAR,TEXT=(*,DDCLT),COL=*             
RqFed    LKREQ F,003,(D,B#SAVED,RQFEDFED),CHAR,TEXT=(*,DDFED),COL=*             
RqAct    LKREQ F,100,(D,B#SAVED,RQFEDACT),CHAR,TEXT=(*,DDACT),COL=*             
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PATTERN DOWNLOAD                                    *         
***********************************************************************         
                                                                                
REQPTRN  LKREQ H,M#TRPTRN,OUTPTRN                                               
RqClt    LKREQ F,2,(I,B#SAVED,RQPATCIN),CHAR,LIST=F,OLEN=L'RQPATCLT,   +        
               TEXT=(*,DDCLT),COL=*                                             
RqPrd    LKREQ F,3,(D,B#SAVED,RQPATPRD),CHAR,TEXT=(*,DDPRD),COL=*               
RqPrL    LKREQ F,4,(D,B#SAVED,RQPATPRL),UBIN,TEXT=(*,DDPRL),COL=*               
RqPPr    LKREQ F,5,(D,B#SAVED,RQPATPPR),CHAR,TEXT=(*,DDPPR),COL=*               
RqPPL    LKREQ F,6,(D,B#SAVED,RQPATPPL),UBIN,TEXT=(*,DDPPL),COL=*               
RqDpt    LKREQ F,7,(D,B#SAVED,RQPATDPT),CHAR,TEXT=(*,DDDPT),COL=*               
RqRef    LKREQ F,10,(D,B#SAVED,RQPATREF),UBIN,TEXT=(*,DDREF),COL=*              
RqNet    LKREQ F,11,(I,B#SAVED,RQPATNIN),CHAR,LIST=F,OLEN=4,           +        
               TEXT=(*,DDNET),COL=*                                             
RqPro    LKREQ F,12,(D,B#SAVED,RQPATPRO),CHAR,TEXT=(*,DDPRO),COL=*              
RqFed    LKREQ F,13,(D,B#SAVED,RQPATFED),CHAR,TEXT=(*,DDFED),COL=*              
RqNMd    LKREQ F,14,(D,B#SAVED,RQPATNMD),CHAR,TEXT=(*,DDNMD),COL=*              
RqNts    LKREQ F,18,(D,B#SAVED,RQPATNTS),CHAR,TEXT=(*,DDFED),COL=*              
RqAcv    LKREQ F,47,(D,B#SAVED,RQPATACV),CHAR,TEXT=(*,DDACV),COL=*              
RqAct    LKREQ F,100,(D,B#SAVED,RQPATACT),CHAR,TEXT=(*,DDACT),COL=*             
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PRODUCT GROUP DOWNLOAD                              *         
***********************************************************************         
                                                                                
REQPGRP  LKREQ H,M#NEPGRD,OUTPGRP                                               
RqMed    LKREQ F,1,(D,B#SAVED,RQPGRMED),CHAR,TEXT=(*,DDMED),COL=*               
RqClt    LKREQ F,2,(I,B#SAVED,RQPGRCIN),CHAR,LIST=F,OLEN=L'RQPGRCLT,   +        
               TEXT=(*,DDCLT),COL=*                                             
RqPGr    LKREQ F,3,(D,B#SAVED,RQPGRPGR),CHAR,TEXT=(*,DDPGR),COL=*               
RqPGN    LKREQ F,4,(D,B#SAVED,RQPGRPGN),CHAR,TEXT=(*,DDPGN),COL=*               
RqAct    LKREQ F,100,(D,B#SAVED,RQPGRACT),CHAR,TEXT=(*,DDACT),COL=*             
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - COMMERCIAL TEXT DOWNLOAD                               *         
***********************************************************************         
                                                                                
OUTCTXT  LKOUT H                                                                
                                                                                
CTXTKEY  LKOUT R,X'98'                      ** COMTEXT KEY VALUES **            
Array    LKOUT C,X'98',(A,ARYCTT)                                               
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - COMMERCIAL CLASS DOWNLOAD                              *         
***********************************************************************         
                                                                                
OUTCCLA  LKOUT H                                                                
                                                                                
CCLAKEY  LKOUT R,X'99'                      ** COMCLASS VALUES **               
Array    LKOUT C,X'99',(A,ARYCCLA)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - FEED DOWNLOAD                                          *         
***********************************************************************         
                                                                                
OUTFEED  LKOUT H                                                                
                                                                                
FEEDKY   LKOUT R,X'9A'                      ** FEED VALUES **                   
Array    LKOUT C,X'9A',(A,ARYFDKY)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - PATTERN DOWNLOAD                                       *         
***********************************************************************         
                                                                                
OUTPTRN  LKOUT H                                                                
                                                                                
PTRNVLS  LKOUT R,X'9B'                      ** PATTERN VALUES **                
Array    LKOUT C,X'9B',(A,ARYPAVLS)                                             
         LKOUT E                                                                
                                                                                
PTRNCLS  LKOUT R,X'9C'                      ** PATTERN CLIENTS **               
Array    LKOUT C,X'9C',(A,ARYPACLS)                                             
         LKOUT E                                                                
                                                                                
PTRNPRS  LKOUT R,X'9D'                      ** PATTERN PRODUCTS **              
Array    LKOUT C,X'9D',(A,ARYPAPRS)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - PRODUCT GROUP DOWNLOAD                                 *         
***********************************************************************         
                                                                                
OUTPGRP  LKOUT H                                                                
                                                                                
PGRPVLS  LKOUT R,X'9C'                      ** PRODUCT GROUP VALUES **          
Array    LKOUT C,X'9C',(A,ARYPGVLS)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL TEXT RECORDS                        *         
***********************************************************************         
                                                                                
ARYCTT   LKOUT A,(R,NXTCTT),MULTIROW=Y,ROWNAME=CTTVALS                          
RtMed    LKOUT C,1,(D,,CTTMED),CHAR,ND=Y                                        
RtCli    LKOUT C,2,(D,,CTTCLI),CHAR,ND=Y                                        
RtPrd    LKOUT C,3,(D,,CTTPRD),CHAR,ND=Y                                        
RtCom    LKOUT C,4,(D,,CTTCML),CHAR,ND=Y                                        
RtNet    LKOUT C,5,(D,,CTTNET),CHAR,ND=Y                                        
RtCkS    LKOUT C,6,(D,,CTTCKS),HEXD,ND=Y                                        
RtTx1    LKOUT C,7,(D,,CTTTX1),CHAR                                             
RtTx2    LKOUT C,7,(D,,CTTTX2),CHAR,FILTROUT=TSTCTA,SKIPCOLS=CTASKIPS           
CTASKIP  EQU   *                                                                
RtTx3    LKOUT C,7,(D,,CTTTX3),CHAR                                             
RtTx4    LKOUT C,7,(D,,CTTTX4),CHAR                                             
RtTx5    LKOUT C,7,(D,,CTTTX5),CHAR                                             
RtTx6    LKOUT C,7,(D,,CTTTX6),CHAR                                             
RtTx7    LKOUT C,7,(D,,CTTTX7),CHAR                                             
RtTx8    LKOUT C,7,(D,,CTTTX8),CHAR                                             
RtTx9    LKOUT C,7,(D,,CTTTX9),CHAR                                             
CTASKIPS EQU   (*-CTASKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL CLASS RECORDS                       *         
***********************************************************************         
                                                                                
ARYCCLA  LKOUT A,(R,NXTCCL),MULTIROW=Y,ROWNAME=CCLAVALS                         
RtMed    LKOUT C,1,(D,,CCLAMED),CHAR,ND=Y                                       
RtCla    LKOUT C,2,(D,,CCLACLA),CHAR,ND=Y                                       
RtClt    LKOUT C,3,(D,,CCLACLT),CHAR,ND=Y                                       
RtPrd    LKOUT C,4,(D,,CCLAPRD),CHAR,ND=Y                                       
RtDes    LKOUT C,5,(D,,CCLADES),CHAR,ND=Y                                       
RtCkS    LKOUT C,6,(D,,CCLACKS),HEXD,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR FEED RECORDS                                   *         
***********************************************************************         
                                                                                
ARYFDKY  LKOUT A,(R,NXTFED),MULTIROW=Y,ROWNAME=FEEDVALS                         
RtNet    LKOUT C,1,(D,,FEEDNET),CHAR,ND=Y                                       
RtClt    LKOUT C,2,(D,,FEEDCLT),CHAR,ND=Y                                       
RtFed    LKOUT C,3,(D,,FEEDFED),CHAR,ND=Y                                       
RtCkS    LKOUT C,4,(D,,FEEDCKS),HEXD,ND=Y                                       
Array    LKOUT C,5,(A,ARYFDTX)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR FEED TEXT RECORDS                              *         
***********************************************************************         
                                                                                
ARYFDTX  LKOUT A,(D,B#SAVED,FDTXT),NROWS=4,ROWWIDTH=FTLNQ,             +        
               ROWNAME=DUMMY_D                                                  
RtLine   LKOUT C,5,DUM_LIN1,(R,NXTFTLIN),UBIN,ND=Y                              
RtText   LKOUT C,6,DUM_LINC,(R,NXTFTTXT),CHAR,ND=Y                              
                                                                                
         LKOUT E                                                                
                                                                                
         USING FDTXD,R2                                                         
NXTFTLIN LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JZ    YES                                                              
         MVC   0(L'FDTXLIN,R4),FDTXLIN                                          
         LHI   R0,L'FDTXLIN                                                     
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING FDTXD,R2                                                         
NXTFTTXT LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JZ    YES                                                              
         MVC   0(L'FDTXTXT,R4),FDTXTXT                                          
         LHI   R0,L'FDTXTXT                                                     
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR PATTERN RECORDS                                *         
***********************************************************************         
                                                                                
ARYPAVLS LKOUT A,(R,NXTPAT),MULTIROW=Y,ROWNAME=PTRNVALS                         
RtMed    LKOUT C,1,(D,,PTRNMED),CHAR,ND=Y                                       
RtClt    LKOUT C,2,(D,,PTRNCLT),CHAR,ND=Y                                       
RtPrd    LKOUT C,3,(D,,PTRNPRD),CHAR,ND=Y                                       
RrPrL    LKOUT C,4,(D,,PTRNPRL),UBIN,ND=Y                                       
RtPPr    LKOUT C,5,(D,,PTRNPPR),CHAR,ND=Y                                       
RtPPL    LKOUT C,6,(D,,PTRNPPL),UBIN,ND=Y                                       
RtUFN    LKOUT C,9,(D,B#SAVED,PTRNUFN),CHAR,ND=Y                                
RtRef    LKOUT C,10,(D,,PTRNREF),UBIN,ND=Y                                      
RtDsc    LKOUT C,11,(D,,PTRNDSC),CHAR,ND=Y                                      
RtPSt    LKOUT C,12,(D,,PTRNPST),CHAR,ND=Y                                      
RtPSt    LKOUT C,13,(D,,PTRNPEN),CHAR,ND=Y                                      
RtStM    LKOUT C,14,(D,,PTRNSTM),CHAR,ND=Y                                      
RtETm    LKOUT C,15,(D,,PTRNETM),CHAR,ND=Y                                      
RtDpt    LKOUT C,17,(D,,PTRNDPT),CHAR,ND=Y                                      
Array    LKOUT C,24,(A,ARYPCOM)                                                 
RtRot    LKOUT C,28,(D,,PTRNROT),CHAR,ND=Y                                      
RtDRo    LKOUT C,40,(D,,PTRNDRO),CHAR,ND=Y                                      
RtNMd    LKOUT C,41,(D,,PTRNNMD),CHAR,ND=Y                                      
RtNet    LKOUT C,42,(D,,PTRNNET),CHAR,ND=Y                                      
RtPro    LKOUT C,43,(D,,PTRNPRO),CHAR,ND=Y                                      
RtFed    LKOUT C,44,(D,,PTRNFED),CHAR,ND=Y                                      
Array    LKOUT C,45,(A,ARYNETS)                                                 
Array    LKOUT C,46,(A,ARYDNTS)                                                 
RtDel    LKOUT C,47,(D,,PTRNDEL),CHAR,ND=Y                                      
RtCmt    LKOUT C,48,(A,ARYCMTS)                                                 
RtHia    LKOUT C,49,(A,ARYHIAS)                                                 
RtCkS    LKOUT C,200,(D,,PTRNCKS),HEXD,ND=Y                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
                                                                                
ARYPCOM  LKOUT A,(D,B#SAVED,PCOMS),NROWS=12,ROWWIDTH=PCLNQ,            +        
               ROWNAME=DUMMY_D                                                  
RtLet    LKOUT C,24,DUM_LIN1,(R,NXTLET),CHAR,ND=Y                               
RtCom    LKOUT C,25,DUM_LINC,(R,NXTCOM),CHAR,ND=Y                               
RtPig    LKOUT C,26,DUM_LINC,(R,NXTPIG),CHAR,ND=Y                               
RtPct    LKOUT C,27,DUM_LIN3,(R,NXTPCT),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
         USING PCOMSD,R2                                                        
NXTLET   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JE    YES                                                              
         MVC   0(L'PCLET,R4),PCLET                                              
         LHI   R0,1                                                             
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING PCOMSD,R2                                                        
NXTCOM   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JZ    YES                                                              
         MVC   0(12,R4),SPACES                                                  
         MVC   0(L'PCCOM,R4),PCCOM                                              
         CLI   PCCOM,C'*'                                                       
         JE    NCOM10                                                           
         TM    SVTSTAT,NPTS_ADID                                                
         JZ    NCOM10                                                           
         GOTO1 VTRPACK,DMCB,(C'U',PCCOM),0(R4)                                  
NCOM10   LHI   R0,12                                                            
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING PCOMSD,R2                                                        
NXTPIG   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JE    YES                                                              
         MVC   0(12,R4),SPACES                                                  
         MVC   0(L'PCPIG,R4),PCPIG                                              
         TM    SVTSTAT,NPTS_ADID                                                
         JZ    NPIG10                                                           
         GOTO1 VTRPACK,DMCB,(C'U',PCPIG),0(R4)                                  
NPIG10   LHI   R0,12                                                            
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING PCOMSD,R2                                                        
NXTPCT   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JE    YES                                                              
         MVC   0(L'PCPCT,R4),PCPCT                                              
         LHI   R0,L'PCPCT                                                       
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
ARYNETS  LKOUT A,(D,B#SAVED,PNETS),NROWS=32,ROWWIDTH=L'PTRNNET,        +        
               ROWNAME=DUMMY_D                                                  
RtNet    LKOUT C,45,DUM_LIN4,(R,NXTNET),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
ARYDNTS  LKOUT A,(D,B#SAVED,PDNETS),NROWS=32,ROWWIDTH=L'PTRNNET,       +        
               ROWNAME=DUMMY_D                                                  
RtNet    LKOUT C,46,DUM_LIN4,(R,NXTNET),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
NXTNET   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JZ    YES                                                              
         MVC   0(L'PTRNNET,R4),0(R2)                                            
         LHI   R0,L'PTRNNET                                                     
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
                                                                                
ARYCMTS  LKOUT A,(D,B#SAVED,PCMTS),NROWS=4,ROWWIDTH=PCMTLNQ,           +        
               ROWNAME=DUMMY_D                                                  
RtCmt    LKOUT C,48,DUM_LIN4,(R,NXTCMT),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
         USING PCMTSD,R2                                                        
NXTCMT   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         CLI   0(R2),0                                                          
         JZ    YES                                                              
         MVC   0(L'PCMTCMT,R4),PCMTCMT                                          
         LHI   R0,L'PCMTCMT                                                     
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
ARYHIAS  LKOUT A,(D,B#SAVED,PHIAS),NROWS=12,ROWWIDTH=PHIALNQ,          +        
               ROWNAME=DUMMY_D                                                  
RtDay    LKOUT C,49,DUM_LIN3,(R,NXTHDY),CHAR,ND=Y                               
RtDat    LKOUT C,50,DUM_LIN8,(R,NXTHDT),CHAR,ND=Y                               
RtSTm    LKOUT C,51,DUM_LIN5,(R,NXTHST),CHAR,ND=Y                               
RtETm    LKOUT C,52,DUM_LIN5,(R,NXTHET),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
         USING PHIASD,R2                                                        
NXTHDY   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         OC    PHDAY,PHDAY                                                      
         JZ    YES                                                              
         MVC   0(L'PHDAY,R4),PHDAY                                              
         LHI   R0,L'PHDAY                                                       
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING PHIASD,R2                                                        
NXTHDT   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         OC    PHDATE,PHDATE                                                    
         JZ    YES                                                              
         MVC   0(L'PHDATE,R4),PHDATE                                            
         LHI   R0,L'PHDATE                                                      
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING PHIASD,R2                                                        
NXTHST   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         OC    PHSTIM,PHSTIM                                                    
         JZ    YES                                                              
         MVC   0(L'PHSTIM,R4),PHSTIM                                            
         LHI   R0,L'PHSTIM                                                      
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING PHIASD,R2                                                        
NXTHET   LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN                                                  
         OC    PHETIM,PHETIM                                                    
         JZ    YES                                                              
         MVC   0(L'PHETIM,R4),PHETIM                                            
         LHI   R0,L'PHETIM                                                      
         STCM  R0,15,LP_OLEN                                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PATTERN CLIENT RECORDS                         *         
***********************************************************************         
                                                                                
ARYPACLS LKOUT A,(R,NXTPAC),MULTIROW=Y,ROWNAME=PTRNCLTS                         
RtClt    LKOUT C,2,(D,,PTCLTCLT),CHAR,ND=Y                                      
RtNam    LKOUT C,3,(D,,PTCLTNAM),CHAR,ND=Y                                      
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PATTERN PRODUCT RECORDS                        *         
***********************************************************************         
                                                                                
ARYPAPRS LKOUT A,(R,NXTPAP),MULTIROW=Y,ROWNAME=PTRNPRDS                         
RtClt    LKOUT C,2,(D,,PTPRDCLT),CHAR,ND=Y                                      
RtPrd    LKOUT C,3,(D,,PTPRDPRD),CHAR,ND=Y                                      
RtNam    LKOUT C,4,(D,,PTPRDNAM),CHAR,ND=Y                                      
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PRODUCT GROUP RECORDS                          *         
***********************************************************************         
                                                                                
ARYPGVLS LKOUT A,(R,NXTPGR),MULTIROW=Y,ROWNAME=PGRPVALS                         
RtMed    LKOUT C,1,(D,,PGRPMED),CHAR,ND=Y                                       
RtClt    LKOUT C,2,(D,,PGRPCLT),CHAR,ND=Y                                       
RtPGr    LKOUT C,3,(D,,PGRPPGR),CHAR,ND=Y                                       
RtB1     LKOUT C,4,(D,,PGRPB1),CHAR,ND=Y                                        
RtB1N    LKOUT C,5,(D,,PGRPB1N),CHAR,ND=Y                                       
RtB2     LKOUT C,6,(D,,PGRPB2),CHAR,ND=Y                                        
RtB2N    LKOUT C,7,(D,,PGRPB2N),CHAR,ND=Y                                       
RtUser   LKOUT C,8,(D,,PGRPUSER),CHAR,ND=Y                                      
RtBTo    LKOUT C,9,(D,,PGRPBTO),CHAR,ND=Y                                       
RtAd1    LKOUT C,10,(D,,PGRPAD1),CHAR,ND=Y                                      
RtAd2    LKOUT C,11,(D,,PGRPAD2),CHAR,ND=Y                                      
RtAd3    LKOUT C,12,(D,,PGRPAD3),CHAR,ND=Y                                      
RtCkS    LKOUT C,200,(D,,PGRPCKS),HEXD,ND=Y                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TEST COMTEXT ACTION                                         *         
***********************************************************************         
                                                                                
TSTCTA   CLI   RQCTXACT,C'D'                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
                                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
KEYLEN   DS    XL1                 L'KEY CHECK                                  
                                                                                
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
ELCODE   DS    XL1                 ELEMENT CODE                                 
                                                                                
***********************************************************************         
* REQUEST VALUES                                                      *         
***********************************************************************         
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUES **                         
                                                                                
* ** COMMERCIAL CLASS ** *                                                      
                                                                                
         ORG   REQVALS                                                          
RQCCLMED DS    CL2                 MEDIA CODE (CHARACTER FORMAT)                
RQCCLLIN DS    XL1                 CLASS INDEX                                  
ARQCLCLA DS    AL3                 A(CLASSES)                                   
RQCCLCLA DS    CL4                 CURRENT CLASS                                
RQCCLCIN DS    XL1                 CLIENT INDEX                                 
ARQCLCLT DS    AL3                 A(CLIENTS)                                   
RQCCLCLT DS    CL4                 CURRENT CLIENT                               
RQCCLPIN DS    XL1                 PRODUCT INDEX                                
ARQCLPRD DS    AL3                 A(PRODUCTS)                                  
RQCCLPRD DS    CL(L'CMXKPROD)      PRODUCT CODE (CHARACTER FORMAT)              
                                                                                
* ** FEED CLASS ** *                                                            
                                                                                
         ORG   REQVALS                                                          
RQFEDNET DS    CL4                 NETWORK CODE                                 
RQFEDCLT DS    CL4                 CLIENT CODE (CHARACTER FORMAT)               
RQFEDFED DS    CL4                 FEED CODE                                    
RQFEDACT DS    CL1                 ACTION                                       
                                                                                
* COMMERCIAL TEXT DOWNLOAD                                                      
                                                                                
         ORG   REQVALS                                                          
RQCTXMED DS    CL2                 MEDIA CODE (CHARACTER FORMAT)                
RQCTXCLT DS    CL4                 CLIENT CODE (CHARACTER FORMAT)               
RQCTXPRD DS    CL(L'CMXKPROD)      PRODUCT CODE (CHARACTER FORMAT)              
RQCTXCML DS    CL12                COMMERCIAL CODE                              
RQCTXNET DS    CL(L'CMXKNET)       NETWORK                                      
RQCTXACT DS    CL1                 ACTION                                       
                                                                                
* PRODUCT GROUP DOWNLOAD                                                        
                                                                                
         ORG   REQVALS                                                          
RQPGRMED DS    CL2                 MEDIA CODE (CHARACTER FORMAT)                
RQPGRCIN DS    XL1                 CLIENT INDEX                                 
ARQPGCLT DS    AL3                 A(CLIENTS)                                   
RQPGRCLT DS    CL4                 CLIENT CODE (CHARACTER FORMAT)               
RQPGRPGR DS    CL4                 PRODUCT GROUP CODE                           
RQPGRPGN DS    CL3                 PRODUCT GROUP NUMERIC CODE                   
RQPGRACT DS    CL1                 ACTION                                       
                                                                                
* PATTERN DOWNLOAD                                                              
                                                                                
         ORG   REQVALS                                                          
RQPATCIN DS    XL1                 CLIENT INDEX                                 
ARQPACLT DS    AL3                 A(CLIENTS)                                   
RQPATCLT DS    CL4                 CLIENT CODE (CHARACTER FORMAT)               
RQPATNIN DS    XL1                 NETWORK INDEX                                
ARQPANET DS    AL3                 A(NETWORKS)                                  
RQPATNMD DS    CL1                 NETWORK MEDIA                                
RQPATNTS DS    CL1                 NETWORKS (Y/N)                               
RQPATPRO DS    CL6                 PROGRAM                                      
RQPATDPT DS    CL2                 DAYPART CODE                                 
RQPATFED DS    CL4                 FEED                                         
RQPATPRD DS    CL3                 PRODUCT CODE (CHARACTER FORMAT)              
RQPATPRL DS    XL1                 PRODUCT LENGTH                               
RQPATPPR DS    CL3                 PRODUCT PARTNER                              
RQPATPPL DS    XL1                 PRODUCT PARTNER LENGTH                       
RQPATREF DS    XL3                 REFERENCE NUMBER                             
RQPATACV DS    CL1                 ACTIVE                                       
RQPATACT DS    CL1                 ACTION                                       
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
***********************************************************************         
* RESPONSE VALUES                                                     *         
***********************************************************************         
                                                                                
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
CCLAVALS DS    0X                                                               
CCLAMED  DS    CL(L'RQCCLMED)      MEDIA                                        
CCLACLA  DS    CL(L'CLSKCLAS)      CLASS                                        
CCLACLT  DS    CL4                 CLIENT                                       
CCLAPRD  DS    CL(L'CLSKPROD)      PRODUCT                                      
CCLADES  DS    CL(L'CLSDESC)       DESCRIPTION                                  
CCLACKS  DS    XL4                 CHECK SUM                                    
CCLAVALL EQU   *-OUTVALS                                                        
                                                                                
* ** FEED KEY ** *                                                              
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
FEEDVALS DS    0X                                                               
FEEDNET  DS    CL4                 NETWORK CODE                                 
FEEDCLT  DS    CL3                 CLIENT CODE                                  
FEEDFED  DS    CL4                 FEED CODE                                    
FEEDCKS  DS    XL4                 CHECK SUM                                    
FEEDVALL EQU   *-OUTVALS                                                        
                                                                                
* ** PATTERN DETAILS ** *                                                       
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
PTRNVALS DS    0X                                                               
PTRNMED  DS    CL1                 MEDIA                                        
PTRNCLT  DS    CL4                 CLIENT                                       
PTRNPRD  DS    CL3                 PRODUCT                                      
PTRNPRL  DS    XL1                 PRODUCT LENGTH                               
PTRNPPR  DS    CL3                 PRODUCT PARTNER                              
PTRNPPL  DS    XL1                 PRODUCT PARTNER LENGTH                       
PTRNUFN  DS    CL1                 UFN DATE?                                    
PTRNREF  DS    XL3                 REFERENCE NUMBER                             
PTRNDSC  DS    CL24                DESCRIPTION                                  
PTRNPST  DS    CL11                PERIOD START DATE                            
PTRNPEN  DS    CL11                PERIOD END DATE                              
PTRNSTM  DS    CL6                 START TIME                                   
PTRNETM  DS    CL6                 END TIME                                     
PTRNDPT  DS    CL2                 DAYPART CODE                                 
PTRNROT  DS    CL68                ROTATION                                     
PTRNDRO  DS    CL51                DERIVED ROTATION                             
PTRNNMD  DS    CL1                 NETWORK MEDIA                                
PTRNNET  DS    CL4                 NETWORK                                      
PTRNPRO  DS    CL6                 PROGRAM                                      
PTRNFED  DS    CL4                 FEED                                         
PTRNDEL  DS    CL1                 DELETED?                                     
PTRNCKS  DS    XL4                 CHECK SUM                                    
PTRNVALL EQU   *-OUTVALS                                                        
                                                                                
* ** PATTERN CLIENT DETAILS ** *                                                
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
PTRNCLTS DS    0X                                                               
PTCLTCLT DS    CL4                 CLIENT                                       
PTCLTNAM DS    CL20                NAME                                         
PTCLVALL EQU   *-OUTVALS                                                        
                                                                                
* ** PATTERN PRODUCT DETAILS ** *                                               
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
PTRNPRDS DS    0X                                                               
PTPRDCLT DS    CL4                 CLIENT                                       
PTPRDPRD DS    CL3                 PRODUCT                                      
PTPRDNAM DS    CL20                NAME                                         
PTPRVALL EQU   *-OUTVALS                                                        
                                                                                
* ** PRODUCT GROUP ** *                                                         
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
PGRPVALS DS    0X                                                               
PGRPMED  DS    CL2                 MEDIA CODE                                   
PGRPCLT  DS    CL3                 CLIENT CODE                                  
PGRPPGR  DS    CL4                 PRODUCT GROUP                                
PGRPB1   DS    CL12                BREAK 1                                      
PGRPB1N  DS    CL24                BREAK 1 NAME                                 
PGRPB2   DS    CL12                BREAK 1                                      
PGRPB2N  DS    CL24                BREAK 2 NAME                                 
PGRPUSER DS    CL3                 USER FIELD MASTER PRODUCT                    
PGRPBTO  DS    CL30                BILL TO                                      
PGRPAD1  DS    CL30                ADDRESS 1                                    
PGRPAD2  DS    CL30                ADDRESS 2                                    
PGRPAD3  DS    CL30                ADDRESS 3                                    
PGRPCKS  DS    XL4                 CHECK SUM                                    
PGRPVALL EQU   *-OUTVALS                                                        
                                                                                
* ** COMMERCIAL TEXT KEY ** *                                                   
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
CTTVALS  DS    0X                                                               
CTTMED   DS    CL(L'RQCTXMED)      MEDIA                                        
CTTCLI   DS    CL(L'RQCTXCLT)      CLIENT                                       
CTTPRD   DS    CL(L'CMXKPROD)      PRODUCT                                      
CTTCML   DS    CL12                COMMERCIAL                                   
CTTNET   DS    CL(L'CMXKNET)       NETWORK                                      
CTTCKS   DS    XL4                 CHECK SUM                                    
CTTTX1   DS    CL58                TEXT LINE 1                                  
CTTTX2   DS    CL58                TEXT LINE 2                                  
CTTTX3   DS    CL58                TEXT LINE 3                                  
CTTTX4   DS    CL58                TEXT LINE 4                                  
CTTTX5   DS    CL58                TEXT LINE 5                                  
CTTTX6   DS    CL58                TEXT LINE 6                                  
CTTTX7   DS    CL58                TEXT LINE 7                                  
CTTTX8   DS    CL58                TEXT LINE 8                                  
CTTTX9   DS    CL58                TEXT LINE 9                                  
CTTVALL EQU    *-OUTVALS                                                        
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
*** REGULAR STORAGE **                                                          
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
                                                                                
ASEQTAB  DS    A                   A(PATTERN SEQUENCE TABLE)                    
ACLTTAB  DS    A                   A(CLIENT TABLE)                              
APRDTAB  DS    A                   A(PRODUCT TABLE)                             
                                                                                
VTRPACK  DS    A                   A(TRPACK)                                    
VDYUNPK  DS    A                   A(DYUNPK)                                    
VUNTIME  DS    A                   A(UNTIME)                                    
                                                                                
SVIOKEY  DS    XL(L'IOKEY)         SAVED IOKEY                                  
ANXTELEM DS    A                   A(NEXT ELEMENT)                              
                                                                                
NETIND   DS    XL1                 INDEX OF NEXT NETWORK                        
                                                                                
PGRIDLEN DS    X                   PRODUCT GROUP ID LENGTH                      
                                                                                
SVTSTAT  DS    XL(L'NPTSTAT)       SAVED PATTERN STATUS BYTE                    
                                                                                
PNETS    DS    CL(32*L'PTRNNET)    PATTERN NETWORK TABLE                        
PDNETS   DS    CL(32*L'PTRNNET)    PATTERN DELETED NETWORK TABLE                
PCOMS    DS    CL(12*PCLNQ)        PATTERN COMERCIAL TABLE                      
PCMTS    DS    CL(4*PCMTLNQ)       PATTERN COMMENT TABLE                        
PHIAS    DS    CL(12*PHIALNQ)      PATTERN HIATUS TABLE                         
FDTXT    DS    CL(4*FTLNQ)         FEED TEXT LINES TABLE                        
         EJECT                                                                  
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPTRCMLTXT                                                     
       ++INCLUDE SPTRCMLCLS                                                     
       ++INCLUDE SPTRNFEED                                                      
       ++INCLUDE SPTRNPAT                                                       
       ++INCLUDE SPGENPRG                                                       
         PRINT ON                                                               
DUMMY_D  DSECT                                                                  
DUM_LIN1 DS    XL1                                                              
         ORG   DUM_LIN1                                                         
DUM_LIN3 DS    XL3                                                              
         ORG   DUM_LIN3                                                         
DUM_LIN4 DS    XL4                                                              
         ORG   DUM_LIN4                                                         
DUM_LIN5 DS    XL5                                                              
         ORG   DUM_LIN5                                                         
DUM_LIN8 DS    XL8                                                              
         ORG   DUM_LIN8                                                         
DUM_LINC DS    XL12                                                             
         ORG   DUM_LINC                                                         
DUM_LINZ DS    XL53                                                             
         EJECT                                                                  
***********************************************************************         
*        PATTERN COMMERCIALS TABLE                                    *         
***********************************************************************         
                                                                                
PCOMSD   DSECT                                                                  
PCLET    DS    CL1                                                              
PCCOM    DS    CL8                                                              
PCPIG    DS    CL8                                                              
PCPCT    DS    CL3                                                              
PCLNQ    EQU   *-PCOMSD                                                         
                                                                                
***********************************************************************         
*        PATTERN COMMENTS TABLE                                       *         
***********************************************************************         
                                                                                
PCMTSD   DSECT                                                                  
PCMTCMT  DS    CL52                                                             
PCMTLNQ  EQU   *-PCMTSD                                                         
                                                                                
***********************************************************************         
*        PATTERN HIATUS TABLE                                         *         
***********************************************************************         
                                                                                
PHIASD   DSECT                                                                  
PHDAY    DS    CL10                                                             
PHDATE   DS    CL8                                                              
PHSTIM   DS    CL5                                                              
PHETIM   DS    CL5                                                              
PHIALNQ  EQU   *-PHIASD                                                         
                                                                                
***********************************************************************         
*        PATTERN CLIENTS TABLE                                        *         
***********************************************************************         
                                                                                
CLTTABD  DSECT                                                                  
CLTCLT   DS    XL(L'NPTXCLT)                                                    
CTLNQ    EQU   *-CLTTABD                                                        
                                                                                
***********************************************************************         
*        PATTERN PRODUCTS TABLE                                       *         
***********************************************************************         
                                                                                
PRDTABD  DSECT                                                                  
PRDCLT   DS    XL(L'NPTXCLT)                                                    
PRDPRD   DS    CL(L'NPTXPRD)                                                    
PTLNQ    EQU   *-PRDTABD                                                        
                                                                                
***********************************************************************         
*        FEED TEXT TABLE                                              *         
***********************************************************************         
                                                                                
FDTXD    DSECT                                                                  
FDTXLIN  DS    X                   TEXT LINE NUMBER                             
FDTXTXT  DS    CL60                TEXT DATA                                    
FTLNQ    EQU   *-FDTXD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NENAV18   03/24/16'                                      
         END                                                                    
