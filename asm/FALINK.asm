*          DATA SET FALINK     AT LEVEL 007 AS OF 08/18/15                      
*PHASE T00AE7A                                                                  
FALINK   TITLE '- handle data transfer for applications'                        
                                                                                
VERSIONQ EQU   V6Q                 Current module version number                
                                                                                
FALINK   RSECT ,                                                                
         PRINT NOGEN                                                            
         NMOD1 WORKDL,*FALINK*,CLEAR=YES                                        
         USING WORKD,RC            RC=A(local working storage area)             
         USING FALINKD,FAPARMS                                                  
         LR    R2,R1               Point to parameter list                      
         L     R9,0(R2)            Point to saved storage area                  
         L     R9,FALASVE-FALINKD(R9)                                           
         USING SAVED,R9            R9=A(saved storage)                          
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
                                                                                
         MVC   FALKWEYE,FALWEYE    Set working storage eyecatcher               
         CLC   FALKSEYE,FALSEYE    Test saved storage eyecatcher intact         
         JE    INIT0002                                                         
         LA    R0,SAVED            No - intialize SAVED                         
         LHI   R1,SAVEL                                                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   FALKSEYE,FALSEYE                                                 
                                                                                
INIT0002 ST    R2,CALLR1           Save parameter list address                  
         L     RF,0(R2)            Point to FALINKD                             
         MVC   FAPARMS(FALINKDL),0(RF)                                          
         ST    RD,CALLRD           Save current RD                              
         MVC   CLIENTRD,4(RD)      Save client's RD                             
         ST    RC,AWORKD           Save for re-entry points                     
                                                                                
         CLI   VRSN,0              Version set?                                 
         JNE   *+8                                                              
         MVI   VRSN,V1Q            Set version 1 (earliest version)             
                                                                                
         XC    DUB,DUB             Call FASWITCH to get A(SYSFACS)              
         MVC   DUB(4),EFFS                                                      
         MVI   DUB,X'FE'                                                        
         GOTOR FALASWCH,DUB                                                     
         L     RF,DUB                                                           
         USING SYSFACD,RF          RF=A(SYSFAC)                                 
         MVC   SHIPIT,VSHIPIT                                                   
                                                                                
         USING COMFACSD,R1                                                      
         L     R1,VSYSFAC0         Extract COMFACS addresses                    
         MVC   DMGR,CDATAMGR                                                    
         MVC   DATCON,CDATCON                                                   
         MVC   GETTXT,CGETTXT                                                   
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   HEXIN,CHEXIN                                                     
         MVC   CASHVAL,CCASHVAL                                                 
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   GETFACT,CGETFACT                                                 
         DROP  R1                                                               
                                                                                
         L     RF,VSSB              Extract TCB addresses                       
         L     RF,SSBTKADR-SSBD(RF) Current task running                        
                                                                                
         USING TCBD,RF             RF=A(TCB)                                    
         MVC   FLNK,TCBLNK         Get address of data page buffer              
         MVC   FTWA,TCBTWA         Get address of TWA                           
         MVC   FUTL,TCBUTL         Get address of UTL entry                     
         MVC   FXATBUFF,TCBRBUFF   Get address of XA TBUFF                      
         DROP  RF                                                               
                                                                                
         USING UTLD,RF                                                          
         L     RF,FUTL                                                          
         TM    TSTATC,TSTATFUP     Bulk upload?                                 
         BZ    *+8                                                              
         OI    FAUPIND,FAUPBULK    Yes                                          
         DROP  RF                                                               
                                                                                
         CLC   FALAPGS,=AL4(FALATMS)                                            
         JNE   *+8                                                              
         OI    SFLAG2,SFTMPEST                                                  
         CLC   FALAPGS,=AL4(FALATMSA)                                           
         JNE   *+8                                                              
         OI    SFLAG2,SFTMPEST+SFSHARE                                          
         CLC   FALAPGS,=AL4(FALATMSB)                                           
         JNE   *+8                                                              
         OI    SFLAG2,SFTMPEST+SFSHARE                                          
                                                                                
         L     RE,FLNK                                                          
         AH    RE,PAGESIZE                                                      
         ST    RE,FLNKX            Save A(byte after FLNK)                      
                                                                                
         TM    SFLAG2,SFTMPEST     Test using TEMPEST                           
         JZ    INIT0050            No                                           
         CLI   PAGETHIS,0          Test have allocation already                 
         JNE   INIT0060            Yes                                          
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,FALUNCIS       Get override number of C/Is                  
         JNZ   INIT0040                                                         
                                                                                
         LHI   R0,FALDFTCI         Default number of C/Is to allocate           
         TM    SFLAG2,SFSHARE      Test halve standard allocation               
         JZ    INIT0040                                                         
         SRL   R0,1                Yes - divide by two                          
                                                                                
INIT0040 GOTOR DMGR,DMCB,DMRSRV,TEMPEST,((R0),0),0                              
         JE    *+10                Exit when we have some storage               
         JCT   R0,INIT0040         Ask for one less next time                   
         DC    H'0'                Can't reserve any TEMPEST                    
                                                                                
         MH    R0,10(R1)           RF=C/Is*records per C/I                      
         STC   R0,PAGETHIS         Set number of pages allocated                
         LH    R0,12(R1)           Get previous C/Is allocated                  
         MH    R0,10(R1)           Multiply by pages per C/I                    
         STC   R0,PAGEPREV         Set base page number                         
                                                                                
INIT0050 GOTOR DATCON,DMCB,(5,0),(20,WORK)                                      
         MVC   THISYEAR,WORK       Set current year (in ebcdic)                 
                                                                                
INIT0060 L     RF,FTWA             Test initialized                             
         CLC   IDENTIFY,TWAMSG-TWAD(RF)                                         
         JNE   ACTIVE              No - go to activation                        
                                                                                
         SAM31 ,                   Set 31-bit addressing mode                   
                                                                                
         L     R2,FALABLD          R2=A(control field header)                   
         USING FALSCRD,R2                                                       
         USING FHD,FALCONH                                                      
         OI    FHOI,FHOITR                                                      
         CLC   FALCON,SPACES       Control field filled in?                     
         JH    *+14                Yes                                          
         MVC   HALF,=AL2(GE$MISIF)                                              
         J     ERRMSG                                                           
                                                                                
         CLI   FALCONA,FADOWN      Downloading?                                 
         JE    DOWNLOAD                                                         
         CLI   FALCONA,FAUPLD      Uploading?                                   
         JE    UPLOAD                                                           
         CLI   FALCONA,FAVER       Version?                                     
         JE    VERSION                                                          
         CLI   FALCONA,FAVERE      Expanded version?                            
         JE    VERSION                                                          
         CLI   FALCONA,FANVER      Version?                                     
         JE    VERSION                                                          
         CLI   FALCONA,FANVERE     Expanded version?                            
         JE    VERSION                                                          
                                                                                
         MVC   HALF,=AL2(GE$INACT)                                              
         J     ERRMSG                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FALINK activation - build screen and initialize for upload          *         
***********************************************************************         
                                                                                
ACTIVE   XC    SAVE,SAVE                                                        
         XC    DMCB(DMCBL),DMCB                                                 
         LA    R3,DMCB                                                          
         USING TWAPARMD,R3         R3=A(parameter list)                         
                                                                                
         MVC   TWAPATWA,FTWA       Set A(TWA)                                   
         MVC   TWAPAOUT,FALABLD                                                 
         MVC   HDRSCREL,HEADELS    Needs to be in Working stroage               
         LA    RF,HDRSCREL                                                      
         ST    RF,TWAPAFST                                                      
         MVC   TWAPAMAX,=AL4(TWMXDSP)                                           
         GOTOR FALTBLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RF,FALABLD          Pull out row number of control field         
         LLH   RE,FHAD-FHD(RF)                                                  
         SRDL  RE,32                                                            
         D     RE,=F'80'                                                        
         AHI   RF,1                                                             
         STC   RF,FSTLIN#                                                       
                                                                                
         MVC   DATSCREL,DATAELS    Needs to be in Working stroage               
         LA    RF,DATSCREL                                                      
         ST    RF,TWAPAFST                                                      
         SR    R2,R2                                                            
         J     *+8                                                              
ACTIVE02 AHI   R2,1                Keep count of data lines                     
         MVC   TWAPAOUT,TWAPANXT                                                
         GOTOR FALTBLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         JE    ACTIVE02                                                         
         CLI   TWAPERRS,TWAPEEMS                                                
         JNL   *+6                                                              
         DC    H'0'                Errors 1-4 are bad                           
         CLI   TWAPERRS,TWAPEELP                                                
         JNH   ACTIVE04                                                         
         DC    H'0'                Errors 8 up are bad                          
                                                                                
ACTIVE04 MHI   R2,LINLEN                                                        
         AHI   R2,FIRSTLEN                                                      
         ST    R2,NSCRCHAR                                                      
         GOTOR SETMSG                                                           
                                                                                
         XC    OSIN,OSIN           Set no OSIN                                  
         GOTOR GETFACT,DMCB,(X'80',OSIN),F#OSISET,XA=OFF                        
                                                                                
         MVI   PAGEINDX,0          Initialize saved storage values              
         MVI   PAGEHIGH,0                                                       
         XC    ANXTBUFF,ANXTBUFF                                                
         XC    ABUFFLEN,ABUFFLEN                                                
         XC    SAVE,SAVE                                                        
         MVI   SAVELEN,0                                                        
         MVI   SFLAG1,0                                                         
         NI    SFLAG2,SFTMPEST+SFSHARE                                          
         MVI   SFLAG3,0                                                         
                                                                                
ACTIVEX  J     FALKX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FALINK version control                                              *         
***********************************************************************         
                                                                                
         USING FALSCRD,R2                                                       
VERSION  SR    R1,R1                                                            
         LA    RF,FALCONO                                                       
                                                                                
VER02    TM    0(RF),C'0'          First non-number                             
         JNO   VER04                                                            
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         J     VER02               Next character                               
                                                                                
VER04    LTR   R1,R1                                                            
         JNZ   VER06               Not a number                                 
         MVC   HALF,=AL2(GE$NOTN)                                               
         J     ERRMSG                                                           
                                                                                
VER06    BCTR  R1,0                                                             
         EXRL  R1,VERPACK                                                       
         CVB   R1,DUB                                                           
         STC   R1,VRSN                                                          
                                                                                
         MVI   FALCONO,C' '                                                     
         LHI   R1,V4Q              Bad versions use version 4                   
         CLI   FALCONA,FAVER                                                    
         JNE   *+8                                                              
         STC   R1,VRSN                                                          
         CLI   FALCONA,FAVERE                                                   
         JNE   *+8                                                              
         STC   R1,VRSN                                                          
         CLI   FALCONA,FAVERE                                                   
         JNE   *+8                                                              
         MVI   FALCONO,EXPANDED                                                 
         CLI   FALCONA,FANVERE                                                  
         JNE   *+8                                                              
         MVI   FALCONO,EXPANDED                                                 
         MVI   FALCONA,FAUPLD      'vn(n)' implies U O(Y)                       
         MVI   FALCONC,C'O'                                                     
         J     UPLOAD                                                           
                                                                                
VERPACK  PACK  DUB,FALCONO(0)                                                   
         EJECT                                                                  
***********************************************************************         
* Upload process - import data                                        *         
* Ntry: program in 31-bit mode                                        *         
***********************************************************************         
                                                                                
FILL     USING FHD,R8                                                           
UPLOAD   LA    R8,FALCONH                                                       
                                                                                
         CLI   FALCONC,C'O'        First time?                                  
         JE    UPL02               Yes                                          
         TM    FAUPIND,FAUPBULK    Yes                                          
         JO    UPL03               Always partitioned                           
         GOTOR GETPAGE             Read page into buffer                        
         L     R3,FLNK             Point to current position in block           
         A     R3,ANXTBUFF         Current position in block                    
         SR    RF,RF                                                            
         J     UPL03                                                            
                                                                                
UPL02    GOTOR GETFACT,DMCB,(X'80',OSIN),F#OSIMAK,XA=OFF                        
         GOTOR (RF),(R1),(X'80',OSIN),F#OSISET,XA=OFF                           
                                                                                
         L     R3,FLNK             Buffer address                               
         XC    ANXTBUFF,ANXTBUFF                                                
         XC    ABUFFLEN,ABUFFLEN   Clear buffer length                          
         XC    SAVE,SAVE                                                        
         MVI   SAVELEN,0                                                        
         NI    SFLAG2,SFTMPEST+SFSHARE                                          
         MVI   PAGEINDX,0          Set no pages buffered                        
         MVI   PAGEHIGH,0                                                       
         MVC   FRAMENUM,=H'1'                                                   
         SR    RF,RF                                                            
                                                                                
         TM    FAUPIND,FAUPBULK    Yes                                          
         JO    UPL03               Can not do the "some input" check            
         CLI   FILL.FHDA+L'FALCON,C' '                                          
         JH    UPL03               Some input                                   
                                                                                
         USING FAMSGD,RF                                                        
         L     RF,FALAMSG                                                       
         XC    0(FAMSGDL,RF),0(RF)                                              
         MVC   FAMSGNO,=AL2(GE$MISIF)                                           
         MVI   FAMSGSYS,FF                                                      
         MVI   FAMSGTYP,GTMERR                                                  
         MVC   FAMSGXTR,SPACES                                                  
         J     ERRCLI                                                           
                                                                                
***********************************************************************         
*        New bulk upload, with CM veiwer release                                
*        Partition into 18k blocks for TEMPSTR or TEMPEST             *         
*        Pull data out of XA TBUFF.                                   *         
*        Up to three 18K-1 segments of actual data. Less one for FF,  *         
*        which means more data to come in next TEMPEST or TEMPSTR.    *         
*        Can have 4th segement if fits within TBUFFXL (Currntly 60K)  *         
*                                                                     *         
*        AL2(Length) 18K-1 data. If x'80' is on in the HOB of the     *         
*                    length then another segement within buffer       *         
***********************************************************************         
UPL03    GOTOR PROTOFF                                                          
                                                                                
         TM    FAUPIND,FAUPBULK    Yes                                          
         JZ    UPL04                                                            
         SAM31 ,                                                                
         L     RE,FXATBUFF                                                      
         SHI   RE,TBHL                                                          
                                                                                
         USING TBHD,RE                                                          
         LLH   RF,TBHMSGL          Length of message                            
         LA    R6,TBHDATA(RF)      Point to upload data                         
         DROP  RE                                                               
***********************************************************************         
* Note: R6 will be positioned at start of the AL2 length field                  
*       followed by the data in XA TBUFF.                                       
***********************************************************************         
UPL03A   LLH   R7,0(R6)            Get length of message                        
         NILL  GR7,X'7FFF'         Turn off linked indicator                    
         TM    0(R6),X'80'         Another buffer linked?                       
         JZ    *+8                 No, end for this TBUFF                       
         OI    FAUPIND,FAUPMORE    More to process                              
         LR    R1,R7                                                            
         LA    R6,2(,R6)           Bump past length field                       
         L     R0,FLNK             Where to put the message                     
         MVCL  R0,R6                                                            
         LR    R3,R0                                                            
         C     R3,FLNKX            Just incase                                  
         JL    *+6                                                              
         DC    H'00'               Did something wrong                          
                                                                                
         TM    FAUPIND,FAUPMORE    More to go?                                  
         JZ    UPL03B              No                                           
         NI    FAUPIND,FF-FAUPMORE                                              
         MVI   0(R3),FF            Set buffer continued                         
                                                                                
         GOTOR PUTPAGE             Save current buffer                          
         GOTOR NXTOPAG             Get next page                                
         JE    UPL03A              Process next                                 
         J     UPL03C              Error                                        
                                                                                
UPL03B   CLI   FALCONM,C'Y'        More to come?                                
         JNE   UPL16               Done, now process data                       
         MVI   0(R3),FF            Set buffer continued                         
         GOTOR PUTPAGE             and write to disk                            
         GOTOR NXTOPAG             Get next page, for next time through         
         JE    UPL13               Back to PC for more                          
                                                                                
UPL03C   GOTOR PROTON                                                           
         LA    R0,FAEQNMPA         No more pages available                      
         J     ERRMSGR0            Set error message and code                   
                                                                                
***********************************************************************         
* Upload screen at a time                                                       
***********************************************************************         
UPL04    CLI   FILL.FHLN,0         End of screen?                               
         JE    UPL10                                                            
         LA    RF,LINLEN-1         RF=L'line-1                                  
         C     R8,FALABLD          First line?                                  
         JNE   *+8                                                              
         LA    RF,FIRSTLEN-1       RF=L'first line data                         
                                                                                
         L     RE,FLNKX            Will data fit in this page?                  
         SR    RE,R3                                                            
         CR    RE,RF                                                            
         JH    UPL06               Yes                                          
                                                                                
         MVI   0(R3),FF            Set buffer continued                         
                                                                                
         GOTOR PUTPAGE             Save current buffer                          
         GOTOR NXTOPAG             Get next page                                
         JE    UPL05                                                            
         GOTOR PROTON                                                           
         LA    R0,FAEQNMPA         No more pages available                      
         J     ERRMSGR0            Set error message and code                   
                                                                                
UPL05    SAM31 ,                   Set 31-bit addressing mode                   
                                                                                
         L     R3,FLNK             Reset displacement into buffer               
         LHI   RF,LINLEN-1         RF=L'line-1                                  
         C     R8,FALABLD          First line?                                  
         JNE   UPL06                                                            
         LHI   RF,FIRSTLEN-1       RF=L'first line data                         
                                                                                
UPL06    C     R8,FALABLD          If this is the first line                    
         JNE   UPL062                                                           
                                                                                
         EXRL  RF,UFLINE           Move from first line after control           
         J     UPL08                                                            
                                                                                
UPL062   EXRL  RF,ULINE            Move from start of all other lines           
                                                                                
UPL08    ST    R3,FULL             Save start of last line processed            
         LA    R3,1(RF,R3)         Point to next                                
         IC    RF,FILL.FHLN                                                     
         AR    R8,RF               Advance to next TWA field                    
         J     UPL04                                                            
                                                                                
UPL10    CLI   VRSN,V2Q            Version 2 or higher?                         
         JL    UPL12               No - don't do this then                      
                                                                                
         CLI   FSTLIN#,2           Is this one of the bad screens?              
         JNE   UPL12               No                                           
         L     R3,FULL             Yes - ignore last line                       
                                                                                
UPL12    CLI   FALCONM,C'Y'        More to come?                                
         JNE   UPL16                                                            
         MVI   0(R3),EOB           Set end of buffer                            
         GOTOR PUTPAGE             and write to disk                            
                                                                                
UPL13    XC    FALCON+L'FALCON(FIRSTLEN),FALCON+L'FALCON                        
                                                                                
CLR      USING FHD,RF                                                           
         LA    RF,FALCONH          Clear screen                                 
         SR    R1,R1                                                            
         ICM   R1,1,CLR.FHLN                                                    
         JZ    FALKXON             Exit turn protection on                      
         AR    RF,R1               Go past first line                           
                                                                                
UPL14    ICM   R1,1,CLR.FHLN       End of screen?                               
         JZ    FALKXON             Yes, call PROTON                             
         AHI   R1,-(FHDAD+1)                                                    
         TM    CLR.FHAT,FHATXH                                                  
         JZ    *+8                                                              
         AHI   R1,-(FHDAD)                                                      
         EXRL  R1,UPLXC1                                                        
         OI    CLR.FHOI,FHOITR                                                  
         AHI   R1,FHDAD+1                                                       
         TM    CLR.FHAT,FHATXH                                                  
         JZ    *+8                                                              
         AHI   R1,FHDAD                                                         
         JXH   RF,R1,UPL14                                                      
         DC    H'0'                                                             
                                                                                
UPLXC1   XC    CLR.FHDA(0),CLR.FHDA                                             
         DROP  CLR                                                              
                                                                                
UPL16    MVI   0(R3),EOB           Set end of buffer                            
         GOTOR PUTPAGE             Write current buffer page                    
                                                                                
***********************************************************************         
* Done storing upload, now process                                              
***********************************************************************         
         XC    ANXTBUFF,ANXTBUFF                                                
         MVC   CLIHOOK,FALARCV     Call client's upload routine                 
         LARL  R0,GETDATA                                                       
         XC    DMCB(DMCBL),DMCB                                                 
         GOTOR PROTON                                                           
         GOTOR CLIENT,DMCB,(R0)                                                 
         JNE   ERRCLI              Error from client                            
                                                                                
         GOTOR PROTOFF                                                          
         SAM31 ,                   Initialize buffer                            
         L     R0,FLNK                                                          
         LH    R1,PAGESIZE                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR PROTON                                                           
                                                                                
         XC    ANXTBUFF,ANXTBUFF                                                
         XC    FALCON+L'FALCON(FIRSTLEN),FALCON+L'FALCON                        
         OI    FALCONH+(FHOI-FHD),FHOITR                                        
                                                                                
CLR      USING FHD,RF                                                           
         LA    RF,FALCONH          Clear screen                                 
         SR    R1,R1                                                            
         ICM   R1,1,CLR.FHLN                                                    
         JZ    DLD18                                                            
         AR    RF,R1               Go past first line                           
                                                                                
UPL18    ICM   R1,1,CLR.FHLN       End of screen?                               
         JZ    DOWNLOAD            Yes                                          
         AHI   R1,-(FHDAD+1)                                                    
         TM    CLR.FHAT,FHATXH                                                  
         JZ    *+8                                                              
         AHI   R1,-(FHDAD)                                                      
         EXRL  R1,UPLXC2                                                        
         OI    CLR.FHOI,FHOITR                                                  
         AHI   R1,FHDAD+1                                                       
         TM    CLR.FHAT,FHATXH                                                  
         JZ    *+8                                                              
         AHI   R1,FHDAD                                                         
         JXH   RF,R1,UPL18                                                      
         DC    H'0'                                                             
                                                                                
UPLXC2   XC    CLR.FHDA(0),CLR.FHDA                                             
UFLINE   MVC   0(0,R3),FILL.FHDA+L'FALCON                                       
ULINE    MVC   0(0,R3),FILL.FHDA                                                
         DROP  CLR,FILL                                                         
         EJECT                                                                  
***********************************************************************         
* Download process - import data to download                          *         
* Ntry: R2=A(control field header)                                    *         
***********************************************************************         
                                                                                
         USING FALSCRD,R2                                                       
DOWNLOAD MVC   HALF,=AL2(GE$ISEQ)                                               
         CLI   FALCONC,FCDONE      Finished?                                    
         JE    ERRMSG              Error condition                              
         CLI   FALCONC,FCZERO      Nothing to download?                         
         JE    ERRMSG              Error condition                              
                                                                                
         TM    FALAINDS,FALALVER   Test use latest download version             
         JZ    *+8                                                              
         MVI   VRSN,VERSIONQ                                                    
                                                                                
         MVI   FALCONA,FADOWN      Set downloading                              
         XC    HALF,HALF                                                        
         L     R3,FLNK             Start of data block                          
         CLI   FALCONM,YES         Download already in progress?                
         JE    DLD12               Yes                                          
                                                                                
         TM    SFLAG2,SFULPQ                                                    
         JZ    DLD02                                                            
         MVC   CLIHOOK,FALAFUL     Call client's overflow routine               
         L     R1,CALLR1                                                        
         L     R1,0(R1)                                                         
         MVI   MAHMOUNS-FALINKD(R1),FF                                          
         GOTOR CLIENT,0            Allow client to be ready to resume           
         JNE   ERRCLI              Error from client                            
         NI    SFLAG2,FF-(SFULPQ+SFLSTQ)                                        
         J     DLD04                                                            
                                                                                
DLD02    TM    SFLAG2,SFSSTPRQ+SFSGLBRQ                                         
         JZ    DLD04                                                            
         MVC   CLIHOOK,FALARSM     Call client's resume routine                 
         GOTOR CLIENT,0                                                         
         JNE   ERRCLI              Error from client                            
         NI    SFLAG2,255-(SFSSTPRQ+SFSGLBRQ)                                   
                                                                                
         GOTOR GETPAGE             Read page into buffer                        
                                                                                
         L     RF,ANXTBUFF         Get displacement to next in buffer           
         A     RF,FLNK                                                          
         ST    RF,ANXTBUFF         Set address of next data                     
         J     DLD06                                                            
                                                                                
DLD04    MVC   ANXTBUFF,FLNK       Next available slot=first position           
         XC    ABUFFLEN,ABUFFLEN   Clear buffer length                          
         XC    APREVLEN,APREVLEN   Clear last buffer length                     
         XC    SAVE,SAVE           Initialize other data fields...              
         MVI   SFLAG1,0                                                         
         MVI   SAVELEN,0                                                        
         MVI   PAGEINDX,0                                                       
         MVI   PAGEHIGH,0                                                       
         MVC   FRAMENUM,=H'1'                                                   
                                                                                
DLD06    MVC   CLIHOOK,FALASND     Call client's download routine               
         LARL  R0,ADDDATA                                                       
         LARL  RF,SETELEM                                                       
         XC    DMCB(DMCBL),DMCB                                                 
         GOTOR CLIENT,DMCB,(RF),(R0)                                            
         JNE   ERRCLI              Error from client?                           
                                                                                
         TM    SFLAG3,SF2USN+SF2YRF                                             
         JZ    DLD08                                                            
         GOTOR PUTPAGE             Ensure this page saved                       
         L     RF,ANXTBUFF                                                      
         S     RF,FLNK                                                          
         ST    RF,ANXTBUFF         Set next displacement                        
         GOTOR YRFMSG                                                           
         NI    SFLAG3,255-(SF2USN+SF2YRF)                                       
         J     ERRFALK                                                          
                                                                                
DLD08    TM    SFLAG2,SFSSTPRQ+SFSGLBRQ                                         
         JZ    DLD10                                                            
         GOTOR PUTPAGE             Ensure this page saved                       
         L     RF,ANXTBUFF         Current displacement                         
         S     RF,FLNK                                                          
         ST    RF,ANXTBUFF         Set next displacement                        
         J     FALXIT                                                           
                                                                                
DLD10    ICM   R3,15,FLNK          Start of data block                          
         JZ    ERRFALK                                                          
         MVI   FALCONC,FCZERO      Nothing in buffers                           
         CLI   PAGEINDX,0          Test any pages buffered                      
         JNE   *+12                Yes                                          
         C     R3,ANXTBUFF         Test anything at all                         
         JE    FALKX               No                                           
                                                                                
         MVI   FALCONC,FCFRST      First time                                   
         L     RE,ANXTBUFF         RE=A(current slot)                           
         S     RE,FLNK             RE=length used this buffer                   
         A     RE,ABUFFLEN         Add previous pages                           
         ST    RE,ABUFFLEN         And save                                     
         XC    APREVLEN,APREVLEN                                                
         MVC   ANXTBUFF,FLNK       Set current pointer                          
         J     DLD14                                                            
         EJECT                                                                  
***********************************************************************         
* Download process - export data                                      *         
***********************************************************************         
                                                                                
DLD12    MVI   FALCONC,C' '        Not first time                               
         OI    FALCONH+(FHOI-FHD),FHOITR                                        
         GOTOR GETPAGE             Read page into buffer                        
         L     R3,FLNK                                                          
         A     R3,ANXTBUFF         Current position in block                    
                                                                                
DLD14    XC    FALCON+L'FALCON(FIRSTLEN),FALCON+L'FALCON                        
                                                                                
         LA    RF,FALCONH          Clear screen                                 
CLR      USING FHD,RF                                                           
         SR    R1,R1                                                            
         ICM   R1,1,CLR.FHLN                                                    
         JZ    DLD18                                                            
         AR    RF,R1               Go past first line                           
                                                                                
DLD16    ICM   R1,1,CLR.FHLN       End of screen?                               
         JZ    DLD18               Yes                                          
         AHI   R1,-(FHDAD+1)                                                    
         TM    CLR.FHAT,FHATXH                                                  
         JZ    *+8                                                              
         AHI   R1,-(FHDAD)                                                      
         EXRL  R1,DLDCLR                                                        
         OI    CLR.FHOI,FHOITR                                                  
         AHI   R1,FHDAD+1                                                       
         TM    CLR.FHAT,FHATXH                                                  
         JZ    *+8                                                              
         AHI   R1,FHDAD                                                         
         JXH   RF,R1,DLD16                                                      
         DC    H'0'                                                             
                                                                                
DLDCLR   XC    CLR.FHDA(0),CLR.FHDA                                             
         DROP  CLR                                                              
                                                                                
DLD18    GOTOR PUTPAGE             Write last page                              
                                                                                
         CLI   FALCONC,C' '        First time?                                  
         JE    DLD20               No                                           
                                                                                
         CLI   PAGEINDX,1          Test first page in buffer                    
         JE    DLD20                                                            
         MVI   PAGEINDX,0          Set to get first page in buffer              
         GOTOR GETPAGE             Read page into buffer                        
                                                                                
DLD20    LA    R8,FALCONH          R8=A(control field header)                   
FILL     USING FHD,R8                                                           
                                                                                
         MVI   FALCONM,C' '        Remove continuation indicator                
         MVI   EOBYTE,0            Clear buffer end indicator                   
                                                                                
DLD22    ST    R8,AFILL            Save A(current fill line)                    
         LA    RF,FILL.FHDA                                                     
         C     R8,FALABLD          First line?                                  
         JNE   *+8                                                              
         AHI   RF,L'FALCON                                                      
         AH    RF,LENMOVED         Add any partial used                         
         ST    RF,FILLFROM         Set A(to start filling from)                 
                                                                                
         LA    RF,LINLEN                                                        
         C     R8,FALABLD          First line?                                  
         JNE   *+8                                                              
         LA    RF,FIRSTLEN                                                      
         SH    RF,LENMOVED                                                      
         STH   RF,FILLMAX          Set max chars available on this line         
                                                                                
         TM    EOBYTE,EOBUFF1      End of buffer within curr line?              
         JO    *+12                Yes                                          
         CLI   0(R3),EOB           End of buffer at start of curr line?         
         JNE   DLD24               No                                           
                                                                                
         NI    EOBYTE,FF-(EOBUFF1)                                              
         GOTOR NXTIPAG             Get next buffer page                         
         JNE   FALKX               No more pages                                
         L     R3,FLNK             Reset current displacement                   
                                                                                
DLD24    L     R1,FALABLD          Test expanded download                       
         CLI   FALCONO-FALSCRD(R1),EXPANDED                                     
         JE    EDLD02              Yes - handle differently                     
                                                                                
         LH    RE,FILLMAX          RE=L'available on line                       
         LR    RF,R3                                                            
                                                                                
DLD26    CLI   0(RF),EOB           Test end of buffer on this line              
         JE    DLD28                                                            
         LA    RF,1(RF)                                                         
         JCT   RE,DLD26                                                         
         LH    RE,FILLMAX          RE=L'line                                    
         J     DLD30               No buffer end on this line                   
                                                                                
DLD28    OI    EOBYTE,EOBUFF1      Set end of buffer found on this line         
         SR    RF,R3                                                            
         LR    RE,RF                                                            
                                                                                
DLD30    BCTR  RE,0                                                             
         L     RF,FILLFROM                                                      
         EXRL  RE,DLDMVCF3                                                      
         AHI   RE,1                                                             
         LA    R3,0(RE,R3)         Advance buffer pointer                       
         CH    RE,FILLMAX          Did we fill line completely?                 
         JE    DLD32               Yes                                          
         AH    RE,LENMOVED                                                      
         STH   RE,LENMOVED         Set new length moved on line                 
         L     R8,AFILL                                                         
         J     DLD22                                                            
                                                                                
DLDMVCF3 MVC   0(0,RF),0(R3)                                                    
                                                                                
DLD32    XC    LENMOVED,LENMOVED   Reset length moved                           
         SR    R1,R1                                                            
         ICM   R1,1,FILL.FHLN                                                   
         JZ    FALKX               Stop if length is zero                       
         AR    R8,R1                                                            
         CLI   FILL.FHLN,0         Screen end reached?                          
         JNE   DLD22               No                                           
         J     FALKX                                                            
         EJECT                                                                  
***********************************************************************         
* Expanded download                                                   *         
***********************************************************************         
                                                                                
EDLD02   LA    R7,FILL.FHDA                                                     
                                                                                
EDLD04   CLI   0(R3),EOB           Test end of current page                     
         JNE   EDLD06                                                           
         GOTOR NXTIPAG             Get next saved TWA page number               
         JNE   FALKX               No more pages                                
         L     R3,FLNK                                                          
                                                                                
EDLD06   CLI   1(R3),ELEMIDQ       Current buffer item is record?               
         JNE   EDLD10              No                                           
         GOTOR EDNXTLIN            New record on new line                       
                                                                                
         MVC   BYTE,0(R3)                                                       
         TR    BYTE,INVLENS                                                     
         CLI   BYTE,FF                                                          
         JNE   *+12                                                             
         LA    R0,FAEQINVL                                                      
         J     ERRMSGR0            Set error message and code                   
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,BYTE                                                        
         JZ    EDLD08                                                           
         BCTR  RF,0                                                             
         EXRL  RF,EDLMV731                                                      
                                                                                
EDLD08   LA    R7,2(RF,R7)         Bump along line                              
         LA    R3,3(RF,R3)         Update buffer pointer                        
         J     EDLD04                                                           
                                                                                
EDLD10   LA    R1,FALCON                                                        
         CR    R1,R7               First line?                                  
         JNE   EDLD102             No                                           
         GOTOR EDNXTLIN            Go on to next line                           
                                                                                
EDLD102  CLI   0(R3),ECHARQ        End character?                               
         JNE   *+12                No                                           
         AHI   R3,1                Ignore it                                    
         J     EDLD04                                                           
                                                                                
         SR    RE,RE               Length increment                             
         CLI   0(R3),SCHARQ        Ignore optimization indicators               
         JNE   *+8                                                              
         AHI   R3,1                                                             
         ST    R3,FULL             Save A(length code)                          
                                                                                
EDLD12   CLI   0(R3),LCHAR         Multiple chararacter length?                 
         JNE   EDLD14                                                           
         AHI   RE,LCHARL           Increment length                             
         AHI   R3,1                                                             
         J     EDLD12                                                           
                                                                                
EDLD14   MVC   BYTE,0(R3)                                                       
         TR    BYTE,INVLENS                                                     
         CLI   BYTE,FF             Test length error                            
         JNE   *+12                                                             
         LA    R0,FAEQLERR                                                      
         J     ERRMSGR0                                                         
                                                                                
         LLC   RF,BYTE             RF=remaining length                          
         TM    FALAINDS,FALAINDD   Test type embedded in data                   
         JZ    *+8                                                              
         AHI   RF,1                Yes - adjust length                          
         LA    RE,0(RF,RE)         Re=total data length                         
         STH   RE,HALF             Save it                                      
         AHI   RE,L'MDTEXT+1       Add header length                            
                                                                                
         LLC   RF,FILL.FHLN        Length of this field                         
         LA    RF,FILL.FHD(RF)                                                  
         SR    RF,R7               Rf=remaining space on this line              
                                                                                
         CR    RE,RF               Will data fit onto this line?                
         JH    EDLD18              No                                           
*++++++++++++++++++++++++++++++++++++++++++                                     
* IF WE GET HERE, MEANS THE DATA WILL FIT ON THIS LINE                          
*++++++++++++++++++++++++++++++++++++++++++                                     
         MVC   0(L'MDTEXT+1,R7),1(R3)                                           
         AHI   R3,L'MDTEXT+2                                                    
         AHI   R7,L'MDTEXT+1       Move in header and bump pointers             
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,HALF           Get length of data                           
         JZ    EDLD16              No data                                      
                                                                                
         BCTR  RE,0                                                             
         EXRL  RE,EDLMV732                                                      
         AHI   RE,1                                                             
         LA    R3,0(RE,R3)         Move current position                        
         LA    R7,0(RE,R7)                                                      
                                                                                
EDLD16   LLC   RF,FILL.FHLN        Length of this field                         
         LA    RF,FILL.FHD(RF)                                                  
         SR    RF,R7               RF=remaining space on this line              
*                                                                               
* Code commented out, no need to skip a line - HWON 08/18/2015                  
*        JNZ   EDLD162                                                          
*        GOTOR EDNXTLIN            Get next line                                
*        J     EDLD04              And continue                                 
* Code commented out, no need to skip a line - HWON 08/18/2015                  
*                                                                               
         JZ    EDLD04              No, get next output data                     
                                                                                
EDLD162  MVI   0(R7),C','          yes, output comma                            
         AHI   R7,1                bump output location                         
         J     EDLD04              and get next output data                     
                                                                                
*++++++++++++++++++++++++++++++++++++++++++                                     
* IF WE GET HERE, MEANS THE DATA WILL NOT FIT ON THIS LINE                      
*++++++++++++++++++++++++++++++++++++++++++                                     
EDLD18   LA    RF,FILL.FHDA                                                     
         CR    RF,R7               At start of a new line?                      
         JE    EDLD19              Yes,                                         
*                                                                               
         LA    RF,3(RF)            Point 3 bts beyond start of new line         
         CR    RF,R7               and see if line only has a record?           
         JE    EDLD19              Yes, consider it a new line                  
         L     R3,FULL                                                          
         J     EDLD24              Get new line and try again                   
                                                                                
EDLD19   MVC   0(L'MDTEXT+1,R7),1(R3)                                           
         AHI   R7,L'MDTEXT+1                                                    
         AHI   R3,L'MDTEXT+2                                                    
         LLC   RF,FILL.FHLN        Length of this field                         
         LA    RF,FILL.FHD(RF)                                                  
         SR    RF,R7               RF=remaining space on this line              
                                                                                
EDLD20   BCTR  RF,0                                                             
         EXRL  RF,EDLMV732                                                      
         AHI   RF,1                                                             
         LA    R3,0(RF,R3)         Move current position                        
         LA    R7,0(RF,R7)         Move current position                        
         LH    RE,HALF                                                          
         SR    RE,RF               Reduce total data length                     
         JZ    EDLD22              We are done with data                        
         STH   RE,HALF             otherwise, save the leftover length          
                                                                                
         LR    R0,R3               Save current pointer                         
         L     R3,FULL             If screen end reached,redisplay line         
         GOTOR EDNXTLIN            Get next line                                
         LR    R3,R0               Otherwise, restore pointer                   
                                                                                
         LH    RE,HALF             Get data length                              
         LLC   RF,FILL.FHLN        Length of this field                         
         LA    RF,FILL.FHD(RF)                                                  
         SR    RF,R7               RF=remaining space on this line              
         CR    RE,RF               Will data fit onto this line?                
         JH    EDLD20              No, output for length of this field          
         LR    RF,RE               Yes, only output remaining data              
         J     EDLD20                                                           
                                                                                
EDLD22   LLC   RF,FILL.FHLN        Length of this field                         
         LA    RF,FILL.FHD(RF)                                                  
         SR    RF,R7               RF=remaining space on this line              
         JNZ   EDLD222                                                          
         GOTOR EDNXTLIN            Get next line                                
         J     EDLD04              And continue                                 
                                                                                
EDLD222  MVI   0(R7),C','                                                       
         AHI   R7,1                                                             
         J     EDLD04                                                           
                                                                                
EDLD24   BCTR  R7,0                Remove any trailing comma                    
         CLI   0(R7),C','                                                       
         JNE   *+8                                                              
         MVI   0(R7),C' '                                                       
         GOTOR EDNXTLIN            Next line on screen                          
         J     EDLD02                                                           
                                                                                
EDNXTLIN SR    RF,RF               Get next line on screen                      
         ICM   RF,1,FILL.FHLN      Length of this field is 0?                   
         JZ    FALKX               Yes, stop                                    
         LA    R8,0(RF,R8)                                                      
         LA    R7,FILL.FHDA        Set R7 to new line start                     
         CLI   FILL.FHLN,0         Screen end reached?                          
         BNER  RE                  no                                           
         J     FALKX                                                            
                                                                                
EDLMV731 MVC   0(0,R7),2(R3)                                                    
EDLMV732 MVC   0(0,R7),0(R3)                                                    
         EJECT                                                                  
***********************************************************************         
* Return to caller                                                    *         
***********************************************************************         
FALKXON  GOTOR PROTON                                                           
                                                                                
FALKX    SAM24 ,                   Back to 24 bit addressing                    
                                                                                
         L     R2,FALABLD          R2=A(control field header)                   
         L     RE,FLNK                                                          
         SR    R3,RE                                                            
         ST    R3,ANXTBUFF         R3=disp into current page for next           
         GOTOR SETMSG                                                           
                                                                                
         L     R3,FTWA                                                          
         AHI   R3,TWAMSG+L'TWAMSG-1-TWAD                                        
         CLI   0(R3),C' '          Get next slot                                
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         MVI   1(R3),C' '                                                       
         AHI   R3,2                                                             
                                                                                
         CLI   FALCONA,FADOWN      Downloading?                                 
         JNE   FALXIT              No                                           
         CLI   FALCONC,FCDONE      Finished?                                    
         JE    FALXIT              Yes                                          
                                                                                
         L     R0,APREVLEN                                                      
         A     R0,ANXTBUFF                                                      
         ST    R0,FULL             Edit length transmitted                      
         CURED FULL,(6,0(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                         
                                                                                
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         AHI   R3,1                Edit total length                            
         CURED ABUFFLEN,(6,0(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         AHI   R3,1                                                             
         OC    FRAMENUM,FRAMENUM   Frame information?                           
         JZ    FALKX02                                                          
                                                                                
         CURED FRAMENUM,(3,0(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         AHI   R3,1                                                             
         SR    RE,RE                                                            
         ICM   RE,3,FRAMENUM                                                    
         AHI   RE,1                                                             
         STCM  RE,3,FRAMENUM                                                    
         L     R0,ABUFFLEN                                                      
         SRDL  R0,32                                                            
         D     R0,NSCRCHAR                                                      
         LA    RF,1(R1)            Total number of frames                       
         CURED (RF),(3,0(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         AHI   R3,1                                                             
                                                                                
FALKX02  TM    SFLAG2,SFULPQ       Size break?                                  
         JZ    *+10                No                                           
         MVC   0(L'MOREMSG,R3),MOREMSG                                          
         L     R2,FALABLD                                                       
         CLC   ABUFFLEN,FULL       Any more data to come out?                   
         JNH   *+12                                                             
         MVI   FALCONM,C'Y'        Set continuation to come                     
         J     FALXIT                                                           
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,FALAKEY+1      Reset current page number                    
         JZ    FALKX06                                                          
         CLI   VRSN,V4Q            Version 4 or higher?                         
         JNL   FALKX04                                                          
         GOTOR YRFMSG              No - you can't do that                       
         J     FALXIT                                                           
                                                                                
FALKX04  SR    R1,R1                                                            
         ICM   R1,1,FALAKEY                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EXRL  R1,FALMVC3F         Move key to message field                    
                                                                                
FALKX06  MVI   PAGEINDX,0          Reset current page index                     
         MVI   PAGEHIGH,0                                                       
         XC    ANXTBUFF,ANXTBUFF   and lengths                                  
         XC    APREVLEN,APREVLEN                                                
         TM    SFLAG2,SFULPQ       Size break?                                  
         JZ    FALXIT              No                                           
         MVI   FALCONM,MORE        Set require more processing                  
         MVI   FALCONC,C' '                                                     
         J     FALXIT                                                           
                                                                                
FALMVC3F MVC   0(0,R3),0(RF)       Move key to message field                    
         EJECT                                                                  
***********************************************************************         
* Set FALINK error messages and exit to caller                        *         
***********************************************************************         
                                                                                
ERRMSG   SAM24 ,                                                                
         MVC   FALCON,ERRF         Set FALINK internal error                    
         L     RF,FALAMSG                                                       
         USING FAMSGD,RF                                                        
         XC    0(FAMSGDL,RF),0(RF)                                              
         MVC   FAMSGNO,HALF        Expects message # to be in half              
         MVI   FAMSGSYS,FF         Always general message                       
         MVI   FAMSGTYP,GTMERR     Always error                                 
         MVC   FAMSGXTR,SPACES                                                  
         J     FALKERR                                                          
         DROP  RF                                                               
                                                                                
ERRMSGR0 SAM24 ,                                                                
         MVC   FALCON,ERRF         Set FALINK internal error                    
         L     RF,FALAMSG          Plus error code                              
         USING FAMSGD,RF                                                        
         XC    0(FAMSGDL,RF),0(RF)                                              
         MVI   FAMSGSYS,FF         Always general message                       
         MVI   FAMSGTYP,GTMERR     Always error                                 
         MVC   FAMSGXTR,SPACES                                                  
         MVC   FAMSGNO,=AL2(GE$IPTL)                                            
         CHI   R0,FAEQNMPA         If no more pages available                   
         JE    FALKERR             use 'input too long' message                 
         MVC   FAMSGNO,=AL2(GE$FLERR) else 'FALINK error' with code             
         CURED (R0),(4,FAMSGXTR),0,ZERO=NOBLANK,ALIGN=LEFT                      
         J     FALKERR                                                          
         DROP  RF                                                               
                                                                                
ERRFALK  MVC   FALCON,ERRF         FALINK error                                 
         J     FALKERR                                                          
                                                                                
ERRCLION GOTOR PROTON                                                           
*                                                                               
ERRCLI   MVC   FALCON,ERRC         Client routine error                         
         J     FALKERR                                                          
                                                                                
FALKERR  SAM24 ,                   Call GETTXT                                  
         LA    RE,FALCONH                                                       
         OI    FHOI-FHD(RE),FHOITR                                              
         AHI   RE,L'FALCONH+L'FALCON                                            
         MVC   0(FIRSTLEN-1,RE),SPACES                                          
                                                                                
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         STCM  RE,7,GTAOUT                                                      
         MVI   GTMAXL,FIRSTLEN     Set maximum message length                   
         L     RE,FALAMSG                                                       
         USING FAMSGD,RE                                                        
         MVC   GTMSGNO,FAMSGNO     Set message number                           
         MVC   GTMTYP,FAMSGTYP     Set message type                             
         OC    GTMTYP,GTMTYP                                                    
         JNZ   *+8                                                              
         MVI   GTMTYP,GTMERR       Default type is error                        
         MVC   GTMSYS,FAMSGSYS     Set message system                           
                                                                                
         LA    R0,FAMSGPRM                                                      
         STCM  R0,7,GTASUBST       Set substitution parameters                  
                                                                                
         LA    RF,FAMSGXTR+L'FAMSGXTR-1                                         
         LHI   R0,L'FAMSGXTR                                                    
         CLI   0(RF),C' '          Get length of extra text                     
         JH    *+14                                                             
         BCTR  RF,0                                                             
         JCT   R0,*-10                                                          
         J     FALKER02            Nothing to send                              
                                                                                
         LA    RF,FAMSGXTR                                                      
         STCM  RF,7,GTATXT         Set length & address of extra text           
         STCM  R0,1,GTLTXT                                                      
                                                                                
FALKER02 OI    GT1INDS,GT1OWRK                                                  
         GOTOR GETTXT,WORK         Build message                                
         J     FALXIT                                                           
         DROP  R1,RE                                                            
         EJECT                                                                  
***********************************************************************         
* Exit                                                                          
***********************************************************************         
FALXIT   CLI   FALCONA,FADOWN      Downloading?                                 
         JNE   FALXIT10            No                                           
                                                                                
         TM    SFLAG2,SFSSTPRQ+SFSGLBRQ                                         
         JNZ   FALXIT10                                                         
         CLI   FALCONM,C'Y'        More to come?                                
         JE    FALXIT02            Yes                                          
         CLI   FALCONC,FCZERO      Nothing to come?                             
         JE    FALXIT02            Yes                                          
         TM    SFLAG2,SFULPQ       Test break on full                           
         JNZ   FALXIT02                                                         
         MVI   FALCONC,FCDONE      Set finished                                 
                                                                                
         USING UTLD,RF                                                          
FALXIT02 L     RF,FUTL             Can we use binary transfer?                  
         TM    TSTAT8,TST8BINT                                                  
         JZ    FALXIT10            No                                           
         DROP  RF                                                               
                                                                                
         LA    R1,DMCB                                                          
         USING SHIPBLKD,R1                                                      
         LA    RE,SAVESHIP                                                      
         ST    RE,SHIPSHIP                                                      
                                                                                
         TM    FALAINDS,FALAINDZ   App says bad compression?                    
         JZ    *+8                 No                                           
         OI    SHIPFLAG,SHIPFNGC                                                
                                                                                
         CLC   FALCON,ERRF         FALINK internal error?                       
         JE    FALXIT04                                                         
         CLC   FALCON,ERRC         FALINK client error?                         
         JE    FALXIT04                                                         
         CLI   FALCONC,FCDONE      Finished?                                    
         JE    FALXIT04                                                         
         CLI   FALCONC,FCZERO      Nothing to download?                         
         JE    FALXIT04                                                         
                                                                                
         TM    SFLAG2,SFULPQ       Test break on full                           
         JZ    FALXIT06                                                         
         CLI   FALCONM,MORE        More to come?                                
         JNE   FALXIT06                                                         
                                                                                
FALXIT04 OI    SHIPFLAG,SHIPFEND   Last for this compression                    
                                                                                
FALXIT06 CLI   FALCONC,FCFRST      First time                                   
         JNE   *+8                                                              
         OI    SHIPFLAG,SHIPFFST                                                
         XC    LENMOVED,LENMOVED                                                
                                                                                
FALXIT08 L     RF,SHIPIT                                                        
         BASSM RE,RF                                                            
         JNE   FALXIT10            Cannot continue                              
         SAM31 ,                                                                
         J     DOWNLOAD                                                         
         DROP  R1                                                               
                                                                                
FALXIT10 L     R1,FALACON          Copy control field to buffer                 
         L     RF,FALABLD                                                       
         MVC   0(L'FALCON,R1),FHDAD(RF)                                         
                                                                                
         TM    FALAINDS,FALAINDX   $ABEND request                               
         JO    FALXMODA                                                         
         SAM24 ,                   Back to 24-bit mode                          
         L     RD,CALLRD           Return to calling program                    
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Build error message                                                 *         
***********************************************************************         
                                                                                
SETMSG   NTR1  LABEL=*                                                          
         L     R2,FTWA                                                          
         USING TWAD,R2                                                          
         MVC   TWAMSG,SPACES                                                    
         MVC   TWAMSG(L'IDENTIFY),IDENTIFY                                      
         LLC   R0,FSTLIN#          Give line number of control field            
         LA    R3,TWAMSG+L'IDENTIFY+1                                           
         CURED (R0),(6,0(R3)),0,ALIGN=LEFT                                      
         AR    R3,R0                                                            
         AHI   R3,1                                                             
         LHI   R0,VERSIONQ         Give FALINK version number                   
         CURED (R0),(6,0(R3)),0,ALIGN=LEFT                                      
         OI    TWAMSGH+(FHOI-FHD),FHOITR                                        
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Build version error message                                         *         
***********************************************************************         
                                                                                
YRFMSG   NTR1  LABEL=*                                                          
         L     R2,FALAMSG                                                       
         USING FAMSGD,R2                                                        
         MVC   FAMSGNO,=Y(GE$YRF)  Set required message                         
         TM    SFLAG3,SF2USN                                                    
         JZ    *+10                                                             
         MVC   FAMSGNO,=Y(GE$USN)                                               
         MVI   FAMSGSYS,FF                                                      
                                                                                
         MVI   FAMSGTYP,GTMERR                                                  
         ICM   R3,15,FAMSGXTR                                                   
         JNZ   *+14                                                             
         MVC   FAMSGXTR,SPACES                                                  
         J     YRFMSGX                                                          
                                                                                
         MVC   FAMSGXTR,SPACES     Output version as XXX.XXX.XXX.XXX            
         LA    R4,FAMSGXTR                                                      
         LHI   R5,4                                                             
                                                                                
YRFMSG02 LR    R0,R3                                                            
         SRL   R0,32-8                                                          
         CURED (R0),(3,(R4)),0,ZERO=NOBLANK,ALIGN=LEFT                          
         AR    R4,R0                                                            
         MVI   0(R4),C'.'                                                       
         AHI   R4,1                                                             
         SLL   R3,8                                                             
         JCT   R5,YRFMSG02                                                      
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
                                                                                
YRFMSGX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Client routine: passes control back to client's up/download routine *         
* Ntry: R1       = parameters to pass to client                       *         
* Exit: CC set by client                                              *         
***********************************************************************         
                                                                                
CLIENT   NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS          Point to globals                             
         L     RE,FALACON          Copy control field for client                
         L     RF,FALABLD                                                       
         MVC   0(L'FALCON,RE),FHDAD(RF)                                         
         SAM24 ,                   Force 24-bit addressing in client            
         GOTOR GOCLI               Call client                                  
         IPM   R0                  Save condition code                          
         SAM31 ,                   Set 31 bit addressing mode                   
         SPM   R0                  Restore saved condition code                 
         J     EXIT                                                             
                                                                                
GOCLI    NTR1  LABEL=*             Call client's processing routine             
         L     RF,CLIHOOK                                                       
         L     RE,CLIENTRD                                                      
         LM    R2,RC,28(RE)                                                     
         BASR  RE,RF                                                            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Set new element/record code and add it to the output buffer         *         
*                                                                     *         
* Ntry: P1: B 0-3: A(FALINKBLK)                                       *         
*       P2: B 0-3: A(record header field)                             *         
*       P3: B 0-3: 0  (gives default record code)                     *         
*             or                                                      *         
*       P3: B 0  : length of override string (must be <60 bytes)      *         
*           B 1-3: A(override string)                                 *         
***********************************************************************         
                                                                                
SETELEM  NTR1  LABEL=*,BASE=*                                                   
         LARL  RA,GLOBALS          Point to globals                             
         ICM   R9,15,0(R1)         A(FALINKBLK is 1st parameter)                
         ICM   R9,15,FALASVE-FALINKD(R9)                                        
         L     RC,AWORKD           Point to local storage                       
                                                                                
         SAM31 ,                                                                
                                                                                
         L     R2,4(R1)            R2=A(record header entry)                    
         USING MHELD,R2                                                         
                                                                                
         TM    SFLAG1,SFSDPEND     Data write pending?                          
         JZ    SETEL02                                                          
         TM    SFLAG1,SFBOS        Within series                                
         JZ    *+8                                                              
         OI    SFLAG1,SFECHAR      Add end character                            
         GOTOR DWRITE              Write pending data                           
                                                                                
         NI    SFLAG1,FF-(SFBOS+SFSCHAR+SFECHAR+SFSDPEND)                       
         XC    SAVE,SAVE                                                        
         MVI   SAVELEN,0                                                        
                                                                                
SETEL02  SR    RF,RF                                                            
         ICM   RF,1,8(R1)          Overriding code?                             
         JZ    SETEL04             No                                           
         BCTR  RF,0                RF=L'-1 of override code                     
         SR    RE,RE                                                            
         ICM   RE,7,9(R1)                                                       
         EXRL  RF,SETMVCEE         Copy in override string                      
         AHI   RF,1                                                             
         J     SETEL10                                                          
                                                                                
SETMVCEE MVC   ELEMENT+2(0),0(RE)                                               
                                                                                
SETEL04  ICM   R1,3,MHCODE                                                      
         JNZ   *+6                                                              
         DC    H'0'                Record code cannot be zero                   
                                                                                
         GOTOR HEXOUT,DMCB,MHCODE,WORK,L'MHCODE,0                               
         ORG   *-2                                                              
         BASSM RE,RF                                                            
         SAM31 ,                                                                
         OC    16(4,R1),16(R1)                                                  
         JNZ   *+6                                                              
         DC    H'0'                Invalid hexout                               
         LHI   RF,L'MHCODE*2       RF=L'output string                           
                                                                                
SETEL06  CLC   =C'00',WORK         Strip leading zero pair(s)                   
         JNE   SETEL08                                                          
         AHI   RF,-3               RF=RF-1                                      
         EXRL  RF,SETMVCWW                                                      
         AHI   RF,1                RF=RF-2                                      
         J     SETEL06                                                          
                                                                                
SETEL08  BCTR  RF,0                                                             
         EXRL  RF,SETMVCEW         Copy in record code                          
         AHI   RF,1                                                             
                                                                                
SETEL10  STC   RF,ELEMENT          Set record code length                       
         LA    R2,2(RF)            R2=total length                              
         MVI   ELEMENT+1,ELEMIDQ   Set record identifier                        
         TR    ELEMENT(1),LENGTHS  Translate length                             
                                                                                
         GOTOR PROTOFF             Client runs with protection on               
                                                                                
         ICM   R3,15,ANXTBUFF      Next available slot                          
         L     RE,FLNKX                                                         
         BCTR  RE,0                RE=A(byte following FLNK)                    
         SR    RE,R3               RE=length remaining in buffer                
         CR    RE,R2                                                            
         JH    SETEL12             Room for this record in buffer               
                                                                                
         GOTOR NEXTPAGE            Get next buffer                              
                                                                                
         L     R3,ANXTBUFF         This gets reset to point to start            
         LR    R0,R3                                                            
         LH    R1,PAGESIZE                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear buffer area down                       
                                                                                
SETEL12  BCTR  R2,0                                                             
         EXRL  R2,SETMVC3E         Move in element information                  
         LA    R3,1(R2,R3)                                                      
         ST    R3,ANXTBUFF         Update pointer                               
         MVI   0(R3),EOB           Set buffer terminator                        
                                                                                
         GOTOR PROTON              Client runs with protection on               
                                                                                
SETELEMX SAM24 ,                                                                
         J     EXIT                                                             
                                                                                
SETMVCWW MVC   WORK(0),WORK+2                                                   
SETMVCEW MVC   ELEMENT+2(0),WORK                                                
SETMVC3E MVC   0(0,R3),ELEMENT                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Add data to output buffer                                           *         
*                                                                     *         
* Ntry: P1: A(FALINKBLK)                                              *         
*       P2: A(record data entry)                                      *         
*       P3: A(data)                                                   *         
*       P4: 0 for normal, non-zero holds field length override        *         
***********************************************************************         
                                                                                
ADDDATA  NTR1  LABEL=*,BASE=*                                                   
         LARL  RA,GLOBALS          Point to globals                             
         ICM   R9,15,0(R1)         A(FALINKBLK is 1st parameter)                
         L     R9,FALASVE-FALINKD(R9)                                           
         L     RC,AWORKD           Point to local storage                       
         L     RF,0(R1)                                                         
         MVC   FAPARMS(FALINKDL),0(RF)                                          
                                                                                
         SAM31 ,                                                                
                                                                                
         XC    WORK,WORK           Reset temporary areas                        
         XC    WORKLEN,WORKLEN                                                  
         XC    WORKDTYP,WORKDTYP                                                
         XC    TEMP,TEMP                                                        
         MVI   TEMPLEN,0                                                        
                                                                                
         CLC   4(4,R1),8(R1)       Special call?                                
         JNE   ADDD10              No                                           
                                                                                
         TM    SFLAG1,SFSDPEND     Data write pending?                          
         JZ    ADDD02              No                                           
         TM    SFLAG1,SFBOS                                                     
         JZ    *+8                                                              
         OI    SFLAG1,SFECHAR      Add end character                            
         GOTOR DWRITE              Write pending data                           
                                                                                
         NI    SFLAG1,FF-(SFSDPEND+SFBOS+SFSCHAR+SFECHAR)                       
                                                                                
ADDD02   CLC   =AL4(FALADNE),4(R1) Finished?                                    
         JE    ADDD08              Yes                                          
                                                                                
         CLC   =AL4(FALAUSN),4(R1) Upgrade soon message?                        
         JNE   *+12                                                             
         OI    SFLAG3,SF2USN                                                    
         J     ADDD08                                                           
                                                                                
         CLC   =AL4(FALAYRF),4(R1) Upgrade now message?                         
         JNE   *+12                                                             
         OI    SFLAG3,SF2YRF                                                    
         J     ADDD08                                                           
                                                                                
         CLC   =AL4(FALABAS),4(R1) Request send & break?                        
         JNE   *+12                                                             
         OI    SFLAG2,SFULBQ                                                    
         J     ADDD08                                                           
                                                                                
         CLC   =AL4(FALABRK),4(R1) Request break?                               
         JNE   ADDD04              Yes                                          
         TM    SFLAG2,SFULPQ                                                    
         JO    ADDD08                                                           
                                                                                
         MVC   CLIHOOK,FALASTP     Call client's save routine                   
         GOTOR CLIENT,0                                                         
         OI    SFLAG2,SFSSTPRQ     Flag break request                           
         J     ADDD08                                                           
                                                                                
ADDD04   CLC   =AL4(FALAGLB),4(R1)                                              
         JNE   ADDD06                                                           
         MVC   CLIHOOK,FALASTP     Call client's save routine                   
         GOTOR CLIENT,0                                                         
         OI    SFLAG2,SFSGLBRQ     Flag GLOBBER break request                   
         J     ADDD08                                                           
                                                                                
ADDD06   DC    H'0'                                                             
                                                                                
ADDD08   XC    SAVE,SAVE                                                        
         MVI   SAVELEN,0                                                        
         J     ADDD22                                                           
                                                                                
ADDD10   ICM   R3,15,4(R1)         RE=A(MDELD for this data)                    
         USING MDELD,R3                                                         
         SR    RE,RE                                                            
         ICM   RE,1,MDTYPE                                                      
         JNZ   *+6                                                              
         DC    H'0'                Type may not be zero                         
         CHI   RE,CNVRTABN                                                      
         JNH   *+6                                                              
         DC    H'0'                Type is out of range                         
         MHI   RE,CNVRTABL                                                      
         LA    RE,CNVRTAB-CNVRTABL(RE)                                          
         LLH   RF,CNVRUPL-CNVRTABD(RE)                                          
         LARL  RE,FALINK                                                        
         AR    RF,RE                                                            
         BASSM RE,RF               Do data conversion                           
         SAM31 ,                                                                
                                                                                
         OC    WORKLEN,WORKLEN     Any current item?                            
         JNZ   *+6                 Yes                                          
         DC    H'0'                                                             
                                                                                
         CLI   SAVELEN,0           Previous item saved?                         
         JE    ADDD18              No - save this one and quit                  
                                                                                
         CLC   MDCODE,SAVEMAP      Current data same code as before?            
         JNE   ADDD16              No                                           
                                                                                
         TM    SFLAG1,SFBOS        Already in series?                           
         JO    *+8                 Yes                                          
         OI    SFLAG1,SFSCHAR      Add start character to save item             
         GOTOR DWRITE              Write pending data                           
                                                                                
         NI    SFLAG1,FF-(SFSCHAR+SFECHAR+SFSDPEND)                             
         OI    SFLAG1,SFBOS                                                     
         J     ADDD18              Move current item to save                    
                                                                                
ADDD16   TM    SFLAG1,SFBOS        Within series                                
         JZ    *+8                                                              
         OI    SFLAG1,SFECHAR      Add end character                            
         GOTOR DWRITE              Write pending data                           
         NI    SFLAG1,FF-(SFBOS+SFSCHAR+SFECHAR+SFSDPEND)                       
                                                                                
ADDD18   MVC   SAVEMAP,MDCODE      Save mapcode                                 
         MVC   SAVEMTXT,MDTEXT     Save map text                                
         MVC   SAVEDTYP,WORKDTYP                                                
         XC    SAVE,SAVE           Move current item to save                    
         MVI   SAVELEN,0                                                        
         LLC   RF,WORKLEN                                                       
         STC   RF,SAVELEN          Save data length                             
         CHI   RF,DONLY            Special - element only                       
         JE    ADDD20                                                           
         BCTR  RF,0                                                             
         EXRL  RF,ADDMVCSW                                                      
                                                                                
ADDD20   OI    SFLAG1,SFSDPEND     Set write pending                            
         J     ADDD22                                                           
                                                                                
ADDD22   TM    SFLAG2,SFULBQ       TEMPEST full break?                          
         JZ    ADDDATAX            No                                           
         TM    SFLAG1,SFSDPEND     Data write pending?                          
         JZ    ADDD24              No                                           
         TM    SFLAG1,SFBOS                                                     
         JZ    *+8                                                              
         OI    SFLAG1,SFECHAR      Add end character                            
         GOTOR DWRITE              Write pending data                           
                                                                                
         NI    SFLAG1,FF-(SFSDPEND+SFBOS+SFSCHAR+SFECHAR)                       
                                                                                
ADDD24   ICM   RF,15,FALAFUL       Need a break/resume routine                  
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   CLIHOOK,FALAFUL     Call client's save routine                   
         L     R1,CALLR1                                                        
         L     R1,0(R1)                                                         
         MVI   MAHMOUNS-FALINKD(R1),0                                           
         GOTOR CLIENT,0                                                         
         IPM   R0                                                               
         NI    SFLAG2,FF-SFULBQ                                                 
         OI    SFLAG2,SFULPQ       Flag TEMPEST break request                   
         SPM   R0                                                               
                                                                                
ADDDATAX SAM24 ,                   Return to caller in 24-bit mode              
         J     EXITY                                                            
                                                                                
ADDMVCSW MVC   SAVE(0),WORK        Save data                                    
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Get item of data from buffer                                        *         
*                                                                     *         
* Ntry: P1: A(FALINKBLK)                                              *         
*       P2: A(record data entry)                                      *         
*       P3: A(data)                                                   *         
*       P4: 0 for normal, non-zero holds field length override        *         
***********************************************************************         
                                                                                
GETDATA  NTR1  LABEL=*,BASE=*                                                   
         LARL  RA,GLOBALS          Point to globals                             
         ICM   R9,15,0(R1)         A(FALINKBLK is 1st parameter)                
         L     R9,FALASVE-FALINKD(R9)                                           
         L     RC,AWORKD           Point to local storage                       
         ST    R1,SAVER1           Save incoming parameter list address         
                                                                                
         L     R3,4(R1)            Save A(incoming parameter block)             
         XC    0(24,R3),0(R3)      Clear incoming parameter block               
                                                                                
         XC    WORK,WORK           Clear storage blocks                         
         XC    WORKLEN,WORKLEN                                                  
         XC    TEMP,TEMP                                                        
         MVI   TEMPLEN,0                                                        
                                                                                
         SAM31 ,                                                                
                                                                                
         ICM   R6,15,ANXTBUFF      Already have a buffer?                       
         JNZ   GETD02              Yes                                          
         ICM   R6,15,FLNK          Point to buffer                              
         CLI   PAGEINDX,1          Test first page in buffer                    
         JE    GETD02              Yes                                          
         MVI   PAGEINDX,0          Set to read first page                       
         GOTOR GETPAGE             Read buffer                                  
                                                                                
GETD02   GOTOR TESTCHAR            Test current address                         
         CLI   0(R6),C' '          End of buffer?                               
         JNH   GETDH               No                                           
                                                                                
         MVC   BYTE,0(R6)          Save current value                           
         GOTOR NEXTCHAR            Move forward to next                         
         CLI   0(R6),ELEMIDQ       Check for record code                        
         JNE   GETD20                                                           
         TR    BYTE,INVLENS        Extended length (>255)                       
         CLI   BYTE,FF                                                          
         JE    GETDERR2            Invalid length                               
         MVC   TEMPLEN,BYTE        Get local copy of record                     
                                                                                
         LLC   R0,BYTE                                                          
         AHI   R6,1                                                             
         LA    R1,TEMP                                                          
GETD04   GOTOR TESTCHAR                                                         
         MVC   0(1,R1),0(R6)                                                    
         AHI   R1,1                                                             
         AHI   R6,1                                                             
         JCT   R0,GETD04                                                        
         GOTOR TESTCHAR                                                         
                                                                                
         ST    R6,ANXTBUFF         Save A(next entry in buffer)                 
         TM    SFLAG3,SF21E        Test first record of upload                  
         JNZ   GETD06              No                                           
         OI    SFLAG3,SF21E                                                     
         CLI   FALAWHEN,FALAW1EU   First record of upload?                      
         JNE   GETD06              No                                           
         MVC   CLIHOOK,FALAHOOK                                                 
         GOTOR CLIENT,DMCB,(TEMPLEN,TEMP)                                       
         JL    GETDERR5            Something is wrong!                          
                                                                                
GETD06   CLI   FALAWHEN,FALAWAEU   All records of upload                        
         JNE   GETD08              No                                           
         MVC   CLIHOOK,FALAHOOK                                                 
         GOTOR CLIENT,DMCB,(TEMPLEN,TEMP)                                       
         JL    GETDERR5            Something is wrong!                          
                                                                                
GETD08   ICM   RF,15,FALAHOOK      Call client to translate map code            
         JZ    GETD10                                                           
         GOTOR CLIENT,DMCB                                                      
         JL    GETDERR5            Something is wrong!                          
                                                                                
GETD10   ICM   RF,15,FALATRN       Call client to translate map code            
         JZ    GETD12                                                           
                                                                                
         MVC   CLIHOOK,FALATRN     To build buffer                              
         XC    DMCB(DMCBL),DMCB                                                 
         GOTOR CLIENT,DMCB,(TEMPLEN,TEMP)                                       
         JL    GETDERR5            Something is wrong!                          
                                                                                
         ICM   R5,15,4(R1)                                                      
         JZ    GETD12              You do the work                              
         USING MHELD,R5                                                         
                                                                                
         LA    RF,MHCODE           Build return parameter list (P1=0)           
         ST    RF,4(R3)            P2=A(translated map code)                    
         LA    RF,L'MHCODE                                                      
         ST    RF,8(R3)            P3=translated length                         
         LA    RF,TEMP                                                          
         ST    RF,12(R3)           P4=A(untranslated map code)                  
         LLC   RF,BYTE                                                          
         ST    RF,16(R3)           P5=untranslated length                       
         J     GETD18              Client has translated for you                
                                                                                
GETD12   XC    HALF,HALF           Map code is 2 or 4 characters                
         LA    RF,HALF             and must be valid hexadecimal                
         CLI   BYTE,4                                                           
         JE    GETD14                                                           
         LA    RF,HALF+1                                                        
         CLI   BYTE,2                                                           
         JNE   GETDERR2            Invalid element length                       
                                                                                
GETD14   LLC   R0,TEMPLEN          Get length for passing in plist              
         GOTOR HEXIN,DMCB,TEMP,(RF),(R0),0                                      
         ORG   *-2                                                              
         BASSM RE,RF                                                            
         SAM31 ,                                                                
                                                                                
         SRL   R0,1                Set data length                              
         STC   R0,WORKLEN                                                       
                                                                                
         LA    RF,WORK             Build return parameter list (P1=0)           
         ST    RF,4(R3)            P2=A(translated map code)                    
         ST    R0,8(R3)            P3=translated length                         
         LA    RF,TEMP                                                          
         ST    RF,12(R3)           P4=A(untranslated map code)                  
         LLC   R0,TEMPLEN                                                       
         ST    R0,16(R3)           P5=untranslated length                       
                                                                                
         L     R5,FALAMAP          Find map table for this map code             
         USING MHELD,R5                                                         
         SR    RF,RF                                                            
                                                                                
GETD16   CLI   MHLEN,MHLENX        Test end of map table                        
         JE    GETDERR1            Invalid map code code                        
         CLC   MHCODE,HALF         Match on map code code                       
         JE    GETD18                                                           
         ICM   RF,3,MHDISP                                                      
         LA    R5,0(RF,R5)                                                      
         J     GETD16                                                           
                                                                                
GETD18   ST    R5,AMAPNTRY         Save A(this map code entry)                  
         ST    R5,0(R3)            Return it in caller's plist                  
         J     GETDE                                                            
                                                                                
GETD20   SR    R4,R4                                                            
         CLI   BYTE,LCHAR                                                       
         JNE   GETD24                                                           
GETD22   AHI   R4,LCHARL                                                        
         GOTOR TESTCHAR                                                         
         MVC   BYTE,0(R6)                                                       
         GOTOR NEXTCHAR                                                         
         CLI   BYTE,LCHAR                                                       
         JE    GETD22                                                           
                                                                                
GETD24   TR    BYTE,INVLENS                                                     
         CLI   BYTE,FF                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         LLC   RF,BYTE                                                          
         AR    R4,RF               R4=length of field                           
         STC   R4,TEMPLEN          Save length                                  
         STC   R4,BYTE                                                          
                                                                                
         SR    R0,R0                                                            
GETD26   GOTOR TESTCHAR                                                         
         CLI   0(R6),XCHAR         Map code greater than 189?                   
         JNE   GETD28                                                           
         LHI   R0,XBASIS                                                        
         GOTOR NEXTCHAR                                                         
         MVC   BYTE,0(R6)                                                       
         TR    BYTE,INVLENS                                                     
         CLI   BYTE,FF                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         LLC   RF,BYTE                                                          
         MHI   RF,LCHARL                                                        
         AR    R0,RF                                                            
         GOTOR NEXTCHAR                                                         
         J     GETD30                                                           
                                                                                
GETD28   CLI   0(R6),LCHAR         Map code greater than 63?                    
         JNE   GETD30                                                           
         AHI   R0,LCHARL                                                        
         GOTOR NEXTCHAR                                                         
         J     GETD28                                                           
                                                                                
GETD30   MVC   BYTE1,0(R6)                                                      
         TR    BYTE1,INVLENS                                                    
         CLI   BYTE1,FF                                                         
         JNE   *+6                                                              
         DC    H'0'                                                             
         LLC   RF,BYTE1                                                         
         AR    R0,RF                                                            
         STH   R0,HALF             Half contains map code                       
                                                                                
         GOTOR NEXTCHAR            Point to executable data                     
                                                                                
         L     R5,AMAPNTRY                                                      
         LLC   RF,MHLEN-MHELD(R5)                                               
         AR    R5,RF                                                            
         USING MDELD,R5                                                         
                                                                                
GETD32   CLI   MDLEN,MDLENX        End of table                                 
         JE    GETDERR3            Unable to find data                          
                                                                                
         CLC   MDCODE,HALF         Match on mapcode                             
         JE    GETD34                                                           
         IC    RF,MDLEN                                                         
         AR    R5,RF                                                            
         J     GETD32                                                           
                                                                                
GETD34   SR    RE,RE               RE=A(conversion table)                       
         ICM   RE,1,MDTYPE                                                      
         JZ    GETDERR4                                                         
         CHI   RE,CNVRTABN                                                      
         JH    GETDERR4                                                         
         MHI   RE,CNVRTABL                                                      
         LA    RE,CNVRTAB-CNVRTABL(RE)                                          
         LLH   R0,CNVRDWN-CNVRTABD(RE)                                          
         LARL  RE,FALINK                                                        
         AR    R0,RE                                                            
         ST    R0,ACONVERT         Set A(conversion routine)                    
                                                                                
         LA    R1,TEMP             Extract input data into TEMP                 
         SR    R0,R0                                                            
         ICM   R0,1,TEMPLEN                                                     
         JZ    GETD40                                                           
GETD38   GOTOR TESTCHAR                                                         
         MVC   0(1,R1),0(R6)                                                    
         AHI   R1,1                                                             
         AHI   R6,1                                                             
         JCT   R0,GETD38                                                        
                                                                                
GETD40   STCM  R6,15,ANXTBUFF      Save pointer                                 
                                                                                
         TM    SFLAG3,SF21D                                                     
         JNZ   GETD42                                                           
         OI    SFLAG3,SF21D                                                     
         CLI   FALAWHEN,FALAW1DU   First data of upload?                        
         JNE   GETD42              No                                           
         MVC   CLIHOOK,FALAHOOK                                                 
         GOTOR CLIENT,DMCB,(TEMPLEN,TEMP)                                       
         JL    GETDERR5            Something is wrong!                          
                                                                                
GETD42   CLI   FALAWHEN,FALAWADU   All data of upload                           
         JNE   GETD44              No                                           
         MVC   CLIHOOK,FALAHOOK                                                 
         GOTOR CLIENT,DMCB,(TEMPLEN,TEMP)                                       
         JL    GETDERR5            Something is wrong!                          
                                                                                
GETD44   LLC   R0,TEMPLEN                                                       
         GOTOR ACONVERT,DMCB,(R5),TEMP,(R0)                                     
         ORG   *-2                                                              
         BASSM RE,RF               Call data conversion routine                 
         LLC   RF,WORKLEN          Build return values for caller               
         GOTOR ,(R3),(1,(R5)),WORK,(RF),TEMP,(R0)                               
         J     GETDE               Exit back to caller                          
                                                                                
GETDERR1 LA    R0,FAEQIRMP         Error - invalid map code code                
         J     GETDERR                                                          
                                                                                
GETDERR2 LA    R0,FAEQIRML         Error - invalid map code length              
         J     GETDERR                                                          
                                                                                
GETDERR3 LHI   R0,FAEQIDMP         Error - no code found                        
         L     R1,SAVER1           Point to caller's parameter list             
         L     RE,AMAPNTRY                                                      
         MVC   08(L'MDCODE,R1),MDCODE-MDELD(RE)                                 
         MVC   10(L'HALF,R1),HALF                                               
         J     GETDERR                                                          
                                                                                
GETDERR4 LA    R0,FAEQIDTY         Error - unknown data type                    
         J     GETDERR                                                          
                                                                                
GETDERR5 LA    R0,FAEQCLIE         Error - no translate                         
                                                                                
GETDERR  SAM24 ,                                                                
         L     R1,SAVER1           Point to user parameter list                 
         STC   R0,0(R1)            Return error to caller                       
         L     RF,FALAMSG          Error output (R0=error code)                 
         USING FAMSGD,RF                                                        
         XC    0(FAMSGDL,RF),0(RF)                                              
         MVC   FAMSGNO,=AL2(GE$FLERR)                                           
         MVI   FAMSGSYS,FF                                                      
         MVI   FAMSGTYP,GTMERR                                                  
         XC    FAMSGXTR,FAMSGXTR   Edit error number                            
         CURED (R0),(4,FAMSGXTR),0,ZERO=NOBLANK,ALIGN=LEFT                      
         DROP  RF                                                               
         LA    RF,0                Set condition code low                       
         J     GETDX                                                            
                                                                                
GETDH    LA    RF,2                Set condition code high                      
         J     GETDX                                                            
                                                                                
GETDE    LA    RF,1                Set condition code equal                     
                                                                                
GETDX    SAM24 ,                                                                
         CHI   RF,1                                                             
         J     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
NEXTCHAR AHI   R6,1                Advance to next character in buffer          
                                                                                
***********************************************************************         
* Test current character in buffer for end of buffer (X'FF') - when   *         
* found read next input page - exits with R6 pointing to next data    *         
* byte to process                                                     *         
***********************************************************************         
                                                                                
TESTCHAR CLI   0(R6),FF            Test for end of buffer                       
         BNER  RE                                                               
         ST    RE,SAVERE                                                        
         GOTOR NXTIPAG             Yes - get next buffered page                 
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R6,FLNK             Point to start of buffer                     
         L     RE,SAVERE                                                        
         BR    RE                  Return                                       
         EJECT                                                                  
***********************************************************************         
* Write pending data to buffer                                        *         
* Ntry: SAVELEN  = length of data pending                             *         
***********************************************************************         
                                                                                
DWRITE   NTR1  LABEL=*                                                          
         CLI   SAVELEN,DONLY       MDCODE only?                                 
         JE    DWRITE02                                                         
         TM    FALAINDS,FALAINDC   Test output compression off                  
         JNZ   DWRITE02                                                         
         GOTOR REPCHR              Repeating character optimization             
                                                                                
DWRITE02 GOTOR PROTOFF             Client runs with protection on               
                                                                                
         SR    R2,R2                                                            
         CLI   SAVELEN,DONLY       Mapcode only - set length zero               
         JE    *+8                                                              
         IC    R2,SAVELEN                                                       
                                                                                
         TM    SFLAG2,SFLSTQ       Last page?                                   
         JZ    DWRITE04            No                                           
         L     R6,ANXTBUFF                                                      
         LA    RE,256(R6)          Buffer spare we will allow                   
         C     RE,FLNKX                                                         
         JL    DWRITE06                                                         
         OI    SFLAG2,SFULBQ       Flag break required                          
         J     DWRITE06                                                         
                                                                                
DWRITE04 L     R6,ANXTBUFF         Get next available slot                      
         L     RE,FLNKX            End of buffer                                
         AHI   RE,-128             Leave spare for map code etc.                
         SR    RE,R6               Remaining space                              
         CR    RE,R2               Will this data fit?                          
         JH    DWRITE06            Yes                                          
                                                                                
         GOTOR NEXTPAGE            Get next buffer                              
                                                                                
         L     R6,ANXTBUFF         This gets reset to point to start            
         LR    R0,R6                                                            
         LH    R1,PAGESIZE                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear buffer area                            
                                                                                
DWRITE06 TM    SFLAG1,SFSCHAR      Start of series required?                    
         JZ    *+12                No                                           
         MVI   0(R6),SCHARQ                                                     
         AHI   R6,1                                                             
         CLI   SAVEDTYP,0          Test data type is set                        
         JNE   DWRITE08                                                         
         TM    FALAINDS,FALAINDT   Test don't send data type                    
         JNZ   DWRITE08                                                         
         LTR   R2,R2                                                            
         JZ    DWRITE08                                                         
         BCTR  R2,0                Length is off by one                         
                                                                                
DWRITE08 CHI   R2,LCHARL           String is longer than 63 characters?         
         JL    DWRITE10            No                                           
         MVI   0(R6),LCHAR                                                      
         AHI   R6,1                                                             
         AHI   R2,-(LCHARL)                                                     
         J     DWRITE08                                                         
                                                                                
DWRITE10 CLI   SAVELEN,DONLY                                                    
         JNE   DWRITE12                                                         
         MVI   0(R6),MONLY                                                      
         MVI   SAVELEN,0                                                        
         J     DWRITE14                                                         
                                                                                
DWRITE12 LA    R1,LENGTHS(R2)      Move in length code for data                 
         MVC   0(1,R6),0(R1)                                                    
                                                                                
DWRITE14 AHI   R6,1                                                             
         LLH   RF,SAVEMAP                                                       
                                                                                
         L     R1,FALABLD          Expanded download?                           
         CLI   FALCONO-FALSCRD(R1),EXPANDED                                     
         JNE   DWRITE16                                                         
         MVC   0(L'MDTEXT,R6),SAVEMTXT                                          
         MVI   L'MDTEXT(R6),C'='                                                
         AHI   R6,L'MDTEXT+1                                                    
         J     DWRITE26                                                         
                                                                                
DWRITE16 TM    SFLAG1,SFBOS        Currently in series?                         
         JZ    DWRITE18            No - output mapcode                          
         TM    FALAINDS,FALAINDD   Test type embedded in data                   
         JZ    DWRITE26                                                         
         SR    R1,R1               Yes - drop type for duplicates               
         ICM   R1,1,SAVELEN                                                     
         JZ    DWRITE26                                                         
         BCTR  R1,0                                                             
         STC   R1,SAVELEN                                                       
         MVC   SAVE(L'SAVE-1),SAVE+1                                            
         J     DWRITE26                                                         
                                                                                
DWRITE18 CLI   VRSN,V6Q            Test version 6 or greater                    
         JL    DWRITE20                                                         
         CHI   RF,XBASIS           Test map code greater than 189               
         JL    DWRITE20                                                         
         MVI   0(R6),XCHAR         Expanded map character                       
         AHI   R6,1                                                             
         AHI   RF,-(XBASIS)                                                     
         SR    RE,RE                                                            
         LHI   R0,LCHARL                                                        
         DR    RE,R0                                                            
         CHI   RF,LCHARL                                                        
         JNH   *+6                                                              
         DC    H'0'                                                             
         LA    RF,LENGTHS(RF)                                                   
         MVC   0(1,R6),0(RF)                                                    
         AHI   R6,1                                                             
         LA    RE,LENGTHS(RE)                                                   
         MVC   0(1,R6),0(RE)                                                    
         AHI   R6,1                                                             
         J     DWRITE24                                                         
                                                                                
DWRITE20 CHI   RF,LCHARL           Mapcode is greater than 63?                  
         JL    DWRITE22            No                                           
         MVI   0(R6),LCHAR                                                      
         AHI   R6,1                                                             
         AHI   RF,-(LCHARL)                                                     
         J     DWRITE20                                                         
                                                                                
DWRITE22 LA    R1,LENGTHS(RF)      Move in mapcode length                       
         MVC   0(1,R6),0(R1)                                                    
         AHI   R6,1                                                             
                                                                                
DWRITE24 CLI   SAVEDTYP,0          Test have data type saved                    
         JE    DWRITE26                                                         
         MVC   0(L'SAVEDTYP,R6),SAVEDTYP                                        
         AHI   R6,L'SAVEDTYP                                                    
                                                                                
DWRITE26 SR    R2,R2               Move in data                                 
         ICM   R2,1,SAVELEN                                                     
         JZ    DWRITE28                                                         
         BCTR  R2,0                                                             
         EXRL  R2,DWRMVC6S                                                      
         AHI   R2,1                                                             
                                                                                
DWRITE28 AR    R6,R2               Point to next output buffer area             
                                                                                
DWRITE30 TM    SFLAG1,SFECHAR      End of series required?                      
         JZ    *+16                No                                           
         MVI   0(R6),ECHARQ        Set end of servies                           
         AHI   R6,1                and point past it                            
         NI    SFLAG1,FF-(SFECHAR)                                              
                                                                                
         MVI   0(R6),EOB           Set end of this buffer                       
         ST    R6,ANXTBUFF                                                      
                                                                                
         OC    SAVEELEM,SAVEELEM   Test element pending                         
         JZ    DWRITEX                                                          
                                                                                
         LA    R2,SAVEELEM+L'SAVEELEM-1                                         
         CLI   0(R2),0                                                          
         JNE   *+8                                                              
         JCT   R2,*-8                                                           
         LA    RE,SAVEELEM                                                      
         SR    R2,RE                                                            
         L     R6,ANXTBUFF         Get next available slot                      
         L     RE,FLNKX            End of buffer                                
         SR    RE,R6               Remaining space                              
         BCTR  RE,0                                                             
         CR    R2,RE                                                            
         JNH   DWRITE32                                                         
         GOTOR NEXTPAGE            Get next buffer                              
                                                                                
DWRITE32 L     R6,ANXTBUFF                                                      
         EXRL  R2,DWRMVC6E                                                      
         LA    R6,1(R2,R6)                                                      
         ST    R6,ANXTBUFF                                                      
         XC    SAVEELEM,SAVEELEM                                                
                                                                                
DWRITEX  GOTOR PROTON              Client runs with protection on               
         J     EXITY                                                            
                                                                                
DWRMVC6S MVC   0(0,R6),SAVE                                                     
DWRMVC6E MVC   0(0,R6),SAVEELEM                                                 
         EJECT                                                                  
***********************************************************************         
* Repeating character optimization (called from DWRITE)               *         
***********************************************************************         
                                                                                
REPCHR   NTR1  LABEL=*                                                          
         LLC   R0,SAVELEN                                                       
         CHI   R0,3                Possibly anything to optimise?               
         JH    REPCHR08            Yes                                          
                                                                                
         XC    WORK2,WORK2         Look for repeat characters                   
         LA    R2,WORK2                                                         
         LA    R1,SAVE                                                          
                                                                                
REPCHR02 CLI   0(R1),REPEATQ       Test repeat character                        
         JNE   REPCHR04                                                         
         MVC   0(L'REALREP,R2),REALREP                                          
         AHI   R2,L'REALREP        Repeat characters must be duplicated         
         J     REPCHR06                                                         
                                                                                
REPCHR04 MVC   0(1,R2),0(R1)                                                    
         CLI   0(R2),C' '          Fix bad data                                 
         JNL   *+8                                                              
         MVI   0(R2),C' '                                                       
         AHI   R2,1                Point to next output character               
                                                                                
REPCHR06 AHI   R1,1                Point to next input character                
         JCT   R0,REPCHR02                                                      
                                                                                
         LA    R1,WORK2                                                         
         SR    R2,R1               R2=length of optimized data                  
         STC   R2,SAVELEN                                                       
         MVC   SAVE,WORK2                                                       
         J     REPCHRX                                                          
                                                                                
REPCHR08 LA    R1,SAVE                                                          
         LA    R2,WORK2                                                         
         XC    WORK2,WORK2                                                      
                                                                                
REPCHR10 LR    RE,R1               Save A(first character)                      
         CLI   0(R1),REPEATQ                                                    
         JNE   REPCHR12                                                         
         MVC   0(L'REALREP,R2),REALREP                                          
         AHI   R2,L'REALREP        Repeat characters must be duplicated         
         J     REPCHR14                                                         
                                                                                
REPCHR12 CLI   0(R1),C' '          Fix bad data                                 
         JNL   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLC   1(2,R1),0(R1)       Test if next 2 characters same               
         JE    REPCHR16                                                         
         MVC   0(1,R2),0(R1)       Copy in this character and advance           
         AHI   R2,1                                                             
                                                                                
REPCHR14 AHI   R1,1                                                             
         JCT   R0,REPCHR10                                                      
                                                                                
         LA    R1,WORK2                                                         
         SR    R2,R1               R2=length of optimized data                  
         STC   R2,SAVELEN                                                       
         MVC   SAVE,WORK2                                                       
         J     REPCHRX                                                          
                                                                                
REPCHR16 AHI   R1,2                Advance repeat pointer                       
         AHI   R0,-(2)             Reduce remaining length                      
         JNM   REPCHR18                                                         
         DC    H'0'                Oh dear                                      
                                                                                
REPCHR18 CLC   0(1,R1),1(R1)       Still repeating?                             
         JNE   *+12                No                                           
         AHI   R1,1                                                             
         JCT   R0,REPCHR18                                                      
                                                                                
         AHI   R1,1                                                             
         SR    R1,RE               R1=number of repetitions                     
         MVI   0(R2),REPEATQ       Insert repeating indicator                   
         AHI   R2,1                                                             
         LR    R3,R1               Save actual length                           
                                                                                
REPCHR20 CHI   R1,LCHARL           Multiple character length?                   
         JL    REPCHR22            No                                           
         MVI   0(R2),LCHAR                                                      
         AHI   R2,1                                                             
         AHI   R1,-(LCHARL)                                                     
         J     REPCHR20                                                         
                                                                                
REPCHR22 STC   R1,BYTE             Translate length                             
         TR    BYTE,LENGTHS                                                     
                                                                                
         MVC   0(1,R2),BYTE                                                     
         MVC   1(1,R2),0(RE)                                                    
         AHI   R2,2                                                             
         LR    R1,R3               Restore actual length                        
         AR    R1,RE               Reset R1 to last repetition                  
         BCTR  R1,0                                                             
         J     REPCHR14                                                         
                                                                                
REPCHRX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Get next available buffer page                                      *         
***********************************************************************         
                                                                                
NEXTPAGE NTR1  LABEL=*                                                          
         ICM   RF,15,ANXTBUFF      Get next available slot                      
         MVI   0(RF),EOB           Set end of buffer in page                    
         GOTOR PUTPAGE             Write buffer page to disk                    
         GOTOR NXTOPAG             Get next page                                
         JE    *+6                                                              
         DC    H'0'                No more pages available                      
         L     RF,ANXTBUFF                                                      
         S     RF,FLNK             RF=length of data in this buffer             
         A     RF,ABUFFLEN                                                      
         ST    RF,ABUFFLEN         Update total used                            
         MVC   ANXTBUFF,FLNK       Set current pointer to start                 
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Get next output buffer page                                         *         
***********************************************************************         
                                                                                
NXTOPAG  NI    SFLAG2,FF-SFLSTQ    Set not last page                            
         TM    SFLAG2,SFTMPEST     Test TEMPEST                                 
         JZ    NXTOPAG2            No                                           
                                                                                
         LLC   R0,PAGEINDX                                                      
         CLM   R0,1,PAGETHIS       Test last page already                       
         JNL   NXTOPAGL                                                         
         AHI   R0,1                Bump index number                            
         STC   R0,PAGEINDX         Set this page index                          
         STC   R0,PAGEHIGH         Set highest page index used                  
         CLM   R0,1,PAGETHIS       Test this is the last page                   
         JL    NXTOPAGE                                                         
         OI    SFLAG2,SFLSTQ       Set this is the last TEMPEST page            
         J     NXTOPAGE                                                         
                                                                                
NXTOPAG2 L     R1,FALAPGS          Point to page list                           
         LLC   R0,PAGEINDX         R0=page index (can be zero)                  
         AR    R1,R0               Point to first/next in list                  
         CLI   0(R1),0             Test this slot available                     
         JE    NXTOPAGL            No                                           
         AHI   R0,1                Bump and set displacement                    
         STC   R0,PAGEINDX         Set this page index                          
         STC   R0,PAGEHIGH         Set highest page index used                  
         CLI   1(R1),0             Is there another page after this?            
         JNE   NXTOPAGE            Yes                                          
         OI    SFLAG2,SFLSTQ       Set this is last TEMPSTR page                
         J     NXTOPAGE                                                         
                                                                                
NXTOPAGE LA    R0,1                Set CC to equal                              
         J     NXTOPAGX                                                         
NXTOPAGL LA    R0,0                Set CC to low                                
NXTOPAGX CHI   R0,1                Set condition code                           
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Get next input buffer page                                          *         
* Ntry: R3 points to last in this buffer                              *         
* Exit: CC set equal if page read                                     *         
***********************************************************************         
                                                                                
NXTIPAG  NTR1  LABEL=*                                                          
                                                                                
         LLC   R0,PAGEINDX         Get current page index                       
         AHI   R0,1                                                             
         CLM   R0,1,PAGEHIGH       Test greater than highest used               
         JH    EXITL                                                            
         STC   R0,PAGEINDX         Set page index value                         
         L     RE,FLNK                                                          
         SR    R3,RE               R3=displacement into this page               
         A     R3,APREVLEN                                                      
         ST    R3,APREVLEN                                                      
         GOTOR GETPAGE             Get page into buffer                         
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Read current page into buffer                                       *         
* Ntry: PAGEINDX = page index value                                   *         
***********************************************************************         
                                                                                
GETPAGE  NTR1  LABEL=*                                                          
         LA    R2,DMREAD           Point to data manager action                 
         J     GETPUTPG                                                         
                                                                                
***********************************************************************         
* Write buffer page to disk                                           *         
* Ntry: PAGEINDX = page to write                                      *         
***********************************************************************         
                                                                                
PUTPAGE  NTR1  LABEL=*                                                          
         LA    R2,DMWRT            Point to data manager action                 
                                                                                
GETPUTPG CLI   PAGEINDX,0          Test page index value set                    
         JNE   *+8                                                              
         MVI   PAGEINDX,1          No - set to read/write page 1                
         TM    SFLAG2,SFTMPEST     Test TEMPSTR                                 
         JZ    GETPUTP2            Yes                                          
                                                                                
         LLC   R0,PAGEINDX         R0=page index                                
         LLC   RF,PAGEPREV         RF=base page number (zero based)             
         AR    R0,RF               R0=actual page number                        
         SLL   R0,32-8             Shift to top of register                     
         ICM   RF,15,LEPGSIZE                                                   
         GOTOR DMGR,DMCB,(X'40',(R2)),TEMPEST,(R0),FLNK,0,(RF),XA=OFF           
         JE    GETPUTPX                                                         
         DC    H'0'                                                             
                                                                                
GETPUTP2 L     RE,FTWA                                                          
         L     R1,FALAPGS          Translate displacement to page               
         LLC   R0,PAGEINDX                                                      
         AR    R1,R0               Point to real page number                    
         BCTR  R1,0                Point to physical page number                
         SR    R0,R0                                                            
         ICM   R0,B'0011',TWATRM-TWAD(RE)                                       
         ICM   R0,B'1000',0(R1)    Get physical page number HOB of R0           
         ICM   RF,15,LEPGSIZE                                                   
         GOTOR DMGR,DMCB,(X'40',(R2)),TEMPSTR,(R0),FLNK,0,(RF),XA=OFF           
         JE    GETPUTPX                                                         
         DC    H'0'                                                             
                                                                                
GETPUTPX DS    0H                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Add to buffer routines                                              *         
* ------------------------------------------------------------------- *         
* Ntry: 04(R1)  = A(map data entry)                                   *         
*       08(R1)  = A(input data)                                       *         
*       12(R1)  = override data length (if required)                  *         
* Exit: WORK    = data                                                *         
*       WORKLEN = data length                                         *         
*                                                                     *         
* Get from buffer routines                                            *         
* ------------------------------------------------------------------- *         
* Ntry: 00(R1)  = A(map data entry)                                   *         
*       04(R1)  = A(input data)                                       *         
*       08(R1)  = data length input                                   *         
* Exit: WORK    = data                                                *         
*       WORKLEN = data length to pass back                            *         
***********************************************************************         
                                                                                
***********************************************************************         
* Add binary data to buffer                                           *         
***********************************************************************         
                                                                                
ADDBIN   NTR1  LABEL=*                                                          
         SR    R0,R0                                                            
         LM    R2,R4,4(R1)                                                      
         USING MDELD,R2                                                         
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
                                                                                
         MVI   WORKDTYP,C'F'       Default type is 32-bit integer               
         CHI   R4,1                                                             
         JNE   *+8                                                              
         MVI   WORKDTYP,C'T'       Length 1 is tiny integer                     
         CHI   R4,2                                                             
         JNE   *+8                                                              
         MVI   WORKDTYP,C'S'       Length 2 is short integer                    
                                                                                
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EXRL  RF,ADDBICM                                                       
         CURED (R0),(14,WORK),0,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
         STC   R0,WORKLEN                                                       
ADDBINX  J     EXIT                                                             
                                                                                
ADDBICM  ICM   R0,0,0(R3)                                                       
         EJECT                                                                  
***********************************************************************         
* Get binary data from buffer                                         *         
***********************************************************************         
                                                                                
GETBIN   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         BCTR  R4,0                                                             
         EXRL  R4,GETBPCK          Pack character number                        
         CVB   R0,DUB                                                           
         LLC   R4,MDDLEN                                                        
         STC   R4,WORKLEN          Set required length                          
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EXRL  RF,GETBSTCM         Store required number of characters          
GETBINX  J     EXIT                                                             
                                                                                
GETBPCK  PACK  DUB,0(0,R3)                                                      
GETBSTCM STCM  R0,0,WORK                                                        
         EJECT                                                                  
***********************************************************************         
* Add unsigned binary data to buffer                                  *         
***********************************************************************         
                                                                                
ADDUSB   NTR1  LABEL=*                                                          
         SR    R0,R0                                                            
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
                                                                                
         MVI   WORKDTYP,C'F'       Default type is 32-bit integer               
         CHI   R4,1                                                             
         JNE   *+8                                                              
         MVI   WORKDTYP,C'T'       Length 1 is tiny integer                     
         CHI   R4,2                                                             
         JNE   *+8                                                              
         MVI   WORKDTYP,C'S'       Length 2 is short integer                    
                                                                                
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EXRL  RF,ADDUICM                                                       
         TMH   R0,X'8000'          Test sign bit on                             
         JO    ADDU02              Yes                                          
         CURED (R0),(14,WORK),0,ALIGN=LEFT,ZERO=NOBLANK                         
         STC   R0,WORKLEN                                                       
         J     ADDUSBX                                                          
                                                                                
ADDU02   N     R0,HOBOFF           Turn sign bit off                            
         CVD   R0,DUB              Number now in DUB                            
         OI    DUB+L'DUB-1,X'0F'                                                
         AP    DUB,=PL8'2147483648' Add the missing HOB                         
         CURED (P8,DUB),(15,WORK),0,ALIGN=LEFT,ZERO=NOBLANK                     
         STC   R0,WORKLEN                                                       
ADDUSBX  J     EXIT                                                             
                                                                                
ADDUICM  ICM   R0,0,0(R3)                                                       
         EJECT                                                                  
***********************************************************************         
* Get unsigned binary data from buffer                                *         
***********************************************************************         
                                                                                
GETUSB   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         BCTR  R4,0                                                             
         EXRL  R4,GETUPACK         Pack character number                        
         CVB   R0,DUB                                                           
         LLC   R4,MDDLEN                                                        
         STC   R4,WORKLEN          Set required length                          
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EXRL  RF,GETUSTCM         Store required number of characters          
         J     EXIT                                                             
                                                                                
GETUPACK PACK  DUB,0(0,R3)                                                      
GETUSTCM STCM  R0,0,WORK                                                        
         EJECT                                                                  
***********************************************************************         
* Add packed data to buffer                                           *         
***********************************************************************         
                                                                                
ADDPCK   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'L'       Packed are all type long                     
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EXRL  R4,ADDPZAP                                                       
         CURED (P8,DUB),(14,WORK),0,ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK             
         STC   R0,WORKLEN                                                       
ADDPCKX  J     EXIT                                                             
                                                                                
ADDPZAP  ZAP   DUB,0(0,R3)                                                      
         EJECT                                                                  
***********************************************************************         
* Get packed data from buffer                                         *         
***********************************************************************         
                                                                                
GETPCK   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         GOTOR CASHVAL,DMCB,(C'0',(R3)),(R4)                                    
         CLI   0(R1),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                Invalid value                                
                                                                                
         LLC   RF,MDDLEN                                                        
         STC   RF,WORKLEN          Set required length                          
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EXRL  RF,GETPZAP          Zap in data                                  
GETPCKX  J     EXIT                                                             
                                                                                
GETPZAP  ZAP   WORK(0),DMCB+4(8)                                                
         EJECT                                                                  
***********************************************************************         
* Add character data to buffer                                        *         
***********************************************************************         
                                                                                
ADDCHR   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'C'       Type chararacter                             
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EXRL  R4,ADDCMVC                                                       
         AHI   R4,1                R4=length of data                            
                                                                                
         LA    RF,WORK(R4)                                                      
         BCTR  RF,0                RF=last character                            
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         JCT   R4,*-10             Find first non-space                         
                                                                                
         LTR   R4,R4                                                            
         JNZ   *+8                                                              
         LHI   R4,DONLY                                                         
         STC   R4,WORKLEN          Set length moved                             
ADDCHRX  J     EXIT                                                             
                                                                                
ADDCMVC  MVC   WORK(0),0(R3)                                                    
         EJECT                                                                  
***********************************************************************         
* User qualified data                                                 *         
***********************************************************************         
                                                                                
ADDUSR   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,0          No type for user fields                      
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EXRL  R4,ADDUMVC                                                       
         AHI   R4,1                R4=length of data                            
                                                                                
         LA    RF,WORK(R4)                                                      
         BCTR  RF,0                RF=last character                            
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         JCT   R4,*-10             Find first non-space                         
                                                                                
         LTR   R4,R4                                                            
         JNZ   *+8                                                              
         LHI   R4,DONLY                                                         
         STC   R4,WORKLEN          Set length moved                             
ADDUSRX  J     EXIT                                                             
                                                                                
ADDUMVC  MVC   WORK(0),0(R3)                                                    
                                                                                
***********************************************************************         
* Get printable (character) data from buffer                          *         
***********************************************************************         
                                                                                
GETCHR   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         BCTR  R4,0                                                             
         EXRL  R4,GETCMVC                                                       
         SR    RF,RF               Data length                                  
         ICM   RF,1,MDDLEN                                                      
         JNZ   *+8                 Zero means variable length                   
         LA    RF,1(R4)                                                         
         STC   RF,WORKLEN                                                       
GETCHRX  J     EXIT                                                             
                                                                                
GETCMVC  MVC   WORK(0),0(R3)       Move data into WORK                          
         EJECT                                                                  
***********************************************************************         
* Add date to buffer (output format YYYYMMDD)                         *         
***********************************************************************         
                                                                                
ADDDTE   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'D'       Date                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR DATCON,DMCB,(1,(R3)),(20,WORK)                                   
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,WORK                                                    
         JNE   ADDDTEX                                                          
         MVI   WORKLEN,4                                                        
         MVC   WORK(4),WORK+4                                                   
ADDDTEX  J     EXIT                                                             
                                                                                
***********************************************************************         
* Get date into binary                                                *         
***********************************************************************         
                                                                                
GETDTE   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         GOTOR DATCON,DMCB,(9,(R3)),(1,WORK)                                    
         MVI   WORKLEN,3                                                        
GETDTEX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Add pwos julian date to buffer (output format YYYYMMDD)             *         
***********************************************************************         
                                                                                
ADDJDT   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'D'       Date                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR DATCON,DMCB,(8,(R3)),(20,WORK)                                   
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,WORK                                                    
         JNE   ADDJDTX                                                          
         MVI   WORKLEN,4                                                        
         MVC   WORK(4),WORK+4                                                   
ADDJDTX  J     EXIT                                                             
                                                                                
***********************************************************************         
* Get pwos julian date from buffer                                    *         
***********************************************************************         
                                                                                
GETJDT   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         GOTOR DATCON,DMCB,(9,(R3)),(19,WORK)                                   
         MVI   WORKLEN,3                                                        
GETJDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Add binary date to buffer (output format YYYYMMDD)                  *         
***********************************************************************         
                                                                                
ADDBDT   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'D'       Date                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR DATCON,DMCB,(3,(R3)),(20,WORK)                                   
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,WORK                                                    
         JNE   ADDBDTX                                                          
         MVI   WORKLEN,4                                                        
         MVC   WORK(4),WORK+4                                                   
ADDBDTX  J     EXIT                                                             
                                                                                
***********************************************************************         
* Get binary date from buffer                                         *         
***********************************************************************         
                                                                                
GETBDT   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         GOTOR DATCON,DMCB,(9,(R3)),(3,WORK)                                    
         MVI   WORKLEN,3                                                        
GETBDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Add compressed date to buffer (output format YYYYMMDD)              *         
***********************************************************************         
                                                                                
ADDCDT   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'D'       Date                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR DATCON,DMCB,(2,(R3)),(20,WORK)                                   
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,WORK                                                    
         JNE   ADDCDTX                                                          
         MVI   WORKLEN,4                                                        
         MVC   WORK(4),WORK+4                                                   
ADDCDTX  J     EXIT                                                             
                                                                                
***********************************************************************         
* Get compressed date from buffer                                     *         
***********************************************************************         
                                                                                
GETCDT   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         GOTOR DATCON,DMCB,(9,(R3)),(2,WORK)                                    
         MVI   WORKLEN,2                                                        
GETCDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Add cash amount to buffer                                           *         
***********************************************************************         
                                                                                
ADDCSH   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'L'       Packed are all type long                     
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EXRL  R4,ADDCZAP                                                       
         CURED (P8,DUB),(14,WORK),0,ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK             
         STC   R0,WORKLEN                                                       
ADDCSHX  J     EXIT                                                             
                                                                                
ADDCZAP  ZAP   DUB,0(0,R3)                                                      
         EJECT                                                                  
***********************************************************************         
* Get packed cash value from buffer                                   *         
***********************************************************************         
                                                                                
GETCSH   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         GOTOR CASHVAL,DMCB,(C'0',(R3)),(R4)                                    
         CLI   0(R1),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                Invalid value                                
                                                                                
         LLC   RF,MDDLEN                                                        
         STC   RF,WORKLEN          Set required length                          
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EXRL  RF,GETCZAP          Zap in data                                  
GETCSHX  J     EXIT                                                             
                                                                                
GETCZAP  ZAP   WORK(0),DMCB+4(8)                                                
         EJECT                                                                  
***********************************************************************         
* Add binary cash data to buffer                                      *         
***********************************************************************         
                                                                                
ADDBCA   NTR1  LABEL=*                                                          
         SR    R0,R0                                                            
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
                                                                                
         MVI   WORKDTYP,C'F'       Default type is 32-bit integer               
         CHI   R4,1                                                             
         JNE   *+8                                                              
         MVI   WORKDTYP,C'T'       Length 1 is tiny integer                     
         CHI   R4,2                                                             
         JNE   *+8                                                              
         MVI   WORKDTYP,C'S'       Length 2 is short integer                    
                                                                                
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         TM    0(R3),X'80'         Test input is negative                       
         JZ    *+8                                                              
         ICM   R0,15,EFFS          Yes - preset R0 to FFs                       
         EXRL  RF,ADDBICM                                                       
         CURED (R0),(14,WORK),0,ALIGN=LEFT,FLOAT=-                              
         STC   R0,WORKLEN                                                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Get binary cash data from buffer                                    *         
***********************************************************************         
                                                                                
GETBCA   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
         GOTOR CASHVAL,DMCB,(C'N',(R3)),(R4)                                    
         CLI   0(R1),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                Invalid value                                
                                                                                
         L     R0,DMCB+4           Binary amount returned here                  
         LLC   R4,MDDLEN                                                        
         STC   R4,WORKLEN          Set required length                          
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EXRL  RF,GETBSTCM         Store required number of digits              
GETBCAX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Add hexadecimal data to buffer                                      *         
***********************************************************************         
                                                                                
ADDHEX   NTR1  LABEL=*                                                          
         MVI   WORKDTYP,C'X'       Type hex                                     
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               Override data length                         
         JNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         GOTOR HEXOUT,DMCB,(R3),WORK,(R4),0                                     
         SLL   R4,1                                                             
                                                                                
ADDHEX02 CHI   R4,1                Must show at least one byte                  
         JNH   ADDHEXX                                                          
         CLI   WORK,C'0'           Strip leding zero(s)                         
         JNE   ADDHEXX                                                          
         BCTR  R4,0                Adjust for EX below                          
         EXRL  R4,ADDHMVC                                                       
         J     ADDHEX02                                                         
                                                                                
ADDHEXX  STC   R4,WORKLEN                                                       
         J     EXIT                                                             
                                                                                
ADDHMVC  MVC   WORK(0),WORK+1                                                   
         EJECT                                                                  
***********************************************************************         
* Get hexadecimal data from buffer                                    *         
***********************************************************************         
                                                                                
GETHEX   NTR1  LABEL=*                                                          
         LM    R2,R4,0(R1)                                                      
                                                                                
         MVI   WORK2,C'0'          Zero fill WORK2                              
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
                                                                                
         LTR   R4,R4               Override data length set?                    
         JNZ   *+12                Yes                                          
         ICM   R4,1,MDDLEN                                                      
         SLL   R4,1                                                             
                                                                                
         LLC   RF,MDDLEN                                                        
         SLL   RF,1                                                             
         SR    RF,R4               RF=amount of alignment required              
         LA    RF,WORK2(RF)                                                     
                                                                                
         BCTR  R4,0                                                             
         EXRL  R4,GETHMVC                                                       
         AHI   R4,1                                                             
                                                                                
         LLC   RF,MDDLEN                                                        
         SLL   RF,1                                                             
         GOTOR HEXIN,DMCB,WORK2,WORK,(RF),0                                     
         L     R4,DMCB+12                                                       
         STC   R4,WORKLEN                                                       
         J     EXIT                                                             
                                                                                
GETHMVC  MVC   0(0,RF),0(R3)                                                    
         EJECT                                                                  
***********************************************************************         
* Add data code only (zero length) to buffer                          *         
***********************************************************************         
                                                                                
ADDMDE   MVI   WORKDTYP,C'H'                                                    
         MVI   WORKLEN,DONLY                                                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Get data code only from buffer                                      *         
***********************************************************************         
                                                                                
GETMDE   BR    RE                                                               
         EJECT                                                                  
EXITL    LA    RF,0                                                             
         J     EXITCC                                                           
EXITY    LA    RF,1                                                             
EXITCC   CHI   RF,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
GLOBALS  DS    0D                  ** Globals/literal values **                 
                                                                                
FALXMODA DC    H'0',C'$ABEND'                                                   
                                                                                
         LTORG ,                                                                
                                                                                
HEADELS  DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(02),AL1(LINLEN),X'C2'             
         DC    AL1(CONFLDQ),X'00'                                               
HEADELNQ EQU   *-HEADELS                                                        
                                                                                
DATAELS  DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(02),AL1(LINLEN),X'C0'             
         DC    AL1(0),X'00'                                                     
DATAELNQ EQU   *-DATAELS                                                        
                                                                                
         DS    0F                                                               
HOBOFF   DC    X'7FFFFFFF'                                                      
                                                                                
K        EQU   1024                                                             
LEPGSIZE DC    C'L='               This and PAGESIZE are a couple               
PAGESIZE DC    AL2(18*K)           Size of TEMPSTR / TEMPEST                    
                                                                                
EFFS     DC    X'FFFFFFFF'                                                      
ERRF     DC    C'ERRF'                                                          
ERRC     DC    C'ERRC'                                                          
DMRSRV   DC    CL8'DMRSRV'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
TEMPEST  DC    CL8'TEMPEST'                                                     
SPACES   DC    CL80' '                                                          
IDENTIFY DC    C'<FALINK>'         Identifier in TWAMSG to show active          
MOREMSG  DC    C'ENCORE'           More processing to do (buffer full)          
REALREP  DC    C'??'               '?' is repeat char '?' becomes '??'          
FALWEYE  DC    C'*FALKWK*'         Working storage eyecatcher                   
FALSEYE  DC    C'*FALKSV*'         Saved storage eyecatcher                     
                                                                                
LENGTHS  DC    C'%ABCDEFGHIJKLMNO'                 00-0F                        
         DC    C'PQRSTUVWXYZ01234'                 10-1F                        
         DC    C'56789abcdefghijk'                 20-2F                        
         DC    C'lmnopqrstuvwxyz+'                 30-3F                        
         DC    C'                '                 40-4F                        
         DC    C'                '                 50-5F                        
         DC    C'                '                 60-6F                        
         DC    C'                '                 70-7F                        
         DC    C'                '                 80-8F                        
         DC    C'                '                 90-9F                        
         DC    C'                '                 A0-AF                        
         DC    C'                '                 B0-BF                        
         DC    C'                '                 C0-CF                        
         DC    C'                '                 D0-DF                        
         DC    C'                '                 E0-EF                        
         DC    C'                '                 F0-FF                        
                                                                                
INVLENS  DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFF3FFF' 40-4F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 50-5F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFF00FFFFFF' 60-6F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 70-7F                        
         DC    X'FF25262728292A2B2C2DFFFFFFFFFFFF' 80-8F                        
         DC    X'FF2E2F30313233343536FFFFFFFFFFFF' 90-9F                        
         DC    X'FFFF3738393A3B3C3D3EFFFFFFFFFFFF' A0-AF                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0-BF                        
         DC    X'FF010203040506070809FFFFFFFFFFFF' C0-CF                        
         DC    X'FF0A0B0C0D0E0F101112FFFFFFFFFFFF' D0-DF                        
         DC    X'FFFF131415161718191AFFFFFFFFFFFF' E0-EF                        
         DC    X'1B1C1D1E1F2021222324FFFFFFFFFFFF' F0-FF                        
                                                                                
CNVRTAB  DS    0XL(CNVRTABL)       ** Data type conversion table **             
         DC    AL2(ADDBIN-FALINK,GETBIN-FALINK)                                 
         DC    AL2(ADDPCK-FALINK,GETPCK-FALINK)                                 
         DC    AL2(ADDCHR-FALINK,GETCHR-FALINK)                                 
         DC    AL2(ADDDTE-FALINK,GETDTE-FALINK)                                 
         DC    AL2(ADDCDT-FALINK,GETCDT-FALINK)                                 
         DC    AL2(ADDCSH-FALINK,GETCSH-FALINK)                                 
         DC    AL2(ADDBCA-FALINK,GETBCA-FALINK)                                 
         DC    AL2(ADDHEX-FALINK,GETHEX-FALINK)                                 
         DC    AL2(ADDMDE-FALINK,GETMDE-FALINK)                                 
         DC    AL2(ADDJDT-FALINK,GETJDT-FALINK)                                 
         DC    AL2(ADDBDT-FALINK,GETBDT-FALINK)                                 
         DC    AL2(ADDUSB-FALINK,GETUSB-FALINK)                                 
         DC    AL2(ADDUSR-FALINK,GETCHR-FALINK)                                 
CNVRTABN EQU   (*-CNVRTAB)/CNVRTABL                                             
                                                                                
CNVRTABD DSECT ,                   ** Data type conversion table **             
CNVRUPL  DS    AL2                 Upload conversion routine                    
CNVRDWN  DS    AL2                 Download conversion routine                  
CNVRTABL EQU   *-CNVRTABD                                                       
         EJECT                                                                  
***********************************************************************         
* Global equates                                                      *         
***********************************************************************         
                                                                                
GE$INACT EQU   0011                Invalid action                               
GE$MISIF EQU   0200                Missing input field                          
GE$IPTL  EQU   0202                input too long                               
GE$NOTN  EQU   0204                Not numeric                                  
GE$ISEQ  EQU   0212                Invalid action sequence                      
GE$YRF   EQU   0088                Upgrade now                                  
*&&UK                                                                           
GE$USN   EQU   0089                Upgrade soon                                 
GE$FLERR EQU   1000                FALINK error - code &T                       
*&&                                                                             
*&&US                                                                           
GE$FLERR EQU   0075                FALINK error - code &T                       
GE$USN   EQU   0117                Upgrade soon                                 
*&&                                                                             
                                                                                
TWMXDSP  EQU   3000                Maximum screen size allowed                  
CONFLDQ  EQU   99                  Field number of control field                
LINLEN   EQU   79                  Width of data line                           
FIRSTLEN EQU   LINLEN-L'FALCON                                                  
                                                                                
EOT      EQU   0                   End of table                                 
EOB      EQU   0                   End of buffer                                
SCHARQ   EQU   C'('                Start of series character                    
ECHARQ   EQU   C')'                End of series character                      
MONLY    EQU   C'%'                Mapcode only                                 
DONLY    EQU   FF                  Only element code required (L'=0)            
REPEATQ  EQU   C'?'                Repeat character                             
ELEMIDQ  EQU   C'='                Element identifier                           
LCHARL   EQU   63                  Normal multiple character length             
LCHAR    EQU   C'+'                Normal multiple character                    
XBASIS   EQU   LCHARL*3            Extended mapcode character length            
XCHAR    EQU   C'-'                Extended mapcode character                   
                                                                                
YES      EQU   C'Y'                                                             
MORE     EQU   C'C'                More for this download                       
FF       EQU   X'FF'                                                            
                                                                                
V1Q      EQU   1                                                                
V2Q      EQU   2                                                                
V3Q      EQU   3                                                                
V4Q      EQU   4                   RUNNER server support                        
V5Q      EQU   5                   Asynchronous download support                
V6Q      EQU   6                   Expanded map code support                    
V7Q      EQU   7                                                                
V8Q      EQU   8                                                                
V9Q      EQU   9                                                                
         EJECT                                                                  
***********************************************************************         
* Local working storage                                               *         
***********************************************************************         
                                                                                
WORKD    DSECT ,                                                                
                                                                                
FALKWEYE DS    CL8                 Saved storage eyecatcher '*FALKWK*'          
                                                                                
DUB      DS    D                                                                
DMCB     DS    8F                  Parameter list                               
DMCBL    EQU   *-DMCB                                                           
FULL     DS    F                                                                
CALLR1   DS    A                   A(Caller's R1)                               
CALLRD   DS    A                   A(Caller's RD) (for hook routine)            
CLIENTRD DS    A                   Client's RD                                  
CLIHOOK  DS    A                   A(client's upload/download routine)          
ACONVERT DS    A                   A(upload map conversion routine)             
                                                                                
DMGR     DS    A                   V(DATAMGR)                                   
DATCON   DS    A                   V(DATCON)                                    
HEXOUT   DS    A                   V(HEXOUT)                                    
HEXIN    DS    A                   V(HEXIN)                                     
GETTXT   DS    A                   V(GETTXT)                                    
CASHVAL  DS    A                   V(CASHVAL)                                   
CUREDIT  DS    A                   V(CUREDIT)                                   
VSYSFAC  DS    A                   V(SYSFACS)                                   
PROTON   DS    A                   V(PROTON)                                    
PROTOFF  DS    A                   V(PROTOFF)                                   
GETFACT  DS    A                   V(GETFACT)                                   
SHIPIT   DS    A                   V(SHIPIT)                                    
                                                                                
SAVER1   DS    A                   A(parameter list)                            
SAVERE   DS    A                   Saved RE value                               
FUTL     DS    A                   A(UTL)                                       
FTWA     DS    A                   A(TCBTWA)                                    
FXATBUFF DS    A                   A(XA TBUFF)                                  
FLNK     DS    A                   A(TCBLNK)                                    
FLNKX    DS    A                   A(byte following FLNK)                       
                                                                                
EOBYTE   DS    X                   Download partial line process flags          
EOBUFF1  EQU   X'80'                                                            
                                                                                
FAUPIND  DS    X                   FALINK upload                                
FAUPBULK EQU   X'80'               .  Bulk upload                               
FAUPMORE EQU   X'08'               .  More blocks in XA TBUFF                   
                                                                                
AFILL    DS    A                   A(current fill line) for download            
FILLFROM DS    A                                                                
FILLMAX  DS    H                                                                
LENMOVED DS    H                   Amount of current fill line used             
HALF     DS    H                                                                
                                                                                
TEMPLEN  DS    X                                                                
TEMP     DS    XL256                                                            
                                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
                                                                                
ELEMENT  DS    XL256               Element build area                           
                                                                                
WORKDTYP DS    X                   Data type                                    
WORKLEN  DS    X                   Length of data in work                       
WORK     DS    XL256               Temporary work area                          
WORK2    DS    XL256               Temporary work area 2                        
                                                                                
FAPARMS  DS    XL(FALINKDL)                                                     
                                                                                
HDRSCREL DS    XL(HEADELNQ)                                                     
DATSCREL DS    XL(DATAELNQ)                                                     
WORKDL   EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* TWA layout                                                          *         
***********************************************************************         
                                                                                
       ++INCLUDE FATWA                                                          
TWASCR   DS    0X                                                               
TWAMSGH  DS    XL8                 Message field header                         
TWAMSG   DS    CL60                Message field                                
TWASRVH  DS    XL8                 Service field header                         
TWASRV   DS    CL17                Service field                                
                                                                                
***********************************************************************         
* Saved working storage                                               *         
***********************************************************************         
                                                                                
SAVED    DSECT ,                                                                
                                                                                
FALKSEYE DS    CL8                 Saved storage eyecatcher '*FALKSV*'          
                                                                                
ANXTBUFF DS    A                   Current position in buffer                   
ABUFFLEN DS    A                   Total output buffer length                   
APREVLEN DS    A                   Total sent so sar                            
AMAPNTRY DS    A                   A(MHELD)                                     
AWORKD   DS    A                   A(WORKD)                                     
NSCRCHAR DS    A                   Number of data characters on screen          
                                                                                
PAGEPREV DS    X                   First page of TEMPEST allocation             
PAGETHIS DS    X                   Number of pages allocated                    
PAGEINDX DS    X                   Current TEMPEST/TEMPSTR page                 
PAGEHIGH DS    X                   Highest TEMPEST/TEMPSTR page used            
                                                                                
VRSN     DS    X                   Communications version number                
FSTLIN#  DS    X                   First line of FALINK screen                  
FRAMENUM DS    XL2                 Frame number of download                     
FRAMETOT DS    XL2                 Total number of frames in download           
THISYEAR DS    CL4                 C'YYYY' this year                            
SAVE     DS    XL256               Saved previous data item                     
SAVELEN  DS    X                   Length of data in save                       
SAVEELEM DS    XL12                Saved element                                
SAVEMAP  DS    XL(L'MDCODE)        MDCODE of item in save                       
SAVEDTYP DS    X                   Saved type                                   
SAVEMTXT DS    XL(L'MDTEXT)        MDTEXT of item in save                       
                                                                                
SFLAG1   DS    X                                                                
SFBOS    EQU   X'80'               Series of matching map codes begun           
SFSCHAR  EQU   X'40'               Start character required                     
SFECHAR  EQU   X'20'               End character required                       
SFSDPEND EQU   X'10'               Data write pending                           
                                                                                
SFLAG2   DS    X                                                                
SFTMPEST EQU   X'80'               Uses TEMPEST (else uses TEMPSTR)             
SFSHARE  EQU   X'40'               Uses half of standard allocation             
SFSGLBRQ EQU   X'20'               Requested break for GLOBBER                  
SFSSTPRQ EQU   X'10'               Requested break                              
SFULBQ   EQU   X'08'               Buffer full break required                   
SFULPQ   EQU   X'04'               Buffer full break in process                 
SFLSTQ   EQU   X'02'               Last page being processed                    
                                                                                
SFLAG3   DS    X                                                                
SF21E    EQU   X'80'               First element of upload found                
SF21D    EQU   X'40'               First data of upload found                   
SF2USN   EQU   X'20'               Upgrade soon message required                
SF2YRF   EQU   X'10'               Upgrade now message required                 
                                                                                
SAVESHIP DS    10F                 SHIPIT Save values                           
                                                                                
OSIN     DS    XL4                 Original SIN                                 
                                                                                
SAVEL    EQU   *-SAVED                                                          
SAVEFREE EQU   K-SAVEL             Free space in SAVED                          
         EJECT                                                                  
***********************************************************************         
* Other included books                                                *         
***********************************************************************         
                                                                                
       ++INCLUDE FALINKBLK                                                      
                                                                                
*DDFH                                                                           
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*FAGETTXTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*FATBHD                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATBHD                                                         
         PRINT ON                                                               
*FATCB                                                                          
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
*FASSB                                                                          
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*FASYSFAC                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
*FASHIPBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASHIPBLK                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007FALINK    08/18/15'                                      
         END                                                                    
