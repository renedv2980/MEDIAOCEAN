*          DATA SET DEDALYO    AT LEVEL 011 AS OF 10/17/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEDALYOA                                                                 
         TITLE 'DEMCON - LOCAL DAILIES TPT CONVERSION - OUTPUT PHASE'           
DEDALYO  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TPTWRKX-TPTWRKD,**DAILYO,R7                                      
         USING TPTWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     DS    0H                                                               
*                                                                               
* DEIS OCT/2019: DEDALYI WAS JUST CHANGED, AND IT NOW USES FIELD "TEMP"         
*  IN CASES WHERE IT HAD NOT BEFORE. THIS EXPOSED PRE-EXISTING BUGS IN          
*  THIS PROGRAM, BECAUSE IT BUILDS ITS ELEMENTS IN FIELD "TEMP" AND (AS         
*  WE SAW DURING TESTING), SOME ELEMENTS WERE BEING BUILT WITH GARBAGE          
*  IN THEM. DISCRETION BEING THE BETTER PART OF VALOR, WE ARE CLEARING          
*  FIELD "TEMP" HERE UNCONDITIONALLY BEFORE WE DO ANYTHING ELSE.                
*                                                                               
         XC    TEMP,TEMP           ELEMENTS ARE BUILT IN THIS FIELD             
*                                                                               
         MVI   EXFLG,0                                                          
         CLI   INTRTYP,BSCODEQU    STATION/MARKET RECORD                        
         BE    CNV10                                                            
         CLI   INTRTYP,DRCODEQU    RATINGS RECORD                               
         BE    CNV14                                                            
*                                                                               
         CLI   INTKEY,0                                                         
         BE    CNVX                                                             
         CLI   INTKEY,1                                                         
         BE    MERGREC                                                          
*                                                                               
         CLI   INTRTYP,C'X'                                                     
         BNE   CNVX                                                             
         MVI   EXFLG,1                                                          
         B     XCL                                                              
*                                                                               
CNV4     XC    SATLASTP,SATLASTP   CLEAR SATELLITE STATION TABLE                
         XC    TEMP,TEMP           SEE COMMENT AT TOP RE: FIELD "TEMP"          
         OC    LOMINOR,LOMINOR                                                  
         BZ    CNVX                                                             
         MVI   EXFLG,2                                                          
         B     XCL20               LAST EXCLUSION                               
*                                                                               
CNVXH    DS    0H                                                               
         LHI   R0,1                                                             
         J     CNVXCR                                                           
                                                                                
CNVXL    DS    0H                                                               
         LHI   R0,-1                                                            
         J     CNVXCR                                                           
                                                                                
CNVXE    DS    0H                                                               
         SR    R0,R0                                                            
         J     CNVXCR                                                           
                                                                                
CNVXCR   DS    0H                                                               
         AR    R0,RB                                                            
         CR    R0,RB                                                            
         J     CNVX                                                             
                                                                                
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD STATION/MARKET, MARKET/STATION & STATION/BOOK RECORDS                   
*                                                                               
CNV10    LA    R6,THISKEY                                                       
         USING BSKEY,R6            BUILD STATION/MARKET RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   BSCODE,BSCODEQU                                                  
         MVC   BSMEDIA,MEDIA                                                    
         MVC   BSSRC,OUTSRC                                                     
         MVC   BSBOOK,INTBOOK                                                   
         XC    BSBOOK,=X'FFFF'                                                  
         MVI   BSIND,BSINDEQU                                                   
         MVC   BSSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   BSKMKT,INTMRKT                                                   
         MVC   BSSTYP,INTSTYP                                                   
         MVC   BSBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   BSRMKT,INTMRKT                                                   
         MVC   BSNDXDA(3),INTPNUM                                               
         GOTO1 ABLDREC,DMCB,(C'E',BSKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING MLKEY,R6            BUILD MARKET/STATION RECORD                  
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,INTMRKT                                                   
         MVC   MLSTAT,INTSTA                                                    
         XC    MLKMKT,MLKMKT                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   MLKMKT,INTMRKT                                                   
         MVC   MLBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   MLNDXDA(3),INTPNUM                                               
         GOTO1 ABLDREC,DMCB,(C'E',MLKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING SBKEY,R6            BUILD STATION/BOOK RECORD                    
         XC    THISKEY,THISKEY                                                  
         MVI   SBCODE,SBCODEQU                                                  
         MVC   SBMEDIA,MEDIA                                                    
         MVC   SBSRC,OUTSRC                                                     
         MVC   SBSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SBKMKT,INTMRKT                                                   
         MVC   SBSTYP,INTSTYP                                                   
         MVC   SBBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   SBRMKT,INTMRKT                                                   
         MVC   SBBOOK,INTBOOK                                                   
         MVC   SBNDXDA(3),INTPNUM                                               
         GOTO1 ABLDREC,DMCB,(C'E',SBKEY)                                        
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
* BUILD DEMO RECORDS                                                            
*                                                                               
CNV14    LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
*        CLI   EXFLG,0                                                          
*        BE    CNV15                                                            
*        MVC   INTPNAM(14),=C'STATIONEXCLUDE'                                   
*        MVI   INTDAY,X'FF'                                                     
*        MVI   INTSQH,X'FF'                                                     
*        MVI   INTEQH,X'FF'                                                     
*                                                                               
*                                                                               
CNV15    XC    THISKEY,THISKEY                                                  
         MVI   DRCODE,DRCODEQU                                                  
         MVC   DRMEDIA,MEDIA                                                    
         MVC   DRSRC,OUTSRC                                                     
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRBOOK,INTBOOK                                                   
         XC    DRBOOK,=X'FFFF'                                                  
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   DRKMKT,INTMRKT                                                   
         MVC   DRSTYP,INTSTYP                                                   
         MVC   DRBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTEQH                                                   
         MVI   CONDFLAG,0          SET DEMO ELEMENT BUILD FLAG                  
*                                  MAJOR KEY CONTROL BREAK                      
CNV18    XC    PREVKEY,PREVKEY     CLEAR LAST TIME VALUES                       
         XC    PREVPNAM,PREVPNAM                                                
*                                  BUILD DEMO RECORDS                           
CNV20    GOTO1 ABLDREC,DMCB,THISKEY                                             
         MVC   PREVKEY,THISKEY                                                  
*                                                                               
*        CLI   EXFLG,0                                                          
*        BE    CNV21                                                            
*        LA    R6,TEMP                                                          
*        USING MRGRELEM,R6                                                      
*        MVI   MRGRCODE,MRGRCDEQ                                                
*        MVI   MRGRLEN,MRGRLENQ                                                 
*        MVI   MRGRTYPE,X'80'      DUMMY TYPE                                   
*        MVC   MRGREYEC(5),=C'DUMMY'                                            
*        GOTO1 APUTEL,MRGRELEM                                                  
*        B     CNV36                                                            
*                                                                               
CNV21    LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET ELEMENT                         
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARTYPE,INTMTYP                                                  
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
*                                                                               
         USING SATELEM,R6          BUILD SATELLITE STATION ELEMENT              
         OC    SATLASTP,SATLASTP                                                
         BZ    CNV28               EXIT IF TABLE EMPTY                          
         LA    RE,SATTAB                                                        
*                                  LOCATE PARENT IN SATELLITE TABLE             
CNV22    CLI   0(RE),0                                                          
         BE    CNV28                                                            
         CLI   0(RE),C'P'          TEST IF A PARENT ENTRY                       
         BNE   *+14                                                             
         CLC   INTSTA,1(RE)        TEST IF CORRECT PARENT                       
         BE    *+12                                                             
         LA    RE,L'INTSTA+1(RE)   NO - BUMP TO NEXT                            
         B     CNV22                                                            
*                                                                               
         LA    RE,L'INTSTA+1(RE)   RE=A(FIRST SATELLITE ENTRY)                  
         LA    R1,SATCALL          R1=A(SATELLITE IN ELEMENT)                   
         MVI   SATCODE,SATCODEQ                                                 
*                                  BUILD LIST OF SATELLITES IN ELEMENT          
CNV24    CLI   0(RE),C'S'                                                       
         BNE   CNV26                                                            
         MVC   0(L'INTSTA,R1),1(RE)                                             
         LA    R1,L'SATCALL(R1)                                                 
         LA    RE,L'INTSTA+1(RE)                                                
         B     CNV24                                                            
*                                                                               
CNV26    SR    R1,R6                                                            
         STC   R1,SATLEN           SET ELEMENT LENGTH                           
         GOTO1 APUTEL,SATELEM                                                   
*                                  BUILD QTR HOUR ELEMENT                       
CNV28    LA    R6,TEMP                                                          
         USING QHELEM,R6                                                        
         MVI   QHCODE,QHCODEQ                                                   
         MVC   QHDAY,INTDAY                                                     
         MVC   QHSQH,INTSQH                                                     
         MVC   QHEQH,INTEQH                                                     
         MVC   QHWKS,INTWEEKS                                                   
*                                                                               
         XC    INTPNAM,INTPNAM                                                  
         MVC   INTPNAM(13),=C'NOT AVAILABLE'                                    
*                                                                               
         MVC   QHPNAME(L'INTPNAM),INTPNAM                                       
         LA    R1,QHPNAME+L'INTPNAM-1                                           
         CLI   0(R1),C' '          LOCATE END OF PROGRAM NAME                   
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         SR    R1,R6                                                            
         STC   R1,QHELN            SET ELEMENT LENGTH                           
*                                                                               
         LA    R3,TEMP2            BUILD INFO ELEMENT                           
         USING QIELEM,R3                                                        
         MVI   QICODE,QICODEQ                                                   
         MVI   QIELN,QIELNEQ1                                                   
*        MVC   QIPNUM(3),INTPNUM+1                                              
         XC    QIPNUM,QIPNUM       SINCE WE DON'T SET IT, CLEAR IT!             
         MVC   QIPTYPE,INTPTYP                                                  
         MVC   QIPNAM6,INTPNAM6                                                 
         MVI   QIREV,1             REVISION 1 FOR PROGRAM SOURCE                
         MVC   QIPRSRC,INTPRSRC    PROGRAM SOURCE                               
         MVC   QIAFFIL,MYSPACES                                                 
         OC    INTAFFL,INTAFFL     IF AFFIL IS SPACES OR ZEROS                  
         BZ    CNV29                 LEAVE IT AS SPACES                         
         CLC   INTAFFL,MYSPACES                                                 
         BE    CNV29                                                            
         MVC   QIAFFIL,INTAFFL     OTHERWISE, COPY FROM INTERM REC              
         DROP  R3                                                               
CNV29    CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV30                                                            
         GOTO1 APUTEL,QHELEM       NO - ADD ELEMENT                             
*                                                                               
         CLI   INTSTA+4,C'U'       SKIP MKTACT IF UNIVERSE DEMOS                
         BE    CNV30                                                            
*                                                                               
         TM    INTSTA,X'F0'                                                     
         BO    CNV30                                                            
*                                  GO RETREIVE DMA UNIVERSES FOR WIRED          
         CLI   INTBTYP,BOOKTYPE_W  L+7 WIRED                                    
         BE    *+8                                                              
         CLI   INTBTYP,BOOKTYPE_Z  LIVE WIRED                                   
         BE    *+8                                                              
         CLI   INTBTYP,BOOKTYPE_W3 L+3 WIRED                                    
         BE    *+8                                                              
         CLI   INTBTYP,BOOKTYPE_WS L+SD WIRED ONLY                              
         BE    *+8                                                              
         CLI   INTBTYP,BOOKTYPE_R  L+7 WIRED PARENT ONLY                        
         BE    *+8                                                              
         CLI   INTBTYP,BOOKTYPE_S  LIVE WIRED PARENT ONLY                       
         BE    *+8                                                              
         CLI   INTBTYP,BOOKTYPE_S3 L+3 WIRED PARENT ONLY                        
         BE    *+8                                                              
         CLI   INTBTYP,BOOKTYPE_SS L+SD WIRED PARENT ONLY                       
         BNE   *+8                                                              
         BAS   RE,GETDUNIV                                                      
*                                                                               
CNV30    CLI   CONDFLAG,0          TEST IF DEMO ELEMENTS BUILT                  
         BNE   CNV33                                                            
CNV31    LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTBOOK                                                  
         ST    RA,DBCOMFCS                                                      
*                                                                               
         MOVE  (CONDLEN,1000),INTACCS                                           
         GOTO1 CDEMEL,DMCB,(C'C',0),DBLOCKD,CONDLEN                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CONDFLAG,1                                                       
         LA    RE,XTENDTAB         RE=TABLE POINTER                             
         LA    R0,XTENDENT         R0=TABLE ENTRY COUNTER                       
         CLC   MEDIA(2),0(RE)      TEST FOR EXTENDED DATA FILE                  
         BE    *+16                YES                                          
         LA    RE,L'XTENDTAB(RE)                                                
         BCT   R0,*-14                                                          
         B     CNV32                                                            
         DROP  R5                                                               
*                                                                               
CNV32    GOTO1 CHELLO,DMCB,(C'P',CORE),CONDLEN,TEMP2                            
*                                                                               
CNV33    LA    R1,CONDWRK1         NO - ADD DEMO ELEMENTS TO RECORD             
         SR    R0,R0                                                            
*                                                                               
CNV34    CLI   0(R1),0             TEST E-O-L                                   
         BE    CNV36                                                            
*        CLI   EXFLG,1                                                          
*        BNE   CNV35X                                                           
*        CLI   0(R1),X'5E'                                                      
*        BE    CNV35X                                                           
*        B     CNV35X2                                                          
***********************************************************************         
CNV35X   GOTO1 APUTEL,(R1)                                                      
CNV35X2  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CNV34                                                            
*                                  SAVE THIS TIME VALUES                        
CNV36    GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
***********************************************************************         
         DROP  R6                                                               
         SPACE 2                                                                
*                                                                               
*============================= EXCLUSION ROUTINE =====================*         
XCL      DS    0H                                                               
         LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         XC    THISKEY,THISKEY                                                  
         MVI   DRCODE,DRCODEQU     R                                            
         MVC   DRMEDIA,MEDIA       O                                            
         MVC   DRSRC,OUTSRC        N                                            
         MVC   DRSTAT,INTSTA       STATION                                      
         MVC   DRBOOK,INTBOOK      X'FF'ED BOOK                                 
         XC    DRBOOK,=X'FFFF'                                                  
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   DRKMKT,INTMRKT      MARKET IF APPLICABLE                         
         MVC   DRSTYP,INTSTYP      STATION TYPE (PARENT+)                       
         MVC   DRBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTEQH                                                   
*                                                                               
         CLC   PRVXCLK(18),DRKEY   MAJOR KEY CHANGED?                           
         BNE   XCL20                                                            
         MVI   EXFLG,3             ONLY 1 EXCLUSION PER RECORD                  
         CLC   DRKMINOR,LOMINOR    LOWER THAN LOWER MINOR?                      
         BNL   XCL10                                                            
         MVC   LOMINOR,DRKMINOR                                                 
         B     XCLX                IN BETWEEN?  MOVE ON                         
XCL10    CLC   DRKMINOR,HIMINOR    HIGHER THAN HIGH MINOR?                      
         BNH   XCLX                                                             
         MVC   HIMINOR,DRKMINOR                                                 
         B     XCLX                                                             
*                                                                               
XCL20    DS    0H                                                               
         OC    LOMINOR,LOMINOR                                                  
         BNZ   *+14                                                             
         MVC   LOMINOR,DRKMINOR                                                 
         B     XCL30                                                            
*                                                                               
* "FULL REPLACEMENT" MODE WILL BE REQUIRED DURING THE MERGE.                    
* NOTIFY DEIS OF THIS FACT VIA E-MAIL.                                          
*                                                                               
* UNCOMMENT THIS BLOCK OF CODE TO SEND THE E-MAIL.                              
*&&DO                                                                           
         BC    0,XCL25             ONLY DO THIS ONCE                            
         MVI   *-3,X'F0'           *** SELF-MODIFYING CODE ***                  
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',(L'NTFYMSG1,NTFYMSG1)                    
*                                                                               
XCL25    DS    0H                                                               
*&&                                                                             
         MVC   PRVXCLK+DRKMINOR-DRKEY(L'DRKMINOR),LOMINOR    MINOR KEY          
         MVI   CONDFLAG,0          SET DEMO ELEMENT BUILD FLAG                  
*                                  MAJOR KEY CONTROL BREAK                      
         GOTO1 ABLDREC,DMCB,PRVXCLK                                             
         LA    R6,TEMP                                                          
         XC    TEMP(MRGRLENQ),TEMP                                              
         USING MRGRELEM,R6         BUILD MERGE ELEMENT                          
         MVI   MRGRCODE,MRGRCDEQ                                                
         MVI   MRGRLEN,MRGRLENQ                                                 
         MVI   MRGRTYPE,MRGRLOW                                                 
         MVC   MRGREYEC,MRGEYEC                                                 
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
*                                                                               
         LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         MVC   PRVXCLK+DRKMINOR-DRKEY(L'DRKMINOR),HIMINOR    MINOR KEY          
         MVI   CONDFLAG,0                                                       
         GOTO1 ABLDREC,DMCB,PRVXCLK                                             
         LA    R6,TEMP                                                          
         XC    TEMP(MRGRLENQ),TEMP                                              
         USING MRGRELEM,R6         BUILD MERGE ELEMENT                          
         MVI   MRGRCODE,MRGRCDEQ                                                
         MVI   MRGRLEN,MRGRLENQ                                                 
         MVI   MRGRTYPE,MRGRHIGH                                                
         MVC   MRGREYEC,MRGEYEC                                                 
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
         XC    LOMINOR,LOMINOR                                                  
         XC    HIMINOR,HIMINOR                                                  
         B     XCL                                                              
*                                                                               
XCL30    MVC   PRVXCLK,THISKEY                                                  
*                                                                               
XCLX     DS    0H                                                               
*        CLI   EXFLG,1                                                          
*        BE    CNV14                                                            
         B     CNVX                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*============================= EXCLUSION ROUTINE =====================*         
MERGREC  DS    0H                                                               
         LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         XC    THISKEY,THISKEY                                                  
*                                  MAJOR KEY CONTROL BREAK                      
         GOTO1 ABLDREC,DMCB,THISKEY                                             
         LA    R6,TEMP                                                          
         XC    TEMP(MRGRLENQ),TEMP                                              
         USING MRGRELEM,R6         BUILD MERGE ELEMENT                          
         MVI   MRGRCODE,MRGRCDEQ                                                
         MVI   MRGRLEN,MRGRLENQ                                                 
         MVI   MRGRTYPE,0                                                       
         MVC   MRGREYEC,MRGEYES                                                 
*                                                                               
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
*                                                                               
* "FULL REPLACEMENT" MODE WILL BE REQUIRED DURING THE MERGE.                    
* NOTIFY DEIS OF THIS FACT VIA E-MAIL.                                          
*                                                                               
* UNCOMMENT THIS BLOCK OF CODE TO SEND THE E-MAIL.                              
*&&DO                                                                           
         BC    0,MERGRECX          ONLY DO THIS ONCE                            
         MVI   *-3,X'F0'           *** SELF-MODIFYING CODE ***                  
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',(L'NTFYMSG1,NTFYMSG1)                    
*                                                                               
MERGRECX DS    0H                                                               
*&&                                                                             
         B     CNVX                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*======================== RETREIVE DMA UNIVERSE ======================*         
* For a given Wired Cable market, go and retrieve the DMA universe              
* and Total demo elements from the DMA records.                                 
*                                                                               
GETDUNIV NTR1                                                                   
*                                                                               
         LA    RE,DMABUFF                                                       
         LA    RF,DMABUFFQ                                                      
         XCEF                                                                   
*                                  FIND THE MKT#T (0101T) RECORD                
         LA    R2,TEMPKEY                                                       
         USING DRKEY,R2                                                         
         MVC   TEMPKEY(L'THISKEY),THISKEY                                       
         CLI   DRBTYP,BOOKTYPE_Z   LIVE WIRED LOOK FOR LIVE DUNIV               
         BNE   *+12                                                             
         MVI   DRBTYP,BOOKTYPE_L                                                
         B     GD10                                                             
         CLI   DRBTYP,BOOKTYPE_S   LIVE PARENT ONLY LOOK FOR L+3 DUNIV          
         BNE   *+12                                                             
         MVI   DRBTYP,BOOKTYPE_L                                                
         B     GD10                                                             
*                                                                               
         CLI   DRBTYP,BOOKTYPE_WS  L+SD WIRED LOOK FOR L+SD DUNIV               
         BNE   *+12                                                             
         MVI   DRBTYP,BOOKTYPE_LS                                               
         B     GD10                                                             
         CLI   DRBTYP,BOOKTYPE_SS  L+1 PARENT ONLY LOOK FOR L+1 DUNIV           
         BNE   *+12                                                             
         MVI   DRBTYP,BOOKTYPE_LS                                               
         B     GD10                                                             
*                                                                               
         CLI   DRBTYP,BOOKTYPE_W3  L+3 WIRED LOOK FOR L+3 DUNIV                 
         BNE   *+12                                                             
         MVI   DRBTYP,BOOKTYPE_L3                                               
         B     GD10                                                             
         CLI   DRBTYP,BOOKTYPE_S3  L+3 PARENT ONLY LOOK FOR L+3 DUNIV           
         BNE   *+12                                                             
         MVI   DRBTYP,BOOKTYPE_L3                                               
         B     *+8                                                              
*                                                                               
         MVI   DRBTYP,BOOKTYPE_STANDARD ALL LIVE PLUS LOOK FOR STANDARD         
*                                                                               
GD10     EDIT  (B2,INTMRKT),(4,DRSTAT)                                          
         OC    DRSTAT(4),=C'0000'                                               
         MVI   DRSTAT+4,C'T'                                                    
         XC    DRKMKT,DRKMKT       NO SPILL FOR MARKET TOTALS                   
         MVC   KEYSAVE,TEMPKEY                                                  
         GOTO1 VDATAMGR,DMCB,READHI,DEMDIR,KEYSAVE,TEMPKEY                      
         CLC   KEYSAVE(L'DRKMAJOR),TEMPKEY                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AWREC                                                         
         MVC   DRKMAJOR,TEMPKEY                                                 
         MVC   DRRSTAT,TEMPKEY+(DRKSTAT-DRKMAJOR)                               
         MVC   TEMPDA,TEMPKEY+(DRNDXDA-DRKMAJOR)                                
         MVC   DRHIGHD,INTDAY      FILL MINOR KEY                               
         MVC   DRHIQHR,INTSQH      LOOK UP RECORD IN BLOCK                      
         GOTO1 VDATAMGR,DMCB,READHI,DEMFIL,TEMPDA,AWREC,WORKS                   
*                                                                               
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTBOOK                                                  
         ST    RA,DBCOMFCS                                                      
         LA    RE,DRFRSTEL-DRKMAJOR                                             
         L     R6,AWREC                                                         
         AR    R6,RE                                                            
         ST    R6,DBAQUART                                                      
         GOTO1 CDEMEL,DMCB,(C'E',0),DBLOCKD,CONDLEN                             
         BAS   RE,MERDUNIV         MERGE THE UNIVERSE WITH INTACCS              
*                                                                               
         DROP  R5                                                               
         DROP  R2                                                               
*                                                                               
GETDX    XIT1                                                                   
         GETEL R6,ELDISP,ELCODE                                                 
***********************************************************************         
*========================== MERGE DMA UNIVERSE =======================*         
* Merge the DEMO universes we just retrieved with intaccs.                      
*                                                                               
MERDUNIV NTR1                                                                   
         LA    R5,DMAELTAB                                                      
*                                                                               
MERD10   CLI   0(R5),X'FF'                                                      
         BE    MERDX                                                            
         LA    RE,INTACCS                                                       
         ZIC   R6,0(R5)                                                         
         SHI   R6,1                                                             
         MHI   R6,4                                                             
         AR    RE,R6                                                            
         LA    R1,CONDLEN                                                       
         AR    R1,R6                                                            
         ZIC   RF,1(R5)                                                         
         ZIC   R6,0(R5)                                                         
         SR    RF,R6                                                            
         AHI   RF,1                                                             
         MHI   RF,4                                                             
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)                                                    
         LA    R5,2(R5)                                                         
         B     MERD10                                                           
*                                                                               
MERDX    XIT1                                                                   
*                                                                               
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
* TABLE OF EXTENDED DATA FILES - BYTES 0-1 = MEDIA/SOURCE                       
*                                                                               
XTENDTAB DS    0CL2                                                             
         DC    C'ON'               US NSI                                       
XTENDENT EQU   (*-XTENDTAB)/L'XTENDTAB                                          
*                                                                               
TEMP2    EQU   TEMP+50                                                          
MYSPACES DC    CL30' '                                                          
MYDMCB   DS    6F                                                               
SVDA     DS    AL4                                                              
SVSTATUS DS    C                                                                
TEMPDYQH DS    XL2                                                              
SAVER1   DS    A                                                                
*                                                                               
ELDISP   DS    H                                                                
ELCODE   DS    X                                                                
*                                                                               
SVMRKT   DS    XL2                                                              
WORKS    DS    12D                                                              
TEMPDA   DS    F                                                                
*                                                                               
SATLASTP DC    A(0)                A(LAST PARENT IN SATTAB)                     
SATLASTS DC    A(0)                A(LAST SATELLITE IN SATTAB)                  
SATTAB   DS    50CL5               PARENT/SATELLITE STATION TABLE               
*                                  THIS TIME/LAST TIME VALUES                   
MYSTAT   DS    X                                                                
THISKEY  DC    XL24'00'                                                         
PREVKEY  DC    XL24'00'                                                         
PRVXCLK  DC    XL20'00'                                                         
TEMPKEY  DC    XL24'00'                                                         
KEYSAVE  DS    XL24                                                             
PREVDAY  DC    X'00'                                                            
PREVQHR  DC    X'00'                                                            
PREVPNAM DC    XL14'00'                                                         
MRGEYEC  DC    CL27'*MERGE REPLACEMENT ELEMENT*'                                
MRGEYES  DC    CL27'*REPLACEMENT MODE REQUIRED*'                                
MRGENOT  DC    CL27'**NO REPLACEMENT ELEMENTS**'                                
EXFLG    DC    X'00'                                                            
*                                                                               
PUTSW    DC    C'Y'                Y=PREVIOUS RECORD WRITTEN                    
DEMFIL   DC    CL7'DEMFIL'         HELLO FILE NAME                              
CORE     DC    C'CORETAB'          HELLO FILE NAME                              
DEMDIR   DC    CL7'DEMDIR'         DATAMGR DEMO DIRECTORY                       
READHI   DC    CL7'DMRDHI'         DATAMGR READ HIGH                            
*                                                                               
NTFYMSG1 DC    C'AUTONOTE*DEISN:** NOTE: LOCAL DAILIES USING FULL REPLA+        
               CEMENT MODE'                                                     
*                                                                               
LOMINOR  DS    XL2                                                              
HIMINOR  DS    XL2                                                              
*                                                                               
DMAELTAB DS    0H                                                               
         DC    AL1(OUHOMES,OUMETROB)   HOMES UNIVERSE                           
         DC    AL1(OUM1214,OUV611)     METERED UNIVERSE                         
         DC    AL1(OMHOMES,OMMETROB)   HOMES TOTAL                              
         DC    AL1(OMM1214,OMV611)     METERED TOTAL                            
         DC    X'FF'                                                            
DMABUFF  DS    XL(DMABUFFQ)                                                     
DMABUFFQ EQU   2000                                                             
         EJECT                                                                  
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
TPTWRKD  DSECT                                                                  
AREC     DS    A                                                                
AFRSTEL  DS    A                                                                
*DEMOREC DS    A                   A(WEEK-0 RECORD FROM DEMO FILE)              
RECLEN   DS    H                                                                
SVAOREC  DS    A                                                                
*                                                                               
MATHFACS DS    0F                  DEMOMATH BLOCK                               
MATHABLK DS    A                   A(USER DBLOCK)                               
MATHFCTR DS    F                                                                
MATHIFIL DS    CL3                                                              
MATHOFIL DS    CL3                                                              
MATHOSRC DS    CL3                                                              
MATHFACL EQU   *-MATHFACS                                                       
*                                                                               
DEMOVAL  DS    F                   DEMO VALUE                                   
DEMOVALS DS    0F                  DEMO VALUES                                  
DVDHOMES DS    F                   DHOMES                                       
DVUHOMES DS    F                   UHOMES                                       
*                                                                               
MYBYTE   DS    XL1                                                              
*                                                                               
CONDFLAG DS    X                                                                
CONDLEN  DS    XL2                                                              
CONDWRK1 DS    1000C                                                            
CONDWRK2 DS    1000C                                                            
*                                                                               
TPTWRKX  EQU   *                                                                
*                                                                               
* INTERIM RECORD DISPLACEMENTS                                                  
ORHOMES  EQU   1                                                                
ORHH1    EQU   2                                                                
ORHH2    EQU   3                                                                
ORHH3    EQU   4                                                                
ORHH4    EQU   5                                                                
OSHOMES  EQU   6                                                                
OPMETROA EQU   7                                                                
OSMETROA EQU   8                                                                
ORMETROA EQU   9                                                                
OPMETROB EQU   10                                                               
OSMETROB EQU   11                                                               
ORMETROB EQU   12                                                               
OTHOMES  EQU   13                                                               
ODHOMES  EQU   14                                                               
OTWWRK   EQU   15                                                               
ODWWRK   EQU   16                                                               
OTW65O   EQU   17                                                               
ODW65O   EQU   18                                                               
OTM65O   EQU   19                                                               
ODM65O   EQU   20                                                               
ODMETROA EQU   21                                                               
ODMETROB EQU   22                                                               
OTM1214  EQU   23                                                               
OTW1214  EQU   24                                                               
OTM1517  EQU   25                                                               
OTW1517  EQU   26                                                               
OTM1820  EQU   27                                                               
OTW1820  EQU   28                                                               
OTM2124  EQU   29                                                               
OTW2124  EQU   30                                                               
OTM2534  EQU   31                                                               
OTW2534  EQU   32                                                               
OTM3549  EQU   33                                                               
OTW3549  EQU   34                                                               
OTM5564  EQU   35                                                               
OTW5564  EQU   36                                                               
OTM5054  EQU   37                                                               
OTW5054  EQU   38                                                               
OTV25    EQU   39                                                               
ODV25    EQU   40                                                               
OTV611   EQU   41                                                               
ODV611   EQU   42                                                               
OUHOMES  EQU   43                                                               
OUM65O   EQU   44                                                               
OUW65O   EQU   45                                                               
OUWWRK   EQU   46                                                               
OUA1HH   EQU   47                                                               
OUA2HH   EQU   48                                                               
OUA3HH   EQU   49                                                               
OUMETROA EQU   50                                                               
OUMETROB EQU   51                                                               
OUM1214  EQU   52                                                               
OUW1214  EQU   53                                                               
OUM1517  EQU   54                                                               
OUW1517  EQU   55                                                               
OUM1820  EQU   56                                                               
OUW1820  EQU   57                                                               
OUM2124  EQU   58                                                               
OUW2124  EQU   59                                                               
OUM2534  EQU   60                                                               
OUW2534  EQU   61                                                               
OUM3549  EQU   62                                                               
OUW3549  EQU   63                                                               
OUM5564  EQU   64                                                               
OUW5564  EQU   65                                                               
OUM5054  EQU   66                                                               
OUW5054  EQU   67                                                               
OUV25    EQU   68                                                               
OUV611   EQU   69                                                               
ODM1214  EQU   70                                                               
ODW1214  EQU   71                                                               
ODM1517  EQU   72                                                               
ODW1517  EQU   73                                                               
ODM1820  EQU   74                                                               
ODW1820  EQU   75                                                               
ODM2124  EQU   76                                                               
ODW2124  EQU   77                                                               
ODM2534  EQU   78                                                               
ODW2534  EQU   79                                                               
ODM3549  EQU   80                                                               
ODW3549  EQU   81                                                               
ODM5564  EQU   82                                                               
ODW5564  EQU   83                                                               
ODM5054  EQU   84                                                               
ODW5054  EQU   85                                                               
OMHOMES  EQU   86                                                               
OMWWRK   EQU   87                                                               
OMW65O   EQU   88                                                               
OMM65O   EQU   89                                                               
OMMETROA EQU   90                                                               
OMMETROB EQU   91                                                               
OMM1214  EQU   92                                                               
OMW1214  EQU   93                                                               
OMM1517  EQU   94                                                               
OMW1517  EQU   95                                                               
OMM1820  EQU   96                                                               
OMW1820  EQU   97                                                               
OMM2124  EQU   98                                                               
OMW2124  EQU   99                                                               
OMM2534  EQU   100                                                              
OMW2534  EQU   101                                                              
OMM3549  EQU   102                                                              
OMW3549  EQU   103                                                              
OMM5564  EQU   104                                                              
OMW5564  EQU   105                                                              
OMM5054  EQU   106                                                              
OMW5054  EQU   107                                                              
OMV25    EQU   108                                                              
OMV611   EQU   109                                                              
OQHOMES  EQU   110                                                              
OQWWRK   EQU   111                                                              
OQW65O   EQU   112                                                              
OQM65O   EQU   113                                                              
OQM1214  EQU   114                                                              
OQW1214  EQU   115                                                              
OQM1517  EQU   116                                                              
OQW1517  EQU   117                                                              
OQM1820  EQU   118                                                              
OQW1820  EQU   119                                                              
OQM2124  EQU   120                                                              
OQW2124  EQU   121                                                              
OQM2534  EQU   122                                                              
OQW2534  EQU   123                                                              
OQM3549  EQU   124                                                              
OQW3549  EQU   125                                                              
OQM5564  EQU   126                                                              
OQW5564  EQU   127                                                              
OQM5054  EQU   128                                                              
OQW5054  EQU   129                                                              
OQV25    EQU   130                                                              
OQV211   EQU   131                                                              
ORV2O    EQU   132                                                              
ORV18O   EQU   133                                                              
ORV1234  EQU   134                                                              
ORV1224  EQU   135                                                              
ORV1217  EQU   136                                                              
ORV611   EQU   137                                                              
ORV211   EQU   138                                                              
ORW18O   EQU   139                                                              
ORW1834  EQU   140                                                              
ORW1849  EQU   141                                                              
ORW2549  EQU   142                                                              
ORW2554  EQU   143                                                              
ORW1224  EQU   144                                                              
ORW2564  EQU   145                                                              
ORW1234  EQU   146                                                              
ORM18O   EQU   147                                                              
ORM1834  EQU   148                                                              
ORM1849  EQU   149                                                              
ORM2554  EQU   150                                                              
ORM2564  EQU   151                                                              
ORWWRK   EQU   152                                                              
ORM2549  EQU   153                                                              
ORA1849  EQU   154                                                              
ORA1834  EQU   155                                                              
ORA2554  EQU   156                                                              
OPV2O    EQU   157                                                              
OPV18O   EQU   158                                                              
OPV1234  EQU   159                                                              
OPV1224  EQU   160                                                              
OPV1217  EQU   161                                                              
OPV611   EQU   162                                                              
OPV211   EQU   163                                                              
OPW18O   EQU   164                                                              
OPW1834  EQU   165                                                              
OPW1849  EQU   166                                                              
OPW2549  EQU   167                                                              
OPW2554  EQU   168                                                              
OPW1224  EQU   169                                                              
OPW2564  EQU   170                                                              
OPW1234  EQU   171                                                              
OPM18O   EQU   172                                                              
OPM1834  EQU   173                                                              
OPM1849  EQU   174                                                              
OPM2554  EQU   175                                                              
OPM2564  EQU   176                                                              
OPWWRK   EQU   177                                                              
OPM2549  EQU   178                                                              
OPA1849  EQU   179                                                              
OPA1834  EQU   180                                                              
OPA2554  EQU   181                                                              
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTTPT3D                                                     
         EJECT                                                                  
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011DEDALYO   10/17/19'                                      
         END                                                                    
