*          DATA SET SRTST00    AT LEVEL 027 AS OF 03/02/18                      
*          DATA SET SRTST00    AT LEVEL 022 AS OF 05/22/02                      
*PHASE T13600A                                                                  
         TITLE '$TEST - DISPLAY/ALTER TEST TABLE'                               
         PRINT NOGEN                                                            
TEST     CSECT                                                                  
         NMOD1 WORKL,*$TST**,RA,R9,CLEAR=YES,RR=RE                              
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         MVC   IPARMS,0(R1)                                                     
*                                                                               
         L     R3,ATWA                                                          
         USING SRTSTFFD,R3         R3=A(TWA)                                    
         BRAS  RE,INIT                                                          
         BRAS  RE,MAIN                                                          
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVI   HDRN,1                                                           
         MVC   ASAVE,ATIA          USE TIA AS SAVE AREA                         
*                                                                               
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         ST    RF,AERRMSGS                                                      
         L     RF,=A(OKMSGS)                                                    
         A     RF,RELO                                                          
         ST    RF,AOKMSGS                                                       
*                                                                               
         L     RF,ATIOB            EXTRACT TIOB DATA                            
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID                                                    
         CLI   PFKEY,1             TEST HELP PFKEY                              
         BNE   INIT02                                                           
*                                                                               
         XR    RE,RE               PF1 HELP                                     
         ICM   RE,3,TIOBCURD                                                    
         AR    RE,R3                                                            
         ST    RE,AHELP            SAVE A(HELP FIELD)                           
         MVI   PFKEY,0                                                          
*                                                                               
INIT02   L     RF,ACOMFACS                                                      
         USING COMFACSD,RF         RF=A(COMFAC LIST)                            
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   AGETTXT,CGETTXT                                                  
         MVC   AGETHELP,CGETHELP                                                
*                                                                               
         L     RF,ASYSFACS                                                      
         USING SYSFACD,RF          R4=A(SYSFAC LIST)                            
         MVC   ASSB,VSSB                                                        
         MVC   ATSTTAB,VTSTTAB                                                  
         MVC   AUTLTAB,VUTL                                                     
*                                                                               
         L     RF,AMYUTL           EXTRACT LOCAL UTL DATA                       
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BZ    *+8                                                              
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
*                                                                               
         L     RF,ASSB             EXTRACT SSB DATA                             
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   SYSNAME,SSBSYSN4                                                 
         DROP  RF                                                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,(0,=CL8'FILTAB'),0                                 
         MVC   AFILTAB,DMCB+4                                                   
*                                                                               
         MVC   MSG,SPACES          CLEAR OUTPUT AREA                            
         BRAS  RE,READSTR          READ SAVED STR                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,DTFADD,TSTRCVR                                     
         XR    RF,RF                                                            
         ICM   RF,7,13(R1)         A(DTF) ALWAYS IN LOW CORE                    
         ST    RF,ADTF             REMEMBER FILE # RETURNED AS HOB              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM                                                        *         
***********************************************************************         
         SPACE 1                                                                
MAIN     NTR1  ,                                                                
         CLI   PFKEY,3             PF3 MEANS EXIT TO LIST                       
         BE    MAIN02                                                           
         CLI   SVACTN,6            ACTION 6 DISPLAY TWAS                        
         BNE   MAIN04                                                           
         CLI   PFKEY,0             ENTER MEANS BACK TO LIST                     
         BE    MAIN02                                                           
         MVI   ACTN,6              ELSE RESET ACTION AND ID                     
         MVC   ID,SVTSTID                                                       
         B     MAIN14                                                           
*                                                                               
MAIN02   BRAS  RE,LIST                                                          
         B     XMOD                                                             
*                                                                               
MAIN04   BRAS  RE,VALP1            VALIDATE TEST ID                             
         BH    HELPOUT             ASKED FOR HELP                               
         BL    EXITL                                                            
*                                                                               
         L     RF,AMYUTL           TEST SPECIAL ACTIONS                         
         USING UTLD,RF                                                          
         CLC   ID,=CL4'ION '       IO COUNT ON                                  
         BNE   MAIN06                                                           
         OI    TSTAT5,TST5IOCT                                                  
         MVI   HDRN,2                                                           
         B     EXITOK                                                           
*                                                                               
MAIN06   CLC   ID,=CL4'IOFF'       IO COUNT OFF                                 
         BNE   MAIN08                                                           
         NI    TSTAT5,255-TST5IOCT                                              
         MVI   HDRN,3                                                           
         B     EXITOK                                                           
*                                                                               
MAIN08   CLC   ID,=CL4'PON '       TRACE PROFILE ON                             
         BNE   MAIN10                                                           
         NI    TSTAT9,255-TST9PROF                                              
         MVI   HDRN,4                                                           
         B     EXITOK                                                           
*                                                                               
MAIN10   CLC   ID,=CL4'POFF'       TRACE PROFILE OFF                            
         BNE   MAIN12                                                           
         OI    TSTAT9,TST9PROF                                                  
         MVI   HDRN,5                                                           
         B     EXITOK                                                           
*                                                                               
MAIN12   CLC   ID,=CL4'SON '       CPU ON                                       
         BNE   MAIN12A                                                          
         OI    TSTAT9,TST9SCRP                                                  
         MVI   HDRN,9                                                           
         B     EXITOK                                                           
*                                                                               
MAIN12A  CLC   ID,=CL4'SOFF'       SCRIPT TRACE                                 
         BNE   MAIN12B                                                          
         NI    TSTAT9,255-TST9SCRP                                              
         MVI   HDRN,10                                                          
*                                                                               
MAIN12B  CLC   ID,=CL4'CON '       CPU TRACE ON                                 
         BNE   MAIN12C                                                          
         OI    TSTATC,TSTCCPU                                                   
         MVI   HDRN,11                                                          
         B     EXITOK                                                           
*                                                                               
MAIN12C  CLC   ID,=CL4'COFF'       CPU TRACE OFF                                
         BNE   MAIN13                                                           
         OI    TSTATC,TSTCCPU                                                   
         MVI   HDRN,12                                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
MAIN13   BRAS  RE,VALP2            VALIDATE ACTION                              
         BH    HELPOUT                                                          
         BL    EXITL                                                            
*                                                                               
         CLC   SVACTN,ACTN         SAME AS LAST TIME?                           
         BE    MAIN14              YES                                          
         XC    SVDATA,SVDATA                                                    
*                                                                               
MAIN14   MVC   SVACTN,ACTN     <== NOTE: EP FROM LIST SUBROUTINE                
         MVC   SVTSTID,ID                                                       
         CLC   ID,ALL              IF ID=ALL THEN DO LIST SCREEN                
         BNE   *+12                                                             
         BRAS  RE,LIST                                                          
         B     XMOD                                                             
*                                                                               
         BRAS  RE,FINDTAB          FIND ENTRY FOR USER R5                       
*                                                                               
         CLI   ACTN,5          *** DISPLAY UPDATE LOG                           
         BNE   MAIN16                                                           
         MVI   BYTE,X'FD'          MAKE SURE FD SCREEN IN                       
         BRAS  RE,LOADSCRN                                                      
         MVC   SRVPFK,PFKLIN2                                                   
         BRAS  RE,DUPD             DISPLAY UPDATES                              
         B     XMOD                                                             
*                                                                               
MAIN16   CLI   ACTN,6          *** DISPLAY TWA LOG                              
         BNE   MAIN18                                                           
         BRAS  RE,TWADISP          DISPLAY TWAS                                 
         B     XMOD                                                             
*                                                                               
MAIN18   MVI   BYTE,X'FE'          MAKE SURE FE SCREEN IN                       
         BRAS  RE,LOADSCRN                                                      
*                                                                               
         CLI   ACTN,2          *** ADD?                                         
         BNE   MAIN20                                                           
         BRAS  RE,ADDDATA          ADD AN ENTRY SET AS MINE AND DISPLAY         
         BL    EXITL                                                            
         BRAS  RE,DISDATA                                                       
         B     XMOD                                                             
*                                                                               
MAIN20   OC    AMYTST,AMYTST       ENTRY MUST EXIST FOR DIS/CHA/DEL             
         BNZ   *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         CLI   ACTN,1          *** DISPLAY                                      
         BNE   MAIN22                                                           
         BRAS  RE,DISDATA          DISPLAY ENTRY                                
         B     XMOD                                                             
*                                                                               
MAIN22   CLI   ACTN,3          *** CHANGE                                       
         BNE   MAIN24                                                           
         BRAS  RE,SCHA             CHANGE AN ENTRY AND DISPLAY IT               
         BRAS  RE,DISDATA                                                       
         B     XMOD                                                             
*                                                                               
MAIN24   CLI   ACTN,4          *** DELETE                                       
         BNE   MAIN26                                                           
         BRAS  RE,SDEL             DELETE AN ENTRY                              
         B     XMOD                                                             
*                                                                               
MAIN26   CLI   ACTN,7          *** RESET                                        
         BNE   MAIN28                                                           
         ICM   R5,15,AMYTST                                                     
         BRAS  RE,RESET            RESET AN ENTRY AND DISPLAY IT                
         BRAS  RE,DISDATA                                                       
         B     XMOD                                                             
*                                                                               
MAIN28   DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST SCREEN                                                         *         
***********************************************************************         
         SPACE 1                                                                
LIST     NTR1  ,                                                                
         XC    SVDATA,SVDATA                                                    
         MVI   ACTN,1                                                           
*                                                                               
         CLI   PFKEY,4         *** PF4=TRACE                                    
         BE    GOTRACE                                                          
*                                                                               
         MVI   BYTE,X'FF'          MAKE SURE FF SCREEN IN                       
         BRAS  RE,LOADSCRN                                                      
*                                                                               
         BRAS  RE,REACT                                                         
         MVC   SRVP1(4),ALL                                                     
         MVC   ID,ALL                                                           
         MVC   SRVFTR,PFKLINE                                                   
*                                                                               
         BRAS  RE,LSCREEN          LIST ALL TEST ENTRYS                         
*                                                                               
         BRAS  RE,VALSUB           VALIDATE SUB ACTION FIELDS                   
         TM    FLAG,FLGSEL                                                      
         BZ    XMOD                                                             
*                                                                               
         TWAXC SRVLINEH,SRVXLINH,PROT=Y                                         
         B     MAIN14              IF SUB ACTIONS CLR SCRN AND PROCESS          
         EJECT                                                                  
***********************************************************************         
* GOBACKS                                                             *         
***********************************************************************         
         SPACE 1                                                                
GOTRACE  BRAS  RE,ON31                                                          
         L     R1,AMYUTL                                                        
         ICM   RF,15,TBUFF-UTLD(R1)                                             
         MVI   0(RF),9                   SET LEN                                
         MVC   1(2,RF),SRVIDH+(FHAD-FHD) SET ADDR                               
         MVC   3(6,RF),=C'=TRACE'        SET =TRACE                             
         MVI   9(RF),0                                                          
         B     GOEXIT                                                           
*                                                                               
GOTFM    GOTO1 AHEXOUT,DMCB,SVTOPDA,DUB,4                                       
         L     R1,AMYUTL                                                        
         ICM   R6,15,TBUFF-UTLD(R1)                                             
         MVI   0(R6),7             SET LEN                                      
         MVC   1(2,R6),SRVIDH+2    SET ADDR                                     
         MVC   3(4,R6),=C'=TFM'    SET =TFM                                     
         MVI   7(R6),10                                                         
         MVC   8(2,R6),SRVP1H+2    SET ADDR                                     
         MVC   10(7,R6),=C'TSTRCVR'                                             
         MVI   17(R6),13                                                        
         MVC   18(2,R6),SRVP2H+2    SET ADDR                                    
         MVC   20(2,R6),=C'A,'                                                  
         MVC   22(8,R6),DUB                                                     
         MVI   30(R6),0                                                         
*                                                                               
GOEXIT   MVC   SRVID,=C'=GOBACK '                                               
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* ADD A NEW ENTRY                                                     *         
* NTRY: AMYTST = A(MY TST ENTRY - IF ALREADY SET)                     *         
* NTRY: AFREE  = A(FREE ENTRY - IF SPACE)                             *         
***********************************************************************         
         SPACE 1                                                                
ADDDATA  NTR1  ,                                                                
         ICM   RF,15,AMYTST        ADD USER ENTRY                               
         BZ    *+12                                                             
         MVI   FERN,03             DUPLICATE ENTRY IN TABLE                     
         B     EXITL                                                            
*                                                                               
         ICM   R5,15,AFREE         GET A(FREE ENTRY)                            
         BNZ   *+12                                                             
         MVI   FERN,4              NO FREE ENTRIES IN TSTTAB                    
         B     EXITL                                                            
*                                                                               
         ST    R5,AMYTST                                                        
         XC    AFREE,AFREE                                                      
         USING TSTTABD,R5                                                       
         BRAS  RE,GETXTNT          GET TRACK INFO                               
*                                                                               
         MVC   TSTACCS,ID                                                       
         MVC   TSTLOW,LTRK                                                      
         MVC   TSTHIGH,HTRK                                                     
         MVC   TSTLAST(2),LTRK                                                  
         XC    TSTTRCIO,TSTTRCIO   DEFAULT TRACE # =0                           
         XC    INTVL,INTVL         INIT INTERVAL TO 0                           
*                                                                               
         L     RF,AMYUTL                                                        
         USING UTLD,RF                                                          
         OI    TSTAT5,TST5IOTR     DEFAULT=IO TRACE ON                          
         MVC   TSTNUM,TNUM                                                      
         DROP  RF                                                               
*                                                                               
         BRAS  RE,VALDATA          VALIDATE ENTRY DATA                          
*                                                                               
         CLI   SRVIOIH+(FHIL-FHD),0                                             
         BNE   EXIT                                                             
         BRAS  RE,SETCNT           RESET TRACE BUFFER                           
         BRAS  RE,SETSCT           RESET SCRIPT TRACE BUFFER                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHANGE ENTRY AT R5                                                  *         
***********************************************************************         
         SPACE 1                                                                
SCHA     NTR1  ,                                                                
         ICM   R5,15,AMYTST                                                     
         USING TSTTABD,R5                                                       
         BRAS  RE,GETXTNT                                                       
         BRAS  RE,VALDATA          VALIDATE ENTRY DATA                          
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE ENTRY AT R5                                                  *         
***********************************************************************         
         SPACE 1                                                                
SDEL     NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         ICM   R5,15,AMYTST                                                     
         USING TSTTABD,R5                                                       
         L     RF,ATSTTAB          GET LENGTH OF ENTRY                          
         LH    RF,0(RF)                                                         
         BCTR  RF,0                                                             
         ICM   R0,15,TSTTRC                                                     
         ICM   R1,15,TSTSCT                                                     
         XC    0(0,R5),0(R5)                                                    
         EX    RF,*-6              CLEAR ENTRY                                  
         STCM  R0,15,TSTTRC                                                     
         STCM  R1,15,TSTSCT                                                     
         MVI   HDRN,06                                                          
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LIST ALL TSTTAB ENTRYS                                              *         
***********************************************************************         
         SPACE 1                                                                
LSCREEN  NTR1                                                                   
         MVC   SRVHDR0,HEADER0     DISPLAY TEST TAB LIST                        
         MVC   SRVHDR,HEADER1                                                   
         MVC   SRVHDR1,HEADER2                                                  
*                                                                               
         L     R5,ATSTTAB          SET UP BXLE                                  
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5,R6)                                                      
*                                                                               
         LA    R2,SRVSELH          LOOP ROUND TSTTAB                            
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         XR    RF,RF                                                            
*                                                                               
LSCR02   OC    0(2,R5),0(R5)                                                    
         BZ    LSCR04                                                           
         BRAS  RE,BUILDL           DISPLAY 1 LINE PER ENTRY                     
*                                                                               
         IC    RF,FHLN             NEXT SELECT LINE                             
         AR    R2,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         LA    R0,SRVFTRH          STOP OVERFLOW                                
         CR    R2,R0                                                            
         BNL   EXIT                                                             
*                                                                               
LSCR04   BXLE  R5,R6,LSCR02                                                     
         LA    R2,SRVSELH          LOOP ROUND TSTTAB                            
         ST    R2,FADRH                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF TSTTAB AT R4 R5=A(ENTRY)                            *         
* NTRY: R2     = A(LINE)                                              *         
*       R5     = A(ENTRY)                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTLINED,R2                                                      
         USING TSTTABD,R5                                                       
BUILDL   NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         BRAS  RE,GETUTL           FIND UTL FOR THS ENTRY                       
         USING UTLD,R6                                                          
*                                                                               
         BRAS  RE,GETXTNT                                                       
         EDIT  (B2,ENTRY),(2,TSTNO)                                             
         MVC   TSTTERM,TSYM                                                     
         MVC   TSTUSER,TSTACCS                                                  
*                                                                               
         MVC   TSTLOG,NO           UPDATE LOGS Y/N                              
         TM    TSTFLAGS,TSTFUPDX   TEST INHIBIT FLAG                            
         BNZ   *+10                                                             
         MVC   TSTLOG,YES                                                       
*                                                                               
         GOTO1 AHEXOUT,DMCB,TSTLOW,TSTLOWT,2                                    
         GOTO1 (RF),(R1),TSTHIGH,TSTHIGT,2                                      
         GOTO1 (RF),(R1),TSTLAST,TSTLAT,4                                       
         GOTO1 (RF),(R1),TSTCPTY,TSTCAPT,2                                      
*                                                                               
         MVC   TSTTRAC,NO          IO TRACE Y/N                                 
         TM    TSTAT5,TST5IOTR                                                  
         BZ    *+10                                                             
         MVC   TSTTRAC,YES                                                      
*                                                                               
         MVC   TSTSCR,NO           SCREEN TRACE Y/N                             
         TM    TSTFLAGS,TSTFSCRQ                                                
         BZ    *+10                                                             
         MVC   TSTSCR,YES                                                       
*                                                                               
         LA    RE,TSTPTCH1         RE=A(PATCH ENTRY)                            
         LA    RF,TSTPTCHX         RF=A(END OF PATCH ENTRY)                     
         XR    R1,R1                                                            
BLDL02   OC    0(3,RE),0(RE)       COUNT NUM OF PATCHES                         
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    RE,L'TSTPTCH1(RE)                                                
         CR    RE,RF                                                            
         BL    BLDL02                                                           
*                                                                               
         EDIT  (R1),(5,TSTPAT)                                                  
         LTR   R1,R1                                                            
         BNZ   *+10                                                             
         MVC   TSTPAT,NONE                                                      
         B     EXIT                                                             
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* FIND TSTTAB ENTRY FOR ID                                            *         
* EXIT: AMYTST = A(MY TSTTAB ENTRY) OR ZERO IF NOT SET                *         
*       AFREE  = A(FREE ENTRY) OR ZERO IF NOTHING FREE                *         
***********************************************************************         
         SPACE 1                                                                
FINDTAB  NTR1  ,                                                                
         XC    AFREE,AFREE         CLEAR A(FREE ENTRY)                          
         XC    AMYTST,AMYTST       CLEAR A(MY ENTRY)                            
         BRAS  RE,ON31                                                          
*                                                                               
         ICM   R2,15,ATSTTAB                                                    
         LH    RE,0(R2)                                                         
         ICM   RF,15,2(R2)                                                      
         LA    R2,6(RE,R2)         FIRST ENTRY RESERVED                         
         USING TSTTABD,R2                                                       
*                                                                               
FT02     OC    TSTNUM,TSTNUM       ENTRY FREE?                                  
         BNZ   FT04                                                             
         OC    AFREE,AFREE         DO WE HAVE A FREE ONE ALREADY                
         BNZ   FT06                                                             
         ST    R2,AFREE            NO SO SAVE THIS                              
         B     FT06                                                             
*                                                                               
FT04     CLC   TSTACCS,ID          IS IT FOR THIS USER                          
         BNE   FT06                YES EXIT R5=A(ENTRY)                         
         ST    R2,AMYTST                                                        
*                                                                               
FT06     OC    AFREE,AFREE         IF GOT A FREE ONE                            
         BZ    *+14                                                             
         OC    AMYTST,AMYTST       AND GOT MINE - THEN EXIT NOW                 
         BNZ   EXITOK                                                           
         BXLE  R2,RE,FT02          ELSE NEXT ENTRY                              
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY A TSTTAB ENTRY                                              *         
* NTRY: AMYTST  = A(TSTTAB ENTRY TO DISPLAY)                          *         
***********************************************************************         
         SPACE 1                                                                
DISDATA  NTR1  ,                                                                
         MVC   SRVHDR2,HEADER3                                                  
         MVC   SRVHDR3,HEADER4                                                  
*                                                                               
         L     R5,AMYTST                                                        
         USING TSTTABD,R5                                                       
         BRAS  RE,ON31                                                          
         BRAS  RE,GETUTL           LOCATE UTL                                   
         USING UTLD,R6                                                          
*                                                                               
         MVC   SRVP1(4),ID         DISPLAY ID                                   
*                                                                               
         MVC   SRVUPD,NO           LOG UPDATES Y/N                              
         TM    TSTFLAGS,TSTFUPDX                                                
         BNZ   *+10                                                             
         MVC   SRVUPD,YES                                                       
*                                                                               
         MVC   SRVIOS,NO           LOG IOS Y/N                                  
         TM    TSTAT5,TST5IOTR                                                  
         BZ    *+10                                                             
         MVC   SRVIOS,YES                                                       
*                                                                               
         OC    TSTTRCIO,TSTTRCIO   IO COUNT Y/N                                 
         BZ    DISP02                                                           
         EDIT  (B2,TSTTRCIO),(5,SRVIOS),ALIGN=LEFT                              
*                                                                               
DISP02   GOTO1 AHEXOUT,DMCB,TSTLAST,SRVLDA,4                                    
*                                                                               
         XC    INTVL,INTVL         SHOW INTERVAL                                
         BRAS  RE,GETINT                                                        
         EDIT  (2,INTVL+2),(5,SRVIOI),ALIGN=LEFT                                
*                                                                               
         MVC   SRVSCR,NO           TRACE SCREENS Y/N                            
         TM    TSTFLAGS,TSTFSCRQ                                                
         BZ    *+10                                                             
         MVC   SRVSCR,YES                                                       
*                                                                               
         MVC   SRVLIM(4),TSTTACCS     SHOW LIMIT ACCESS                         
*                                                                               
         LHI   R0,NUMPATS          SET MAX NUM OF PATCHES                       
*                                                                               
         LA    R2,SRVPAT1H         R4=A(SCR PATCH FIELD HDR)                    
         USING FHD,R2                                                           
         LA    R6,TSTPTCH1         R6=A(TAB PATCH FIELD)                        
*                                                                               
DISP04   XR    RF,RF               CLEAR SCREEN PATCH FIELD                     
         IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+1)                                                    
         MVC   FHDA(0),SPACES                                                   
         EX    RF,*-6                                                           
         OI    FHOI,FHOITR                                                      
*                                                                               
         OC    0(3,R6),0(R6)       IS PATCH DEFINED IN TABLE                    
         BZ    DISP06              NO                                           
*                                                                               
         GOTO1 AHEXOUT,DMCB,(R6),FHDA,3,0                                       
         MVC   FHDA+6(1),FHDA                                                   
         MVI   FHDA,C'T'                                                        
*                                                                               
         LA    R7,FHDA+6                                                        
         CLI   0(R7),C'0'                                                       
         BE    *+12                                                             
         NI    0(R7),X'CF'         SET TO LEVEL A,B, OR C                       
         LA    R7,1(R7)                                                         
*                                                                               
         MVI   0(R7),C'+'                                                       
         LA    R7,1(R7)                                                         
         GOTO1 (RF),(R1),3(R6),(R7),2                                           
         MVI   4(R7),C'='                                                       
         LA    R7,5(R7)                                                         
*                                                                               
         SR    R8,R8                                                            
         IC    R8,5(R6)                                                         
         GOTO1 (RF),(R1),6(R6),(R7),(R8)                                        
*                                                                               
DISP06   AHI   R6,L'TSTPTCH1       NEXT IN TSTTAB                               
         XR    RF,RF               NEXT ON SCREEN                               
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         BCT   R0,DISP04                                                        
         MVI   HDRN,1              NO SET TO TSTTAB ENTRY DISPLAYED             
         B     EXIT                                                             
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* FIND THE UTL ENTRY FOR THE TERMINAL IN TSTTAB - MUST BE IN XA MODE  *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
GETUTL   L     R6,AMYUTL           CHECK IT ISN'T MY LOCAL UTL FIRST            
         USING UTLD,R6                                                          
         CLC   TSTNUM,TNUM                                                      
         BER   RE                                                               
*                                                                               
         ICM   R6,15,AUTLTAB       INDEX IN AND CHANGE UTL IN XA LIST           
         XR    R1,R1                                                            
         ICM   R1,3,TSTNUM                                                      
         BCTR  R1,0                                                             
         MH    R1,0(R6)                                                         
         LA    R6,6(R1,R6)                                                      
         CLC   TSTNUM,TNUM         CONFIRM UTL FOUND                            
         BER   RE                                                               
         DC    H'0'                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TSTTAB ENTRY SCREEN                                        *         
* NTRY: AMYTST = A(TSTTAB ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
VALDATA  NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         ICM   R5,15,AMYTST                                                     
         USING TSTTABD,R5                                                       
         BRAS  RE,GETUTL                                                        
         USING UTLD,R6                                                          
*                                                                               
         LA    R2,SRVUPDH          LOG UPDATES = Y OR N                         
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BE    VDT02               ASSUME UNCHANGED IF NOT INPUT                
*                                                                               
         CLI   FHDA,C'N'                                                        
         BNE   *+12                                                             
         OI    TSTFLAGS,TSTFUPDX   SET INHIBIT FLAG                             
         B     VDT02                                                            
         CLI   FHDA,C'Y'                                                        
         BNE   *+12                                                             
         NI    TSTFLAGS,255-TSTFUPDX                                            
         B     VDT02                                                            
*                                                                               
         MVI   FERN,05                                                          
         B     EXITL                                                            
*                                                                               
VDT02    LA    R2,SRVLDAH          LAST REC = TTTT                              
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    VDT08               ASSUME UNCHANGED IF NOT INPUT                
         MVC   FULL,TSTLAST        SAVE OLD VALUE                               
         CLI   FHIL,8                                                           
         BE    VDT04                                                            
         MVI   FERN,06                                                          
         B     EXITL                                                            
*                                                                               
VDT04    GOTO1 AHEXIN,DMCB,FHDA,TSTLAST,8                                       
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+12                                                             
         MVI   FERN,07                                                          
         B     EXITL                                                            
*                                                                               
         CLC   TSTLAST(2),LTRK                                                  
         BL    VDT06                                                            
         CLC   TSTLAST(2),HTRK                                                  
         BH    VDT06                                                            
*                                                                               
         CLC   FULL,TSTLAST     CAN ONLY SET RECORD NUMBER TO ZERO              
         BE    VDT08                                                            
         CLI   TSTLAST+3,0                                                      
         BNE   VDT06                                                            
         CLI   TSTLAST+2,0                                                      
         BNE   VDT06                                                            
*                                                                               
VDT06    MVI   FERN,08             DISK ADDRESS IS OUT OF RANGE                 
         B     EXITL                                                            
*                                                                               
VDT08    BRAS  RE,GETINT           GET PREVIOUS INTERVAL                        
         CLI   ACTN,2              IF THIS IS AN ADD, INIT INVL=0               
         BNE   *+10                                                             
         XC    INTVL,INTVL         INTERVAL BETWN I/O'S SAVED                   
*                                                                               
         LA    R2,SRVIOSH          IO TRACE HEADER                              
         ST    R2,FADRH                                                         
         CLI   FHIL,0              ANY INPUT IN IO TRACE PARM?                  
         BE    VDT12               NO, SEE IF ANYTHING IN INTERVAL              
*                                                                               
         OI    TSTAT5,TST5IOTR     DEFAULT IS TRACE ON                          
         CLI   FHDA,C'Y'           YES?                                         
         BE    VDT12                                                            
         CLI   FHDA,C'N'           NO?                                          
         BNE   VDT10               SEE IF NUMERIC INPUT                         
*                                                                               
         NI    TSTAT5,255-TST5IOTR TURN OFF BIT                                 
         XC    INTVL,INTVL         CLEAR INTERVAL                               
         XC    TSTTRCIO,TSTTRCIO   DEFAULT TRACE # =0                           
         B     VDT12               GO DEAL W/NEXT INPUT PARM                    
*                                                                               
VDT10    TM    FHII,FHIINU         VALID NUMERIC?                               
         BO    *+12                NO, ERROR                                    
         MVI   FERN,09                                                          
         B     EXITL                                                            
*                                                                               
         CLI   FHIL,5              NO MORE THAN 5 DIGITS                        
         BNH   *+12                                                             
         MVI   FERN,10                                                          
         B     EXITL                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,VDTPCK                                                        
         CVB   R0,DUB              R0=VALUE INPUT                               
         C     R0,=A(32*1024-1)                                                 
         BNH   *+12                                                             
         MVI   FERN,10                                                          
         B     EXITL                                                            
*                                                                               
         STH   R0,TSTTRCIO         SAVE IN TRACE TABLE                          
         B     VDT12                                                            
*                                                                               
VDT12    LA    R2,SRVIOIH                                                       
         ST    R2,FADRH                                                         
         CLI   FHIL,0              ANY INPUT                                    
         BE    VDT14                                                            
*                                                                               
         TM    FHII,FHIINU         VALID NUMERIC?                               
         BO    *+12                                                             
         MVI   FERN,11                                                          
         B     EXITL                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,VDTPCK                                                        
         CVB   R0,DUB              R0=VALUE INPUT                               
         ST    R0,INTVL            SAVE INTERVAL                                
*                                                                               
VDT14    NI    TSTFLAGS,255-TSTFSCRQ                                            
         CLI   SRVSCR,C'Y'                                                      
         BNE   *+8                                                              
         OI    TSTFLAGS,TSTFSCRQ                                                
*                                                                               
         BRAS  RE,SETCNT           RESET COUNTER IN TRACE BUFFER                
         BRAS  RE,SETSCT           RESET SCRIPT TRACE BUFFER                    
*                                                                               
         LA    R2,SRVLIMH          ACCESS LIMIT = XXXX                          
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    VDT16               ASSUME UNCHANGED IF NOT INPUT                
         MVC   TSTTACCS,=CL4' '                                                 
         CR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         MVC   TSTTACCS(0),FHDA                                                 
         EX    RF,*-6                                                           
         DROP  R6                                                               
*                                                                               
         CHI   RF,4                MAX 4 CHRS                                   
         JNH   *+8                                                              
         LA    RF,4                                                             
*                                                                               
VDT16    LA    R2,SRVPAT1H         EACH PATCH IS TSPPOOL+DDDD=VVVVVVVV          
         ST    R2,FADRH                                                         
         LA    R6,TSTPTCH1                                                      
         LHI   R0,NUMPATS                                                       
*                                                                               
VDT18    XC    0(L'TSTPTCH1,R6),0(R6)       DELETE PATCH                        
         CLI   FHIL,0                                                           
         BE    VDT38                                                            
*                                                                               
VDT20    GOTO1 ASCANNER,DMCB,FHD,(2,WORK1),C',==+'                              
         CLI   4(R1),2                                                          
         BE    *+12                                                             
         MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
         LA    R7,WORK1                                                         
         USING SCANBLKD,R7                                                      
         CLI   SC1STLEN,6          PHASE NAME IS TSPPOO OR TSPPOOX              
         BL    VDT26                                                            
         CLI   SC1STLEN,7                                                       
         BH    VDT26                                                            
*                                                                               
VDT22    CLI   SC1STFLD,C'T'       MUST BE A T                                  
         BNE   VDT26                                                            
         MVC   SC1STFLD(1),SC1STFLD+6                                           
         CLI   SC1STFLD,C' '                                                    
         BNE   *+12                                                             
         MVI   SC1STFLD,C'0'       SET BLANK LEVEL AS A ZERO                    
         B     VDT24                                                            
*                                                                               
         CLI   SC1STFLD,C'A'       ELSE MUST BE LEVELS A-C                      
         BL    VDT26                                                            
         CLI   SC1STFLD,C'C'                                                    
         BH    VDT26                                                            
*                                                                               
VDT24    GOTO1 AHEXIN,DMCB,SC1STFLD,0(R6),6                                     
         OC    12(4,R1),12(R1)                                                  
         BNZ   VDT28                                                            
*                                                                               
VDT26    MVI   FERN,14             FORMAT ERROR                                 
         B     EXITL                                                            
*                                                                               
VDT28    MVC   DUB,ZEROS           DISPLACEMENT IS 1 THRU 4 HEX CHRS            
         CLI   SC2NDLEN,1                                                       
         BL    VDT30                                                            
         CLI   SC2NDLEN,4                                                       
         BH    VDT30                                                            
         XR    R1,R1               RIGHT JUSTIFY DISPLACEMENT                   
         IC    R1,SC2NDLEN                                                      
         LHI   RF,4                                                             
         SR    RF,R1                                                            
         LA    RF,DUB(RF)                                                       
         BCTR  R1,0                                                             
         MVC   0(0,RF),SC2NDFLD                                                 
         EX    R1,*-6                                                           
         GOTO1 AHEXIN,DMCB,DUB,3(R6),4                                          
         OC    12(4,R1),12(R1)                                                  
         BNZ   VDT32                                                            
*                                                                               
VDT30    MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
VDT32    AHI   R7,SCBLKLQ          PATCH VALUE IS 2 THRU 32 HEX CHRS            
         CLI   SC2NDLEN,0                                                       
         BNE   VDT34                                                            
         CLI   SC1STLEN,2                                                       
         BL    VDT34                                                            
         CLI   SC1STLEN,32                                                      
         BH    VDT34                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         GOTO1 AHEXIN,DMCB,SC1STFLD,6(R6),(RF)                                  
         OC    12(4,R1),12(R1)                                                  
         BNZ   VDT36                                                            
*                                                                               
VDT34    MVI   FERN,15             BAD PATCH VALUE                              
         B     EXITL                                                            
*                                                                               
VDT36    MVC   5(1,R6),15(R1)      SET L'PATCH                                  
*                                                                               
VDT38    XR    RF,RF               BUMP TO NEXT SCREEN PATCH FIELD              
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         ST    R2,FADRH                                                         
         AHI   R6,L'TSTPTCH1                                                    
         BCT   R0,VDT18                                                         
         B     EXIT                                                             
*                                                                               
VDTPCK   PACK  DUB,FHDA(0)         PACK TO CONVERT TO HEX                       
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUBACTION FIELDS                                           *         
***********************************************************************         
         SPACE 1                                                                
VALSUB   NTR1  ,                                                                
         MVI   FLAG,0                                                           
         L     R5,ATSTTAB          SET UP BXLE                                  
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5,R6)                                                      
*                                                                               
         LA    R2,SRVSELH          LOOP ROUND TSTTAB                            
         USING FHD,R2                                                           
         XR    R1,R1                                                            
*                                                                               
VALS02   ST    R2,FADRH                                                         
         CLI   FHDA,C'?'                                                        
         BNE   *+8                                                              
         ST    R2,AHELP                                                         
         OC    0(2,R5),0(R5)                                                    
         BZ    VALS06                                                           
         CLI   FHIL,0              1 LINE PER ENTRY                             
         BNE   VALS08              GO SEE WHAT THE INPUT IS                     
*                                                                               
VALS04   IC    R1,FHLN             GO TO NEXT SELECT FIELD                      
         AR    R2,R1                                                            
         IC    R1,FHLN                                                          
         AR    R2,R1                                                            
*                                                                               
VALS06   BXLE  R5,R6,VALS02                                                     
         LA    R2,SRVSELH                                                       
         ST    R2,FADRH                                                         
         B     EXIT                                                             
*                                                                               
VALS08   LA    RF,SELTAB                                                        
         IC    R1,FHIL                                                          
         MVI   FHIL,0              ONCE ONLY PLEASE                             
         BCTR  R1,0                                                             
VALS10   EX    R1,*+8                                                           
         BE    VALS12                                                           
         CLC   FHDA(0),0(RF)                                                    
*                                                                               
         AHI   RF,L'SELTAB                                                      
         CLI   0(RF),0                                                          
         BNE   VALS10                                                           
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
VALS12   MVC   FHDA(3),SPACES      CLEAR INPUT FIELD                            
         ICM   RF,15,4(RF)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     VALS04                                                           
         DROP  R2                                                               
*                                                                               
SELTAB   DS    0XL8                                                             
         DC    CL3'SEL',X'00',AL4(SELECT)                                       
         DC    CL3'CHA',X'00',AL4(CHANGE)                                       
         DC    CL3'DEL',X'80',AL4(DELETE)                                       
         DC    CL3'UP ',X'80',AL4(UPDT)                                         
         DC    CL3'UU ',X'00',AL4(UUPDT)                                        
         DC    CL3'IO ',X'80',AL4(TRACE)                                        
         DC    CL3'TWA',X'00',AL4(TWAS)                                         
         DC    CL3'TT ',X'00',AL4(TTWAS)                                        
         DC    CL3'RES',X'80',AL4(RESET)                                        
         DC    XL4'00'                                                          
         EJECT                                                                  
***********************************************************************         
* SELECT AN ENTRY                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
SELECT   NTR1  ,                                                                
         MVC   SRVP1(4),TSTACCS    SET UP FOR DISPLAY                           
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,1                                                           
         BRAS  RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY CHANGES                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
CHANGE   NTR1  ,                                                                
         MVC   SRVP1(4),TSTACCS    SET UP FOR CHANGE                            
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,3                                                           
         BRAS  RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE ENTRY                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
DELETE   NTR1  ,                                                                
         MVC   SRVP1(4),TSTACCS    SET UP FOR CHANGE                            
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,4                                                           
         BRAS  RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SWITCH TRACE FLAG                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
TRACE    NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         BRAS  RE,GETUTL                                                        
         USING UTLD,R6                                                          
         XI    TSTAT5,TST5IOTR     SWITCH TRACE FLAG                            
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SWITCH UPDATE FLAG                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
UPDT     XI    TSTFLAGS,TSTFUPDX   SWITCH UPD FLAG                              
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SWITCH TWA FLAG                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
TWAS     XI    TSTFLAGS,TSTFSCRQ   SWITCH TWA FLAG                              
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* RESET ENTRY                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
RESET    NTR1  ,                                                                
         XC    TSTCPTY,TSTCPTY     RESET TRACK                                  
         XC    TSTLAST,TSTLAST                                                  
         MVC   TSTLAST(2),TSTLOW                                                
         XC    TSTTRCIO,TSTTRCIO   DEFAULT TRACE # =0                           
         XC    INTVL,INTVL         INIT INTERVAL TO 0                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY UPDATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
UUPDT    NTR1  ,                                                                
         MVC   SRVP1(4),TSTACCS    SET UP FOR DISPLAY                           
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,5                                                           
         BRAS  RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TWAS                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
TTWAS    NTR1  ,                                                                
         MVC   SRVP1(4),TSTACCS    SET UP FOR DISPLAY                           
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,6                                                           
         BRAS  RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE P1 (TEST ID)                                               *         
***********************************************************************         
         SPACE 1                                                                
VALP1    NTR1  ,                                                                
         LA    R2,SRVP1H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   VP102                                                            
         MVC   ID,ALL                                                           
         MVC   FHDA(L'ALL),ALL                                                  
         B     EXITOK                                                           
*                                                                               
VP102    CLI   FHDA,C'?'                                                        
         BNE   VP104               MEANS ASKED FOR HELP                         
         ST    R2,AHELP                                                         
         B     EXITH                                                            
*                                                                               
VP104    MVC   ID,FHDA             TAKE ID FROM INPUT FIELD                     
*                                                                               
         LHI   R0,L'ID             MAKE SURE NO BLANKS IN THIS FIELD            
         LA    RF,ID+L'ID-1                                                     
VP106    CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R0,VP106                                                         
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE P2 (ACTION)                                                *         
***********************************************************************         
         SPACE 1                                                                
VALP2    NTR1  ,                                                                
         LA    R2,SRVP2H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
*                                                                               
         LA    RE,ACTNTBL          DEFAULT ACTION IS THE FIRST ONE              
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BZ    VP206                                                            
         BCTR  RF,0                                                             
*                                                                               
         CLI   FHDA,C'?'                                                        
         BNE   *+12                                                             
         ST    R2,AHELP                                                         
         B     EXITH                                                            
*                                                                               
VP204    CLI   0(RE),X'FF'                                                      
         BNE   *+12                                                             
         MVI   FERN,01                                                          
         B     EXITL                                                            
         EX    RF,VP2CLC           MATCH NAME                                   
         BE    VP206                                                            
         AHI   RE,L'ACTNTBL        TRY NEXT ENTRY                               
         B     VP204                                                            
*                                                                               
VP2CLC   CLC   FHDA(0),0(RE)       LOOK FOR A MATCH                             
*                                                                               
VP206    MVC   ACTN,14(RE)         SAVE ACTION VALUE                            
         MVC   FHDA(7),7(RE)       REDISPLAY IT NICELY                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD SCREEN BYTE=SCREEN                                             *         
* NTRY: BYTE   = SCREEN NUMBER                                        *         
***********************************************************************         
         SPACE 1                                                                
LOADSCRN NTR1  ,                                                                
         L     RF,AMYUTL           CHECK FOR CURRENT                            
         XR    R1,R1                                                            
         IC    R1,TSVCREQ-UTLD(RF)                                              
         SRL   R1,4                                                             
         STC   R1,BYTE1                                                         
         OI    BYTE1,X'F0'                                                      
         CLC   BYTE1,BYTE          EXIT IF SAME                                 
         BE    EXIT                                                             
*                                                                               
         MVI   DMCB+4,C'R'         LOAD NEW ROOT SCREEN                         
         MVC   DMCB+5(2),=X'0136'                                               
         MVC   DMCB+7(1),BYTE                                                   
         GOTO1 ACALLOV,DMCB,SCREEN,,0                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                BAD RETURN FROM CALLOV                       
         IC    R1,BYTE                                                          
         SLL   R1,4                                                             
         STC   R1,BYTE1            SET TSVCREQ TO NEW ROOT                      
*                                                                               
         L     R1,AMYUTL                                                        
         NI    TSVCREQ-UTLD(R1),X'0F'                                           
         OC    TSVCREQ-UTLD(1,R1),BYTE1                                         
*                                                                               
         MVC   SRVID(5),=C'=TEST'  RESET FIELDS                                 
         OI    SRVIDH+6,X'80'                                                   
         MVC   SRVP1(4),ID                                                      
         OI    SRVP1H+6,X'80'                                                   
*                                                                               
         BRAS  RE,REACT            RESTORE ACTION                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REDISPLAY THE ACTION FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
REACT    NTR1  ,                                                                
         LA    RF,ACTNTBL                                                       
REACT02  CLI   0(RF),X'FF'                                                      
         BE    EXITOK                                                           
         CLC   ACTN,14(RF)                                                      
         BE    *+12                                                             
         AHI   RF,L'ACTNTBL                                                     
         B     REACT02                                                          
         MVC   SRVP2(7),7(RF)                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY TWA LOG FROM TSTRCVR                                        *         
***********************************************************************         
         SPACE 1                                                                
TWADISP  NTR1  ,                                                                
         L     R5,AMYTST                                                        
         USING TSTTABD,R5                                                       
*                                                                               
TWAD10   MVC   DA(2),TSTLOW        START FROM LOW+0100                          
         MVC   DA+2(2),=X'0100'                                                 
         MVI   READ,C'T'                                                        
*                                                                               
         CLI   PFKEY,8             WAS DOWN PFKEY PRESSED                       
         BNE   TWAD20                                                           
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
         MVI   FIRST,C'Y'          READ THIS TWA                                
         BRAS  RE,RDRCV                                                         
         BE    TWAD20                                                           
         MVI   HDRN,7                                                           
         B     EXITL                                                            
*                                                                               
TWAD20   CLI   PFKEY,7             WAS UP PFKEY PRESSED                         
         BNE   TWAD50                                                           
*                                                                               
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
TWAD21   OI    FLAG,FLGBACK                                                     
         MVI   FIRST,C'Y'          BACK ONE RECORD                              
         CLI   DA+2,1                                                           
         BE    TWAD22              BACK ONE TRACK                               
*                                                                               
TWAD21A  IC    R1,DA+2             JUST SUB ONE FROM RECORD                     
         BCTR  R1,0                                                             
         STC   R1,DA+2                                                          
         B     TWAD30                                                           
*                                                                               
TWAD22   CLC   DA(2),TSTLOW        ARE WE GOING OFF THE TOP                     
         BE    TWAD50                                                           
*                                                                               
         GOTO1 ADATAMGR,DMCB,DADDS,DABACK,IOAREA,0,ADTF,DA                      
         MVI   FIRST,C'Y'                                                       
         B     TWAD21A             BACK ONE TRACK                               
*                                                                               
TWAD30   BRAS  RE,RDRCV                                                         
         BNE   TWAD21                                                           
         NI    FLAG,255-FLGBACK                                                 
         B     TWAD55                                                           
*                                                                               
TWAD50   MVI   READ,C'T'           READ FIRST/NEXT TWA                          
         BRAS  RE,RDRCV                                                         
         BE    *+12                                                             
         MVI   HDRN,7                                                           
         B     EXITL                                                            
*                                                                               
TWAD55   GOTO1 AHEXOUT,DMCB,DA,FOOTER+28,4                                      
         LA    RE,IOAREA+64                                                     
         LA    R0,SRVMSGH                                                       
         LH    R1,TSTLEN                                                        
         CH    R1,=H'4096'                                                      
         BL    *+6                                                              
         DC    H'0'                MAX TWA LEN 4K                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LR    R1,R0                                                            
         XC    0(3,R1),0(R1)                                                    
         MVC   SRVID(17),=CL17'=TEST'  RESET ID FIELD                           
         MVI   SRVIDH+5,5                                                       
         OI    SRVIDH+6,X'81'                                                   
         LA    R1,SRVIDH                                                        
*NOP     ST    R1,FADRH                                                         
*                                                                               
         LA    RE,SRVMSGH                                                       
TWAD60   OI    6(RE),X'80'         SET ALL THE XMIT BITS ON                     
         CLC   2(2,RE),FOOTER+2                                                 
         BNL   TWADX                                                            
         SR    R0,R0                                                            
         ICM   R0,1,0(RE)                                                       
         BZ    TWADX                                                            
         AR    RE,R0                                                            
         B     TWAD60                                                           
*                                                                               
         B     TWADX                                                            
*                                                                               
TWAD90   MVI   PFKEY,0                                                          
         B     TWAD10                                                           
*                                                                               
TWADX    MVC   0(87,RE),FOOTER                                                  
         XC    87(3,RE),87(RE)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY UPDATE LOG FROM TSTRCVR                                     *         
***********************************************************************         
         SPACE 1                                                                
DUPD     NTR1                                                                   
         L     R5,AMYTST                                                        
         USING TSTTABD,R5                                                       
DUPD10   MVC   DA(2),TSTLOW        START FROM LOW+0100                          
         MVC   DA+2(2),=X'0100'                                                 
         OC    SVRECDA,SVRECDA     TEST FIRST TIME                              
         BZ    DUPD40                                                           
         OC    SVTOPDA,SVTOPDA                                                  
         BNZ   *+10                                                             
         MVC   SVTOPDA,DA                                                       
*                                                                               
         CLI   PFKEY,4             RF4=TFM                                      
         BE    GOTFM                                                            
*                                                                               
         CLI   PFKEY,0             WAS ENTER PRESSED                            
         BNE   DUPD15                                                           
         XC    SVRECDS,SVRECDS     RESET TO ZERO DISP                           
         MVC   SVRECDA,SVTOPDA                                                  
         MVC   DA,SVTOPDA          TOP RECORD                                   
*                                                                               
DUPD15   CLI   PFKEY,8             WAS DOWN PFKEY PRESSED                       
         BNE   *+10                                                             
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
*                                                                               
         CLI   PFKEY,7             WAS UP PFKEY PRESSED                         
         BNE   DUPD40                                                           
*                                                                               
         OI    FLAG,FLGBACK                                                     
         MVC   SVRECDA,SVTOPDA     RESTORE TOP DISP                             
         MVC   SVRECDS,SVTOPDS                                                  
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
*                                                                               
         LA    R2,19               19 SCREEN LINES                              
DUPD20   SR    R1,R1                                                            
         ICM   R1,3,SVRECDS        TRACK BACKWARDS                              
         BZ    DUPD21                                                           
         SH    R1,=H'20'           20 BYTES PER LINE                            
         BM    DUPD21                                                           
         STH   R1,SVRECDS                                                       
         BCT   R2,DUPD20           NEXT LINE                                    
         B     DUPD40                                                           
*                                                                               
DUPD21   MVI   FIRST,C'Y'          BACK ONE RECORD                              
         CLI   DA+2,1                                                           
         BE    DUPD22              BACK ONE TRACK                               
*                                                                               
DUPD21A  IC    R1,DA+2             JUST SUB ONE FROM RECORD                     
         BCTR  R1,0                                                             
         STC   R1,DA+2                                                          
         B     DUPD23                                                           
*                                                                               
DUPD22   CLC   DA(2),TSTLOW        ARE WE GOING OFF THE TOP                     
         BNE   DUPD22A                                                          
         MVI   PFKEY,0             RESET TO START                               
         NI    FLAG,255-FLGBACK                                                 
         B     DUPD10                                                           
*                                                                               
DUPD22A  GOTO1 ADATAMGR,DMCB,DADDS,DABACK,IOAREA,0,ADTF,DA                      
         MVI   FIRST,C'Y'                                                       
         B     DUPD21A             BACK ONE TRACK                               
*                                                                               
DUPD23   BRAS  RE,RDRCV            READ THE RECORD                              
         BNE   DUPD21                                                           
         SR    R0,R0                                                            
         NI    FLAG,255-FLGBACK                                                 
         LH    R1,TSTLEN                                                        
         LA    R1,20(R1)           ADD 20 FOR BLANK LINE                        
         D     R0,=F'20'           CALCULATE END DISP                           
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)            ADD ONE IF ANY REMAINDER                     
         MH    R1,=H'20'                                                        
         STH   R1,SVRECDS          SAVE END DISP                                
         B     DUPD20                                                           
*                                                                               
DUPD40   MVC   SVTOPDA,SVRECDA     SAVE NEW TOP OF SCREEN                       
         MVC   SVTOPDS,SVRECDS                                                  
         LA    R4,SRVLI1                                                        
         MVI   READ,C'R'           SET RECOVERY                                 
         MVI   FIRST,C'Y'          SET READ DA                                  
*                                                                               
DUPD50   BRAS  RE,RDRCV            READ FIRST/NEXT RECORD                       
         BE    *+12                                                             
         MVI   HDRN,8                                                           
         B     EXITL                                                            
*                                                                               
         BRAS  RE,BLDREC           BUILD A SCREEN                               
         ICM   R4,15,FULL                                                       
         BNZ   DUPD50              IF MORE SCREEN LEFT GET NEXT                 
*                                                                               
DUPDX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ FIRST/NEXT TWA OR RECOVERY REC                                 *         
***********************************************************************         
         SPACE 1                                                                
RDRCV    NTR1  ,                                                                
*                                                                               
RDRCV1   LA    RF,DMRSEQ           SET READ TYPE                                
         CLI   FIRST,C'N'                                                       
         BE    *+12                                                             
         LA    RF,DMRDIR           IF FIRST=Y DMRDIR                            
         B     *+10                                                             
         XC    SVRECDS,SVRECDS     SET DISP TO 0                                
*                                                                               
         MVI   FIRST,C'N'                                                       
         OC    DA(2),DA                                                         
         BZ    EXITL                                                            
*                                                                               
         GOTO1 ADATAMGR,DMCB,(RF),TSTRCVR,DA,IOAREA,0                           
         TM    8(R1),X'80'                                                      
         BO    RDRCV9              EOF                                          
         TM    8(R1),X'10'                                                      
         BO    RDRCV9              NOT FOUND RETURN EOF                         
         CLI   8(R1),0                                                          
         BNE   RDRCV9                                                           
         MVC   TSTLEN,18(R1)       EXTRACT RECORD LEN                           
*                                                                               
         CLC   DA,TSTLAST          EOF                                          
         BH    RDRCV9                                                           
*                                                                               
         CLI   READ,C'T'           ARE WE READING TWAS                          
         BNE   RDRCV2                                                           
         CLI   IOAREA,0            NOT A TWA SO RSEQ                            
         BNE   RDRCVN                                                           
         MVC   SVRECDA,DA          SAVE DA                                      
         LH    R1,TSTLEN                                                        
         SH    R1,=H'64'           SUBTRACT HEADER LEN                          
         STH   R1,TSTLEN                                                        
         B     EXITOK                                                           
*                                                                               
RDRCV2   CLI   IOAREA,0            IS A TWA SO RSEQ                             
         BE    RDRCVN                                                           
         MVC   SVRECDA,DA          SAVE DA                                      
         LH    R1,TSTLEN                                                        
         SH    R1,=H'24'           SUBTRACT HEADER LEN                          
         STH   R1,TSTLEN                                                        
         B     EXITOK                                                           
*                                                                               
RDRCV9   B     EXITL                                                            
*                                                                               
RDRCVN   TM    FLAG,FLGBACK        READ NEXT UNLESS BACKWARDS                   
         BZ    RDRCV1                                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD RECOVERY RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
BLDREC   NTR1  ,                                                                
         L     R6,AFILTAB                                                       
         USING FILTABD,R6                                                       
         MVI   BYTE,1              SET FIRST LINE OF RECORD                     
         LA    R7,IOAREA                                                        
         SR    R1,R1                                                            
         IC    R1,0(R7)            LOCATE FILE NEAME IT TABLE                   
         SLL   R1,5                                                             
         AR    R6,R1                                                            
         MVC   0(7,R4),DMFLNAME    DISPLAY FILE NAME                            
*                                                                               
         CLI   1(R7),1             IS THIS COPY/CHANGE OR ADD                   
         BNE   *+10                                                             
         MVC   8(4,R4),=C'COPY'                                                 
         CLI   1(R7),2                                                          
         BNE   *+10                                                             
         MVC   8(6,R4),=C'CHANGE'                                               
         CLI   1(R7),3                                                          
         BNE   *+10                                                             
         MVC   8(3,R4),=C'ADD'                                                  
*                                                                               
         SR    R2,R2               R2=RECORD LEN + 20 FOR BLANK LINE            
         ICM   R2,3,TSTLEN                                                      
         LA    R2,20(R2)                                                        
*                                                                               
BLDR010  CLI   BYTE,2              SECOND LINE GETS DISK ADDR                   
         BNE   BLDR019                                                          
*                                                                               
         MVC   0(10,R4),=C'(........)'                                          
         GOTO1 AHEXOUT,DMCB,DA,1(R4),4                                          
*                                                                               
BLDR019  STH   R2,HALF             SAVE LENGTH IN HALF                          
         CH    R2,=H'20'                                                        
         BL    *+8                                                              
         LA    R2,20               IF REMAINING LEN > 20 DISP 20                
*                                                                               
         CLC   HALF,=H'40'         IF REMAINING LEN < 40 DISP HALF-20           
         BNL   BLDR020                                                          
         LH    R2,HALF                                                          
         SH    R2,=H'20'                                                        
         BNP   BLDR030             IF THIS IS < 1 ITS A BLANK LINE              
*                                                                               
BLDR020  GOTO1 AHEXOUT,DMCB,24(R7),16(R4),(R2)                                  
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12             SHOW CHARS AND TRANSLATE OUT FUNNYS          
         EX    R1,*+14                                                          
         B     *+16                                                             
         MVC   58(0,R4),24(R7)                                                  
         TR    58(0,R4),TTABLE                                                  
*                                                                               
         CLI   BYTE,3              THIRD + LINE GETS DISPLACEMENTS              
         BL    BLDR030                                                          
         EDIT  (B2,SVRECDS),(4,10(R4)),FILL=0                                   
*                                                                               
BLDR030  LA    R7,20(R7)           BUMP DOWN RECORD                             
         LR    R1,R7                                                            
         LA    R0,IOAREA                                                        
         SR    R1,R0               R1=CURRENT DISPLACEMENT                      
         CH    R1,SVRECDS                                                       
         BNH   BLDR050             SKIP THIS LINE IF NOT UP TO SVDISP           
         STCM  R1,3,SVRECDS                                                     
*                                                                               
BLDR040  LA    R4,78+8(R4)         NEXT LINE                                    
         LA    R1,SRVPFK                                                        
         CR    R4,R1               TEST END OF SCREEN                           
         BL    *+10                                                             
         SR    R4,R4               SET R4 TO ZERO IF END                        
         B     BLDRECX                                                          
*                                                                               
BLDR050  MVC   0(78,R4),SPACES     CLEAR NEXT LINE                              
         IC    R1,BYTE                                                          
         LA    R1,1(R1)            BUMP LINE IN REC COUNT                       
         STC   R1,BYTE                                                          
*                                                                               
         LH    R1,HALF             SUB 20 FROM REMAINING                        
         SH    R1,=H'20'                                                        
         LR    R2,R1                                                            
         BP    BLDR010             STILL MORE TO DISPLAY                        
*                                                                               
         DROP  R6                                                               
BLDRECX  ST    R4,FULL             EXIT R4 SHOWS MORE SCREEN AVAILABLE          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ IN SAVED STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
READSTR  NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R2),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),TSTID   TEST FOR MY ID                               
         BNE   READX                                                            
         LA    R1,SRCOMWRK                                                      
         MVC   SAVEDSTR(SAVEDL),4(R1)                                           
READX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE OUT SAVED STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
WRITESTR NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R1,SRCOMWRK                                                      
         MVC   0(4,R1),TSTID                                                    
         MVC   4(SAVEDL,R1),SAVEDSTR                                            
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 ADATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISERR   NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'E#'        SET ERROR                                  
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,FERN                                                          
         CHI   R0,255                                                           
         BNE   *+6                                                              
         XR    R0,R0                                                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         LTR   R0,R0               SPECIAL ERROR MESSAGE?                       
         BNE   DERR02              NO                                           
         XR    RF,RF                                                            
         ICM   RF,7,FERNA                                                       
         MVC   0(30,R4),0(RF)                                                   
         B     DERR04                                                           
*                                                                               
DERR02   AHI   R0,-1                                                            
         MHI   R0,EMSGL                                                         
         L     RF,AERRMSGS                                                      
         AR    RF,R0                                                            
         MVC   0(EMSGL,R4),0(RF)                                                
*                                                                               
DERR04   ICM   R2,15,FADRH         SET CURSOR ON BAD FIELD                      
         BZ    EXITOK                                                           
         L     R5,ATIOB                                                         
         USING TIOBD,R5                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY OK MESSAGE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOK    NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'I#'      SET OK                                       
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,HDRN                                                          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         AHI   R0,-1                                                            
         MHI   R0,IMSGL                                                         
         L     RF,AOKMSGS                                                       
         AR    RF,R0                                                            
         MVC   0(IMSGL,R4),0(RF)                                                
*                                                                               
         L     R5,ATIOB                                                         
         USING TIOBD,R5                                                         
         OI    TIOBINDS,TIOBSETC                                                
         ICM   RF,15,FADRH                                                      
         BNZ   *+8                                                              
         LA    RF,SRVP1H                                                        
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CALCULATE ENTRY/LTRK/HTRK                                           *         
* NTRY: R5     = A(THIS ENTRY)                                        *         
* EXIT: LTRK/HTRK/ENTRY ALL SET                                       *         
***********************************************************************         
         SPACE 1                                                                
GETXTNT  NTR1  ,                                                                
         L     RF,ATSTTAB                                                       
         LR    R0,R5                                                            
         SR    R0,RF                                                            
         SRDL  R0,32               R0/1=DISPLACEMENT                            
*                                                                               
         LH    RF,0(RF)            RF=WIDTH                                     
         DR    R0,RF                                                            
         STH   R1,ENTRY            R1=ENTRY NUMBER                              
*                                                                               
         L     RF,ATSTTAB                                                       
         MH    R1,6(RF)            MULT BY TRACKS PER CI                        
         AHI   R1,1                                                             
         STH   R1,LTRK             R1=LOW TRACK NUMBER                          
*                                                                               
         AH    R1,6(RF)                                                         
         BCTR  R1,0                                                             
         STH   R1,HTRK             R1=HIGH TRACK NUMBER                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SETCNT- INITIALISE COUNTER OF # RECORDS READ TO 0 IN TRACE BUFFER   *         
* NTRY: R5 = A(TSTTAB ENTRY)                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
SETCNT   NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         ICM   R6,15,TSTTRC          GET ADDRESS OF TRACE TABLE                 
         BZ    EXITOK                                                           
         XC    36(4,R6),36(R6)       ZERO THE TRACE COUNT                       
         MVC   28(4,R6),INTVL                                                   
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SETSCT - RESET VALUES IN SCRIPT TRACE BUFFER                        *         
* NTRY: R5 = A(TSTTAB ENTRY)                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
SETSCT   NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         ICM   R6,15,TSTSCT          GET ADDRESS OF SCRIPT TRACE TABLE          
         BZ    EXIT                                                             
         USING SCTTABD,R6                                                       
         XC    SCTCALL,SCTCALL                                                  
         XC    SCTCOUNT,SCTCOUNT                                                
         XC    SCTSIN,SCTSIN                                                    
         XC    SCTNEXT,SCTNEXT                                                  
         XC    SCTSERR,SCTSERR                                                  
         XC    SCTSDSP,SCTSDSP                                                  
         XC    SCTSOFF,SCTSOFF                                                  
         XC    SCTSOUTL,SCTSOUTL                                                
         XC    SCTSTA1,SCTSTA1                                                  
         XC    SCTRD,SCTRD                                                      
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GETINT- READ OFF INTERVAL FROM TRACE BUFFER                         *         
* NTRY: R5 = A(TSTTAB ENTRY)                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
GETINT   NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         ICM   RF,15,TSTTRC          GET ADDRESS OF TRACE TABLE                 
         MVC   INTVL,28(RF)          GET INTERVAL                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CALL GETHELP AND EXIT TO MONITOR                                    *         
***********************************************************************         
         SPACE 1                                                                
HELPOUT  L     R1,AHELP                                                         
         USING FHD,R1                                                           
         OI    FHOI,FHOICU          SET CURSOR                                  
         LR    R0,R1                                                            
         MVC   HELPKEY,HELPID                                                   
*                                                                               
         LA    RE,HELPTAB          FIND WHICH PANEL                             
         SR    RF,RF                                                            
         LA    RF,1                                                             
HELP010  EX    0,0(RE)             BY TESTING AHELP                             
         CR    R1,R0                                                            
         BE    HELP020                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),0                                                          
         BE    HELP020                                                          
         LA    RF,1(RF)                                                         
         B     HELP010                                                          
*                                                                               
HELP020  STC   RF,HELPNUM                                                       
         XC    DMCB(24),DMCB                                                    
         GOTO1 AGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0)                         
         DC    H'0'                                                             
HELPTAB  LA    R1,SRVP1H           POSSIBLE HELP FIELDS                         
         LA    R1,SRVP2H                                                        
         LA    R1,SRVP3H                                                        
         LA    R1,SRVP4H                                                        
         LA    R1,SRVSELH          <--- OR GREATER                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* HANDY ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XMOD     OC    AHELP,AHELP         USE HELPOUT IF AHELP NOT ZERO                
         BNZ   HELPOUT                                                          
         BRAS  RE,WRITESTR         WRITE SAVED STR                              
*                                                                               
         CLI   FERN,0                                                           
         BNE   *+12                                                             
         BRAS  RE,DISOK                                                         
         B     *+8                                                              
         BRAS  RE,DISERR                                                        
*                                                                               
         L     RD,SAVERD           RESET BASE RD                                
         B     EXIT                AND EXIT                                     
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
IMSGL    EQU   45                                                               
EMSGL    EQU   45                                                               
DABACK   EQU   9                                                                
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DTFADD   DC    CL8'DTFADD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
TSTRCVR  DC    CL8'TSTRCVR'                                                     
DADDS    DC    CL8'DADDS'                                                       
SPACES   DC    80C' '                                                           
ZEROS    DC    8C'0'                                                            
EFFS     DC    16X'FF'                                                          
*                                                                               
ACTNTBL  DS    0CL16               ACTION TABLE                                 
         DC    C'DISPLAYDisplay',X'0100'                                        
         DC    C'ADD    Add    ',X'0200'                                        
         DC    C'CHANGE Change ',X'0300'                                        
         DC    C'DELETE Delete ',X'0400'                                        
         DC    C'UPDATESUpdates',X'0500'                                        
         DC    C'TWAS   Twas   ',X'0600'                                        
         DC    C'RESET  Reset  ',X'0700'                                        
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*              C'....+....1....+....2....+....3....+....4'                      
*              C'....+....5....+....6....+....7....+....8'                      
*                                                                               
HEADER0  DC    C'                     - RCVR Disk Addr - '                      
         DC    C'Trk  --- Traces ---                     '                      
HEADER1  DC    C'Sel No Terminal User Low  High Last     '                      
         DC    C'Left Updt I/Os Twas Patch               '                      
HEADER2  DC    C'--- -- -------- ---- ---- ---- -------- '                      
         DC    C'---- ---- ---- ---- -----               '                      
HEADER3  DC    C'PATCHES  PHASE+OFFS=DATA                '                      
         DC    C'                                        '                      
HEADER4  DC    C'----------------------------------------'                      
         DC    C'--------                                '                      
PFKLINE  DC    C'PF1=Help PF3=Return PF4=Trace PF7=Up PF8'                      
         DC    C'=Down PF12=Exit                         '                      
PFKLIN2  DC    C'PF1=Help PF3=Return PF4=TFM PF7=Up PF8=D'                      
         DC    C'own Enter=Align PF12=Exit               '                      
*                                                                               
TSTID    DC    C'$TST'                                                          
ALL      DC    CL4'ALL '                                                        
YES      DC    C'Yes  '                                                         
NO       DC    C'No   '                                                         
NONE     DC    C'None '                                                         
*                                                                               
NUMPATS  EQU   5                   NUMBER OF PATCHES IN TSTTAB ENTRY            
*                                                                               
HELPID   DC    XL10'0136FF00010000000000'                                       
*                                                                               
FOOTER   DC    X'5728073000008000'                                              
         DC    C'* Screen trace. DA=(00010100) PF8 Next S'                      
         DC    C'creen / PF7=Previous / Enter to return '                       
*                                                                               
*        DC    C'ACT',X'FF',AL4(ROUTINE)                                        
*                                                                               
TTABLE   DC    C'................................'                              
         DC    C'................................'                              
         DC    X'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'                              
         DC    X'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'                              
         DC    X'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'                              
         DC    X'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F'                              
         DC    C'.abcdefghi.......jklmnopqr......'                              
         DC    C'..stuvwxyz......................'                              
         DC    C'{ABCDEFGHI......}JKLMNOPQR......'                              
         DC    C'\.STUVWXYZ......0123456789|.....'                              
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'Invalid Action                               '         
ERR02    DC    CL(EMSGL)'Requested entry not in table                 '         
ERR03    DC    CL(EMSGL)'Duplicate entry in table                     '         
ERR04    DC    CL(EMSGL)'TSTTAB is full - cannot add any more entries '         
ERR05    DC    CL(EMSGL)'Invalid input field - should be Y or N only  '         
ERR06    DC    CL(EMSGL)'Invalid D/A - 8 Hex digits only valid input  '         
ERR07    DC    CL(EMSGL)'Value input in needs to be valid Hex input   '         
ERR08    DC    CL(EMSGL)'D/A input is out of Low/High address range   '         
ERR09    DC    CL(EMSGL)'Input must be Y, N or a valid number         '         
ERR10    DC    CL(EMSGL)'Number input is out of range - 32K or less   '         
ERR11    DC    CL(EMSGL)'Input must be valid numeric                  '         
ERR12    DC    CL(EMSGL)'Invalid sub-action choice                    '         
ERR13    DC    CL(EMSGL)'Invalid Format - use TSSPPOOL+DISP=PPPPPPPP  '         
ERR14    DC    CL(EMSGL)'Bad Phase - use TSSPPOO(L)                   '         
ERR15    DC    CL(EMSGL)'Bad Displacement: 1-4 Hex digits only        '         
ERR16    DC    CL(EMSGL)'Bad Patch Value: 2-32 Hex digits only        '         
         EJECT                                                                  
***********************************************************************         
* OK MESSAGES                                                         *         
***********************************************************************         
         SPACE 1                                                                
OKMSGS   DS    0CL(IMSGL)                                                       
OK01     DC    CL(IMSGL)'TSTTAB entries displayed                     '         
OK02     DC    CL(IMSGL)'I/O count status set on                      '         
OK03     DC    CL(IMSGL)'I/O count status set off                     '         
OK04     DC    CL(IMSGL)'Trace User Profile option set on             '         
OK05     DC    CL(IMSGL)'Trace User Profile option set off            '         
OK06     DC    CL(IMSGL)'TSTTAB entry deleted as requested            '         
OK07     DC    CL(IMSGL)'End of TWA log                               '         
OK08     DC    CL(IMSGL)'End of update log                            '         
OK09     DC    CL(IMSGL)'Script trace status set on                   '         
OK10     DC    CL(IMSGL)'Script trace status set off                  '         
OK11     DC    CL(IMSGL)'CPU trace status set on                      '         
OK12     DC    CL(IMSGL)'CPU trace status set off                     '         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
SYSNAME  DS    F                                                                
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
SAVER1   DS    A                                                                
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
*                                                                               
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AMYUTL   DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
FULL     DS    F                                                                
FULL1    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
ASAVE    DS    A                                                                
ADTF     DS    A                                                                
AUTLTAB  DS    A                                                                
AHEXOUT  DS    A                                                                
AHEXIN   DS    A                                                                
ASCANNER DS    A                                                                
ACALLOV  DS    A                                                                
ADATAMGR DS    A                                                                
ASSB     DS    A                                                                
ATSTTAB  DS    A                                                                
AGETTXT  DS    A                                                                
AGETHELP DS    A                                                                
AHELP    DS    A                                                                
*                                                                               
SAVETST  DS    A                                                                
IDADDR   DS    A                                                                
AVADDR   DS    A                                                                
*                                                                               
IDCOUNT  DS    PL2                                                              
AVCOUNT  DS    PL2                                                              
*                                                                               
AFREE    DS    A                                                                
AMYTST   DS    A                                                                
LTRK     DS    H                                                                
HTRK     DS    H                                                                
ENTRY    DS    H                                                                
*                                                                               
DA       DS    F                                                                
TSTLEN   DS    H                                                                
FIRST    DS    X                                                                
READ     DS    X                                                                
*                                                                               
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
AERRMSGS DS    A                                                                
AOKMSGS  DS    A                                                                
AFILTAB  DS    A                                                                
*                                                                               
PFKEY    DS    X                                                                
INTVL    DS    F                                                                
ID       DS    CL4                                                              
IDSTART  DS    H                                                                
RECLEN   DS    H                                                                
TRM      DS    H                                                                
ACTN     DS    C                                                                
FLAG     DS    C                                                                
FLGSEL   EQU   X'80'                                                            
FLGDIS   EQU   X'40'                                                            
FLGBACK  EQU   X'20'                                                            
MSG      DS    CL60                                                             
WORK     DS    CL200                                                            
WORK1    DS    CL200                                                            
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
SAVEDSTR EQU   *                   SAVED PART OF W/S                            
SVDATA   DS    0CL12                                                            
SVRECDA  DS    F                   SAVED DA FOR UPDATES                         
SVTOPDA  DS    F                   SAVED DA FOR UPDATES                         
SVRECDS  DS    H                   DISPLACEMENT INTO REC                        
SVTOPDS  DS    H                   DISPLACEMENT INTO REC                        
*                                                                               
SVACTN   DS    X                   SAVED ACTION                                 
SVTSTID  DS    CL4                 SAVED TEST ID                                
*                                                                               
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
IOAREA   DS    6096C                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* TSTTAB LINE DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
TSTLINED DSECT                                                                  
TSTSELH  DS    XL8                                                              
TSTSEL   DS    CL3                                                              
TSTLINH  DS    XL8                                                              
TSTLIN   DS    0CL75                                                            
TSTNO    DS    CL2                                                              
         DS    CL1                                                              
TSTTERM  DS    CL8                                                              
         DS    CL1                                                              
TSTUSER  DS    CL4                                                              
         DS    CL1                                                              
TSTLOWT  DS    CL4                                                              
         DS    CL1                                                              
TSTHIGT  DS    CL4                                                              
         DS    CL1                                                              
TSTLAT   DS    CL8                                                              
         DS    CL1                                                              
TSTCAPT  DS    CL4                                                              
         DS    CL1                                                              
TSTLOG   DS    CL4                                                              
         DS    CL1                                                              
TSTTRAC  DS    CL4                                                              
         DS    CL1                                                              
TSTSCR   DS    CL4                                                              
         DS    CL1                                                              
TSTPAT   DS    CL5                                                              
         DS    CL1                                                              
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* DMFILTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
***********************************************************************         
* SCREEN DSECTS                                                       *         
***********************************************************************         
         SPACE 1                                                                
SRTSTFFD DSECT                                                                  
         DS    CL64                                                             
SCREEN   EQU   *                                                                
* SRTSTFFD                                                                      
       ++INCLUDE SRTSTFFD                                                       
         ORG   SCREEN                                                           
* SRTSTFED                                                                      
       ++INCLUDE SRTSTFED                                                       
         ORG   SCREEN                                                           
* SRTSTFDD                                                                      
       ++INCLUDE SRTSTFDD                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SRTST00   03/02/18'                                      
         END                                                                    
