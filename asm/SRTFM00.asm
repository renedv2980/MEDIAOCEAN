*          DATA SET SRTFM00    AT LEVEL 004 AS OF 11/08/10                      
*PHASE T13700A                                                                  
*INCLUDE DECODE                                                                 
         TITLE 'TFM00 - PFM FOR THE SERVICE SYSTEM'                             
         PRINT NOGEN                                                            
TFM00    CSECT                                                                  
         NMODL WORKL,**TFM0**,RA,RR=RE,CLEAR=YES                                
         USING WORKD,RC                                                         
         MVC   IPARMS,0(R1)                                                     
         ST    RE,RELO                                                          
         L     R9,ALITS                                                         
         AR    R9,RE                                                            
         USING LITERALS,R9                                                      
*                                                                               
         L     R8,ATWA                                                          
         USING TWAD,R8             R8=A(TWA)                                    
         BRAS  RE,INIT             DO INITIALISATION                            
         BRAS  RE,READTWA          NOW GO GET SAVE AREA                         
*                                                                               
         L     R7,SAVEAREA                                                      
         USING TFMSAVED,R7                                                      
         BRAS  RE,TOGGLE                                                        
*                                                                               
TF       USING FILTABD,TFILTAB                                                  
TP       USING PERMTABD,TPERMTAB                                                
*                                                                               
         BRAS  RE,VFILE            VALIDATE FILE NAME                           
         BNE   OERR                                                             
         BRAS  RE,VRID             VALIDATE RECORD ID FIELD                     
         BNE   OERR                                                             
         BRAS  RE,VRACT            VALIDATE RECORD ACTION FIELD                 
         BNE   OERR                                                             
*                                                                               
         BRAS  RE,VEID             VALIDATE ELEMENT ID FIELD                    
         BNE   OERR                                                             
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'FE',TFMLOADH),0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                WHERE DISPLAY SCREEN?                        
*                                                                               
         MVC   VWDISP,WDISPH       HEX DISPLAY VALUES                           
         MVC   TFEHH,HEXHDRH                                                    
         MVC   TFEDH,DECHDRH                                                    
         CLI   DTYPE,DTHEX                                                      
         BE    MAIN02                                                           
*                                                                               
         MVC   TFEHH,HEXHDR        DECIMAL DISPLAY VALUES                       
         MVC   TFEDH,DECHDR                                                     
         MVC   VWDISP,WDISP                                                     
*                                                                               
MAIN02   BRAS  RE,RECREAD          NOW GO GET THE RECORD                        
         BL    OERR                                                             
*                                                                               
MAIN04   BRAS  RE,SCROLL           VALIDATE ANY SCROLL                          
         BNE   OERR                                                             
         BRAS  RE,NXTREC                                                        
         BNE   OERR                                                             
         BRAS  RE,RECDIS           DISPLAY RECORD                               
         BNE   OERR                                                             
*                                                                               
MAIN06   XC    FADRH,FADRH                                                      
         MVI   FERRDSP,0                                                        
         B     OOK                                                              
*                                                                               
OERR     CLI   FERN,0                                                           
         BE    OOK                                                              
         BRAS  RE,DISERR           SET ERROR MESSAGE                            
         B     EXIT                                                             
*                                                                               
OOK      BRAS  RE,DISOK            SET OK HEADER MESSAGE                        
         BRAS  RE,PFKFIX           REDO PFKEY LINE                              
         BRAS  RE,WRTTWA           WRITE OFF TWA 11                             
         B     EXIT                                                             
*                                                                               
ALITS    DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* MAIN INITIALISATION ROUTINE                                         *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     R0,RELO             RELOCATE VARIABLES                           
         L     RF,=A(FILTAB)                                                    
         AR    RF,R0                                                            
         ST    RF,AFILTAB                                                       
         L     RF,=A(RCVTAB)                                                    
         AR    RF,R0                                                            
         ST    RF,ARCVTAB                                                       
         L     RF,=V(DECODE)                                                    
         AR    RF,R0                                                            
         ST    RF,ADECODE                                                       
*                                                                               
         MVC   VNDISP,NDISP                                                     
         L     RF,=A(DISPTBLL)     LOWER CASE                                   
         AR    RF,R0                                                            
         ST    RF,ADISPTBL                                                      
*                                                                               
         L     RF,ACOMFAC                                                       
         USING COMFACSD,RF                                                      
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   AUNSCAN,CUNSCAN                                                  
         DROP  RF                                                               
*                                                                               
         L     RF,ASYSFAC          EXTRACT SSB TEMPSTR/TEMPEST INFO             
         USING SYSFACD,RF                                                       
         MVC   ATSTTAB,VTSTTAB                                                  
         MVC   ASSB,VSSB                                                        
         DROP  RF                                                               
*                                                                               
         L     RF,ASSB                                                          
         USING SSBD,RF                                                          
*                                                                               
         MVC   SYSNAME,SSBSYSN4    SYSTEM NAME                                  
         LH    R0,SSBSSMAX                                                      
         MH    R0,SSBSSPGS                                                      
         AHI   R0,2                                                             
         STH   R0,TMPSTRMX         SET MAX PAGES FOR TEMPSTR                    
*                                                                               
         MVC   TWAL,SSBTWAL        GET TWA AND TEMPEST LENGTHS                  
         MVC   TMSL,SSBTMSL                                                     
*        MVC   TWCHKD,=Y(CHKPTDSP) 18K TEMPSTR RECORDS                          
         MVC   TWGLBD,=Y(CHKPTGLD)                                              
         CLC   TWAL,=H'14336'                                                   
         BNE   INIT02                                                           
         MVC   TWCHKD,=H'12800'    14K TEMPSTR RECORDS                          
         MVC   TWGLBD,=H'12800'                                                 
         DROP  RF                                                               
*                                                                               
INIT02   CLC   TFMSR+1(4),=C'TFM,' TEST $TFM,00 THRU $TFM,0B FOR TWA            
         BNE   INIT08                                                           
         CLC   =C'ME',TFMSR+5                                                   
         BE    INIT06                                                           
*                                                                               
         CLI   TFMSR+5,C'0'                                                     
         BNE   INIT08                                                           
         CLI   TFMSR+6,C'A'                                                     
         BL    INIT08                                                           
         CLI   TFMSR+6,C'B'                                                     
         BNH   INIT04                                                           
         CLI   TFMSR+6,C'0'                                                     
         BL    INIT08                                                           
         CLI   TFMSR+6,C'9'                                                     
         BH    INIT08                                                           
*                                                                               
INIT04   MVI   TFMFILEH+5,7                                                     
         MVC   TFMFILE,SPACES                                                   
         MVC   TFMFILE(7),=C'TEMPSTR'                                           
         MVI   TFMRIDH+5,4                                                      
         MVC   TFMRID,SPACES                                                    
         MVC   TFMRID(2),=C'K,'                                                 
         MVC   TFMRID+2(2),TFMSR+5                                              
         XC    TFMSR+4(3),TFMSR+4                                               
         B     INIT08                                                           
*                                                                               
INIT06   MVI   TFMFILEH+5,7                                                     
         MVC   TFMFILE,SPACES                                                   
         MVC   TFMFILE(7),=C'TSTRCVR'                                           
         MVI   TFMRIDH+5,2                                                      
         MVC   TFMRID,SPACES                                                    
         MVC   TFMRID(2),=C'ME'                                                 
         XC    TFMSR+4(3),TFMSR+4                                               
*                                                                               
INIT08   L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         XR    R1,R1                                                            
         ICM   R1,1,TIOBAID        PFKEY PRESSED                                
         CHI   R1,12               MAKE 13-24 LOOK LIKE 1-12                    
         BNH   *+8                                                              
         AHI   R1,-12                                                           
         STC   R1,PFK                                                           
         ICM   R1,3,TIOBCURS       DISPLACEMENT TO FIELD FOR CURSOR             
         STCM  R1,3,SCURSOR                                                     
         DROP  RF                                                               
*                                                                               
INIT10   L     RF,AUTL                                                          
         USING UTLD,RF                                                          
         TM    TSVCREQ,X'01'                                                    
         BNO   *+8                                                              
         OI    TSVCREQ,X'02'                                                    
         DROP  RF                                                               
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY HEADERS PROPERLY                                            *         
***********************************************************************         
         SPACE 1                                                                
TOGGLE   NTR1  ,                                                                
         CLI   PFK,4               TOGGLE DISPLAY TYPE                          
         BNE   *+8                                                              
         XI    DTYPE,255                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIX PFKEY LINE SO IT SHOWS THE CORRECT INFORMATION                  *         
***********************************************************************         
         SPACE 1                                                                
PFKFIX   NTR1  ,                                                                
         XR    R0,R0                                                            
         MVC   TFEPFK,SPACES                                                    
*                                                                               
         CLI   INHELP,YES                                                       
         BE    EXITOK                                                           
*                                                                               
         XC    USCANBLK,USCANBLK                                                
         LA    R2,USCANBLK                                                      
         MVC   0(20,R2),PF04                                                    
         AHI   R2,20                                                            
         LHI   R0,1                                                             
*                                                                               
PFF02    CLI   THSACT,ACTDIS                                                    
         BNE   PFF06                                                            
*                                                                               
         CLC   =CL8'TSTRCVR',TF.FTNAME                                          
         BNE   PFF04                                                            
         OC    STIB,STIB                                                        
         BNZ   PFF04                                                            
         MVC   0(20,R2),PF06                                                    
         AHI   R2,20                                                            
         AHI   R0,1                                                             
*                                                                               
PFF04    CLI   THSELA,0                                                         
         BNE   PFF06                                                            
*                                                                               
         MVC   0(20,R2),PF07                                                    
         AHI   R2,20                                                            
         AHI   R0,1                                                             
         MVC   0(20,R2),PF08                                                    
         AHI   R2,20                                                            
         AHI   R0,1                                                             
*                                                                               
PFF06    GOTO1 AUNSCAN,DMCB,((R0),USCANBLK),TFEPFKH,0,0                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ANY REQUESTED SCROLL                                       *         
***********************************************************************         
         SPACE 1                                                                
NXTREC   NTR1  ,                                                                
         CLI   PFK,6               NEXT RECORD REQUESTED?                       
         BNE   EXITOK                                                           
         OC    STIB,STIB                                                        
         BZ    NXTR01                                                           
*                                                                               
         LA    R2,TFMRACTH                                                      
         ST    R2,FADRH                                                         
         MVI   FERN,19             NO START ALLOWED                             
         B     EXITL                                                            
*                                                                               
NXTR01   CLC   =CL8'TSTRCVR',TF.FTNAME                                          
         BE    NXTR02                                                           
         LA    R2,TFMRIDH          INVALID PFKEY                                
         ST    R2,FADRH                                                         
         MVI   FERN,35                                                          
         B     EXITL                                                            
*                                                                               
NXTR02   LA    R4,STSTACCS                                                      
         USING TSTTABD,R4                                                       
         OC    TSTACCS,TSTACCS                                                  
         BNZ   NXTR04                                                           
         LA    R2,TFMRIDH          NO MATCH                                     
         ST    R2,FADRH                                                         
         MVI   FERN,26                                                          
         B     EXITOK                                                           
*                                                                               
NXTR04   XR    RF,RF                                                            
         OC    TSTLOW,TSTLOW                                                    
         BNZ   NXTR05                                                           
         LA    R2,TFMRIDH          NO MATCH                                     
         ST    R2,FADRH                                                         
         MVI   FERN,26                                                          
         B     EXITOK                                                           
*                                                                               
NXTR05   XR    RF,RF                                                            
         IC    RF,STIK+2                                                        
         AHI   RF,1                                                             
         STC   RF,STIK+2                                                        
*                                                                               
NXTR06   BRAS  RE,RECREAD                                                       
         BE    NXTR08                                                           
*                                                                               
         XR    RF,RF               NEXT TRACK                                   
         ICM   RF,3,STIK                                                        
         AHI   RF,1                                                             
         STCM  RF,3,STIK                                                        
         CLM   RF,3,TSTHIGH                                                     
         BNH   *+10                                                             
         MVC   STIK(2),TSTLOW                                                   
         MVI   STIK+2,01                                                        
         MVI   STIK+3,00                                                        
         B     NXTR06                                                           
*                                                                               
NXTR08   MVC   TFMRID,SPACES       OUTPUT FIRST DISK ADDRESS                    
         MVC   TFMRID(2),=C'A,'                                                 
         GOTO1 AHEXOUT,DMCB,STIK,TFMRID+2,4,0                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ANY REQUESTED SCROLL                                       *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   NTR1  ,                                                                
         CLI   PFK,7               CHECK SCROLL PFKEY PRESSED                   
         BL    EXITOK                                                           
         CLI   PFK,8                                                            
         BH    EXITOK                                                           
*                                                                               
         LA    R2,TFMRACTH         RECORD ID FIELD                              
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
*                                                                               
         CLI   BEFORE,YES          NO SCROLL FIRST TIME                         
         BNE   EXITOK                                                           
         CLI   SCRLOK,NO           ALLOWED TO SCROLL?                           
         BE    EXITOK              YES                                          
         CLI   THSACT,ACTDIS       ??                                           
         BNE   EXITOK                                                           
         CLC   STFMRACT,TFMRACT                                                 
         BNE   EXITOK                                                           
*                                                                               
         XC    SCANBLK,SCANBLK     GET START AND END VALUES                     
         GOTO1 ASCANNER,DMCB,(0,FHD),(3,SCANBLK)                                
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                HOW?                                         
*                                                                               
         XC    FULL,FULL           CLEAR THESE                                  
         XC    FULL1,FULL1                                                      
*                                                                               
         LA    R5,SCANBLK                                                       
         USING SCANBLKD,R5                                                      
         AHI   R5,SCBLKLQ                                                       
         BRAS  RE,VFSFLD                                                        
         BNE   EXITL                                                            
         MVC   FULL,SC1STNUM       START IS IN FULL                             
         AHI   R5,SCBLKLQ                                                       
         CLI   SC1STLEN,0                                                       
         BE    SCRL02                                                           
         BRAS  RE,VFSFLD                                                        
         BNE   EXITL                                                            
         MVC   FULL1,SC1STNUM      END IS IN FULL1                              
*                                                                               
SCRL02   LH    R0,VNDISP                                                        
         MH    R0,VWDISP                                                        
         STH   R0,HALF             SIZE OF SCREEN IS IN HALF                    
*                                                                               
         CLI   PFK,7               SCROLL UP?                                   
         BNE   SCRL04              NO                                           
*                                                                               
         L     RF,FULL                                                          
         SH    RF,HALF                                                          
         BNM   *+6                                                              
         XR    RF,RF                                                            
         ST    RF,FULL                                                          
         STH   RF,STIB             SET START BYTE                               
         STH   RF,DISPSB           SET DISPLACEMENT TO START BYTE               
*                                                                               
         ICM   R0,15,FULL1         ANY END BYTE?                                
         BNZ   *+8                 YES                                          
         LH    R0,SLRL             NO END INPUT - USE RECORD LENGTH             
         SH    R0,STIB                                                          
         AHI   R0,1                                                             
         STH   R0,DISPDL           SET DISPLAY LENGTH                           
         B     SCRL06                                                           
*                                                                               
SCRL04   L     RF,FULL                                                          
         AH    RF,HALF                                                          
         ICM   R1,15,FULL1         ANY REAL END BYTE?                           
         BNZ   *+8                 YES                                          
         LH    R1,SLRL             NO END INPUT - USE RECORD LENGTH             
         CR    RF,R1               SCROLLED PAST END?                           
         BNL   EXITOK              YES - IGNORE SCROLL                          
*                                                                               
         ST    RF,FULL             SET NEW START                                
         STH   RF,STIB                                                          
         STH   RF,DISPSB                                                        
*                                                                               
         LH    RF,DISPDL                                                        
         SH    RF,HALF                                                          
         BNM   *+6                                                              
         DC    H'0'                                                             
         STH   RF,DISPDL                                                        
*                                                                               
SCRL06   MVC   USCANBLK,SPACES                                                  
         LA    R5,SCANBLK                                                       
         LA    R3,USCANBLK         FIRST DO NAME                                
         MVC   USCANBLK,SPACES                                                  
         MVC   0(L'SCONEFLD,R3),SCONEFLD                                        
*                                                                               
         AHI   R3,20               NOW DO START                                 
         L     RF,FULL                                                          
         STH   RF,HALF                                                          
         BRAS  RE,EDOUT                                                         
         MVC   0(5,R3),DUB                                                      
         LHI   R0,2                                                             
*                                                                               
         ICM   RF,15,FULL1         WAS AN END VALUE *REALLY* ENTERED            
         BZ    SCRL08              NO                                           
*                                                                               
         AHI   R3,20               NOW DO END                                   
         MVC   0(L'SCONEFLD,R3),SPACES                                          
         STH   RF,HALF                                                          
         BRAS  RE,EDOUT                                                         
         MVC   0(5,R3),DUB                                                      
         LHI   R0,3                                                             
*                                                                               
SCRL08   XR    RF,RF               CLEAR FIELD AND UNSCAN INTO IT               
         IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+1)                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),SPACES                                                   
         GOTO1 AUNSCAN,DMCB,((R0),USCANBLK),FHD,0,0                             
         B     EXITOK                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ THE REQUESTED RECORD INTO IOAREA                    *         
***********************************************************************         
         SPACE 1                                                                
RECREAD  NTR1  ,                                                                
         MVI   FERN,0                                                           
*                                                                               
         MVI   DISKIOOP,0          SET TO FIRST I/O                             
RECR02   BRAS  RE,DISKIO           DO THE I/O                                   
         BNE   RECRERR             ERROR RETURNED                               
*                                                                               
         CLI   DISKIOOP,1                                                       
         BE    RECR04                                                           
         MVI   DISKIOOP,1          SET TO SECOND I/O                            
*                                                                               
         CLI   TP.PS02,0           ANY SECOND I/O REQUIRED?                     
         BNE   RECR02              YES                                          
*                                                                               
RECR04   CLI   THSACT,ACTBRO       RECORD READ WITH LEN=SLRL                    
         BE    EXITOK                                                           
         MVC   TFMMSG,WRK          SET D/A + LENGTH INFORMATION                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,SLRL                                                        
         BCTR  RF,0                R4=MAX VALUE FOR STIB/STIL                   
         CLM   RF,3,STIB                                                        
         BNL   *+12                                                             
         MVI   FERN,18             ERROR START GT REC LEN                       
         B     RECRERR                                                          
*                                                                               
         CH    RF,STIL                                                          
         BNL   RECR06                                                           
         MVI   FERN,17             ERROR END GT REC LEN                         
         B     RECRERR                                                          
*                                                                               
RECR06   XR    R0,R0                                                            
         ICM   R0,3,STIL           ANY END BYTE?                                
         BNZ   *+6                 YES                                          
         LR    R0,RF               NO END INPUT - USE RECORD LENGTH             
         SH    R0,STIB                                                          
         AHI   R0,1                                                             
         STH   R0,DISPDL           SET DISPLAY LENGTH                           
         B     EXITOK                                                           
*                                                                               
RECRERR  LA    RF,TFMFILEH         RESET ERROR DETAILS                          
         ST    RF,FADRH                                                         
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILE NAME AND SAVE TABLE ENTRY FOR THIS FILE               *         
***********************************************************************         
         SPACE 1                                                                
VFILE    NTR1  ,                                                                
         LA    R2,TFMFILEH         FILENAME FIELD                               
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         L     R3,AFILTAB                                                       
         USING FILTABD,R3                                                       
*                                                                               
FILN02   XR    RF,RF                                                            
         ICM   RF,1,FHIL           MUST HAVE INPUT                              
         BNZ   *+12                                                             
         MVI   FERN,1                                                           
         B     EXITL               ERROR MISSING FILE NAME                      
*                                                                               
         CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   FILN04                                                           
         BRAS  RE,FNHELP                                                        
         BH    FILN02              SELECT FROM HELP BY PFKEY                    
         MVI   INHELP,YES                                                       
         B     EXITL                                                            
*                                                                               
FILN04   CHI   RF,4                MUST BE AT LEAST 4 BYTES                     
         BNL   *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL               ERROR INVALID FILE NAME                      
*                                                                               
         BCTR  RF,0                                                             
         XR    R0,R0                                                            
FILN06   ICM   R0,3,FTDISP         EOT                                          
         BNZ   *+12                NO                                           
         MVI   FERN,02                                                          
         B     EXITL               ERROR INVALID FILE NAME                      
*                                                                               
         EX    RF,FILCMP                                                        
         BE    FILN08                                                           
         AR    R3,R0                                                            
         B     FILN06                                                           
*                                                                               
FILCMP   CLC   FHDA(0),FTNAME      COMPARE FILE NAME                            
*                                                                               
FILN08   ST    R3,AFTNTRY          SAVE ENTRY                                   
         MVC   TFILTAB,FILTABD                                                  
         MVC   FHDA(L'FTNAME),FTNAME                                            
         MVI   FHOL,L'FTNAME                                                    
         DROP  R3                                                               
*                                                                               
FILN10   CLC   =CL8'TEMPSTR',TF.FTNAME                                          
         BNE   FILN12              NOT TEMPSTR                                  
         LH    R1,TWAL                                                          
         BCTR  R1,0                                                             
         STCM  R1,3,TF.FTMAXL1                                                  
         B     EXITOK                                                           
*                                                                               
FILN12   CLC   =CL8'TEMPEST',TF.FTNAME                                          
         BNE   FILN14              NOT TEMPEST                                  
         LH    R1,TMSL                                                          
         BCTR  R1,0                                                             
         STCM  R1,3,TF.FTMAXL1                                                  
         B     EXITOK                                                           
*                                                                               
FILN14   CLC   SFILTAB(L'FTDISP+L'FTNAME),TFILTAB                               
         BE    FILN16                                                           
         MVI   SCRLOK,NO                                                        
         XC    SSTIL,SSTIL         CLEAR ANY SAVED VALUES                       
         XC    SSTIB,SSTIB                                                      
*                                                                               
FILN16   B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD ID                                                  *         
***********************************************************************         
         SPACE 1                                                                
VRID     NTR1  ,                                                                
         LA    R2,TFMRIDH          RECORD ID FIELD                              
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
*                                                                               
         CLI   FHIL,0              FIELD REQUIRES INPUT                         
         BNE   *+12                                                             
         MVI   FERN,03             ERROR MISSING REC ID                         
         B     EXITL                                                            
*                                                                               
         CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   RID02                                                            
         BRAS  RE,RIHELP                                                        
         BH    *+12                HIGH MEANS SELECTION MADE                    
         MVI   INHELP,YES                                                       
         B     EXITL                                                            
*                                                                               
RID02    LA    R3,RIDTBL           NOW TRY TO MATCH ACTION TYPED IN             
         USING KEYTBLD,R3                                                       
*                                                                               
RID04    CLI   KEYTNAME,0          EOT                                          
         BNE   *+12                NO                                           
         MVI   FERN,04             ERROR INVALID REC ID                         
         B     EXITL                                                            
*                                                                               
         CLC   KEYTNAME,FHDA       TRY TO MATCH NAME TO ACTION                  
         BE    RID06                                                            
         AHI   R3,KEYTBLL                                                       
         B     RID04                                                            
*                                                                               
RID06    L     R4,AFTNTRY          SAVE ACTION                                  
         AHI   R4,FILTABL                                                       
         USING PERMTABD,R4                                                      
*                                                                               
RID08    CLI   PKEY,0                                                           
         BNE   *+12                NO                                           
         MVI   FERN,49             INVALID ACTION                               
         B     EXITL                                                            
*                                                                               
         CLC   PKEY,KEYTACT        MATCH ACTION                                 
         BE    RID10                                                            
         AHI   R4,PERMTABL                                                      
         B     RID08                                                            
         DROP  R4                                                               
*                                                                               
RID10    MVC   THSKEYN,KEYTACT                                                  
         ICM   RF,15,KEYTRTN       ADDITIONAL VALIDATION?                       
         BZ    EXITOK              NO                                           
         A     RF,RELO                                                          
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* RECORD ID FORMAT K,                                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDK     MVC   WSS,SPACES                                                       
                                                                                
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         AHI   RF,-3                                                            
         BNM   RIDK01              ERROR NO KEY DATA                            
         MVI   FERRDSP,2                                                        
         MVI   FERN,48                                                          
         B     EXITL                                                            
*                                                                               
RIDK01   EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WSS(0),FHDA+2       SET WSS FOR V(DECODE)                        
*                                                                               
         XR    R0,R0                                                            
         IC    R0,TF.FTKEYL         GET KEY LEN                                 
         XR    RF,RF                                                            
         IC    RF,TF.FTFILL         GET KEY FILL CHR                            
         GOTO1 ADECODE,PLIST,((R0),WSS),((RF),STIK),0                           
         CLI   8(R1),X'FF'                                                      
         BNE   RIDK02                                                           
         MVC   FERNA,9(R1)         KEY INVALID                                  
         MVI   FERN,255            SET SPECIAL ERR MSG CODE                     
         MVI   FERRDSP,2                                                        
         B     EXITL                                                            
*                                                                               
RIDK02   CLC   =CL8'TEMPSTR',TF.FTNAME                                          
         BE    RIDK04                                                           
         CLC   =CL8'TEMPEST',TF.FTNAME                                          
         BE    RIDK04                                                           
         MVC   STIKL,TF.FTKEYL     KEY VALID                                    
         B     EXITOK                                                           
*                                                                               
RIDK04   MVI   STIKL,4             TEMP(STR)/(EST) KEY = X'PP00TTTT'            
         XR    RF,RF                                                            
         IC    RF,STIK                                                          
         CHI   RF,X'80'                                                         
         BL    RIDK06                                                           
         AHI   RF,-X'80'                                                        
         CH    RF,TMPSTRMX                                                      
         BNH   RIDK08                                                           
         MVI   FERN,44             ERROR IN TEMPSTR PAGE/TERMINAL               
         B     EXITL                                                            
*                                                                               
RIDK06   CH    RF,TMPSTRMX                                                      
         BNH   *+12                                                             
         MVI   FERN,44             ERROR IN TEMPSTR PAGE/TERMINAL               
         B     EXITL                                                            
*                                                                               
RIDK08   CLI   STIK+1,0                                                         
         BE    *+12                                                             
         MVI   FERN,44             ERROR IN TEMPSTR PAGE/TERMINAL               
         B     EXITL                                                            
*                                                                               
         MVI   STIK+1,X'FF'                                                     
         CLC   STIK+2(2),=H'5000'                                               
         BNH   *+12                                                             
         MVI   FERN,44             ERROR IN TEMPSTR PAGE/TERMINAL               
         B     EXITL                                                            
*                                                                               
         OC    STIK+2(2),STIK+2    IS TRM NUM ZERO                              
         BNZ   EXITOK              NO                                           
         L     RF,AUTL                                                          
         MVC   STIK+2(2),TNUM-UTLD(RF)                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD ID FORMAT A,                                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDA     XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         AHI   RF,-2                                                            
         BNZ   RIDA02                                                           
         MVI   FERRDSP,2                                                        
         MVI   FERN,46             ERROR NO KEY DATA                            
         B     EXITL                                                            
*                                                                               
RIDA02   CHI   RF,8                                                             
         BE    RIDA04                                                           
         MVI   FERRDSP,2                                                        
         MVI   FERN,09             ERROR DISK ADDR NOT 8 HEX CHRS               
         B     EXITL                                                            
*                                                                               
RIDA04   GOTO1 AHEXIN,PLIST,FHDA+2,STIK,8,0                                     
         OC    12(4,R1),12(R1)                                                  
         BNZ   RIDA14                                                           
*                                                                               
         LHI   R0,8                FIND BAD BYTE                                
         LA    RF,FHDA+2                                                        
RIDA06   CLI   0(RF),C'A'                                                       
         BL    RIDA12                                                           
         CLI   0(RF),C'F'                                                       
         BNH   RIDA10                                                           
*                                                                               
RIDA08   CLI   0(RF),C'0'                                                       
         BL    RIDA12                                                           
         CLI   0(RF),C'9'                                                       
         BH    RIDA12                                                           
*                                                                               
RIDA10   AHI   RF,1                                                             
         BCT   R0,RIDA06                                                        
*                                                                               
RIDA12   LA    R0,FHDA             SET DISPLACEMENT TO BAD HEX                  
         SR    RF,R0                                                            
         STC   RF,FERRDSP                                                       
         MVI   FERN,07             ERROR INVALID HEX                            
         B     EXITL                                                            
*                                                                               
RIDA14   CLI   TF.FTLOG,FTL20      20 BIT DISK ADDRESS?                         
         BE    RIDA20                                                           
         MVI   STIKL,4                                                          
         OC    STIK(2),STIK        CHECK DISK ADDRESS                           
         BNZ   RIDA16                                                           
         MVI   FERRDSP,2                                                        
         MVI   FERN,10             TTTT MUST BE 01 OR GREATER                   
         B     EXITL                                                            
*                                                                               
RIDA16   CLI   STIK+2,0                                                         
         BNE   RIDA30                                                           
         MVI   FERRDSP,6                                                        
         MVI   FERN,47             BB MUST BE 01 OR GREATER                     
         B     EXITL                                                            
*                                                                               
RIDA20   MVI   STIKL,4                                                          
         OC    STIK(3),STIK        CHECK DISK ADDRESS                           
         BNZ   RIDA22                                                           
         MVI   FERRDSP,2                                                        
         MVI   FERN,53             TTTT MUST BE 01 OR GREATER                   
         B     EXITL                                                            
*                                                                               
RIDA22   TM    STIK+2,X'0F'                                                     
         BZ    RIDA24                                                           
         MVI   FERRDSP,7                                                        
         MVI   FERN,54             BB MUST BE 01 OR GREATER                     
         B     EXITL                                                            
*                                                                               
RIDA24   CLI   STIK+3,0                                                         
         BNE   RIDA30                                                           
         MVI   FERRDSP,8                                                        
         MVI   FERN,55             BB MUST BE 01 OR GREATER                     
         B     EXITL                                                            
*                                                                               
RIDA30   XC    FADRH,FADRH                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD ID FORMAT FI                                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDF     CLI   TF.FT1STKY,FT1STKNO                                              
         BNE   *+12                CANT HAVE FIRST FOR FILE                     
         MVI   FERN,04             ERROR INV HEX                                
         B     EXITL                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TF.FT1STKY                                                    
         XR    R1,R1                                                            
         STC   R1,STIK(RF)         SET STIK TO ZEROS                            
         LA    RF,1(RF)                                                         
         STC   RF,STIKL                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD ID FORMAT NE                                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDN     CLC   TF.FTNAME,SLRF      SAME FILENAME?                               
         BNE   RIDN02                                                           
         CLI   SLRI,0                                                           
         BE    RIDN02                                                           
         B     EXITOK                                                           
*                                                                               
RIDN02   MVI   FERN,11             ERROR NO PREV REC FOR FILE                   
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* RECORD ID FORMAT I=                                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDI     CLI   FHIL,2                                                           
         BH    RIDI02                                                           
         MVI   FERRDSP,2                                                        
         MVI   FERN,34             ERROR NO test id                             
         B     EXITL                                                            
*                                                                               
RIDI02   ICM   R4,15,ATSTTAB       TSTTAB FOUND?                                
         BNZ   *+12                                                             
         MVI   FERN,24             NO TSTTAB                                    
         B     EXITL                                                            
*                                                                               
         GOTO1 ASCANNER,DMCB,(0,FHD),(X'82',SCANBLK)                            
         CLI   4(R1),1                                                          
         BE    *+12                                                             
         MVI   FERN,05             ERROR IN SCANNER                             
         B     EXITL                                                            
*                                                                               
         LA    R5,SCANBLK                                                       
         USING SCANBLKD,R5                                                      
*                                                                               
         LH    RE,0(R4)                                                         
         ICM   RF,15,2(R4)                                                      
         AHI   R4,6                                                             
         USING TSTTABD,R4                                                       
*                                                                               
RIDI04   CLC   TSTACCS,SC2NDFLD                                                 
         BE    RIDI06              NO                                           
         BXLE  R4,RE,RIDI04                                                     
         MVI   FERN,26                                                          
         MVC   FERRDSP,SC2NDFLD                                                 
         B     EXITL                                                            
*                                                                               
RIDI06   MVC   STSTACCS,0(R4)                                                   
         MVI   STIKL,4                                                          
         XC    STIK,STIK                                                        
         MVC   STIK(2),TSTLOW                                                   
         MVI   STIK+2,01                                                        
*                                                                               
         MVC   TFMRID,SPACES       OUTPUT FIRST DISK ADDRESS                    
         MVC   TFMRID(2),=C'A,'                                                 
         GOTO1 AHEXOUT,DMCB,STIK,TFMRID+2,4,0                                   
         B     EXITOK                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* RECORD ID FORMAT ME                                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDM     ICM   R5,15,AUTL          TSTTAB FOUND?                                
         BNZ   *+12                                                             
         MVI   FERN,26             NO TSTTAB                                    
         B     EXITL                                                            
         USING UTLD,R5                                                          
*                                                                               
         ICM   R4,15,TACCS                                                      
         BNZ   *+12                                                             
         MVI   FERN,26             NO TSTTAB                                    
         B     EXITL                                                            
         USING TSTTABD,R4                                                       
*                                                                               
         CLC   TNUM,TSTNUM                                                      
         BE    *+12                                                             
         MVI   FERN,26             NO TSTTAB                                    
         B     EXITL                                                            
*                                                                               
         MVC   STSTACCS,0(R4)                                                   
         MVI   STIKL,4                                                          
         XC    STIK,STIK                                                        
         MVC   STIK(2),TSTLOW                                                   
         MVI   STIK+2,01                                                        
*                                                                               
         MVC   TFMRID,SPACES       OUTPUT FIRST DISK ADDRESS                    
         MVC   TFMRID(2),=C'A,'                                                 
         GOTO1 AHEXOUT,DMCB,STIK,TFMRID+2,4,0                                   
         B     EXITOK                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* RECORD ID FORMAT LA                                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDL     CLC   TF.FTNAME,SLRF      SAME FILENAME?                               
         BNE   RIDL1               NO                                           
         CLI   SLRI,0                                                           
         BE    RIDL1               NO VALID I/O                                 
         CLI   SLIRA,1                                                          
         BNE   RIDL1               PREV ACTION MUST BE DISP                     
         MVC   STIKL,SLIKL                                                      
         MVC   STIK,SLIK                                                        
         B     EXITOK                                                           
*                                                                               
RIDL1    MVI   FERN,11             ERROR NO PREV REC FOR FILE                   
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* RECORD ACTION IS OPTIONAL - A DEFAULT IS SET FOR EACH FILE TYPE     *         
***********************************************************************         
         SPACE 1                                                                
VRACT    NTR1  ,                                                                
         LA    R2,TFMRACTH         RECORD ID FIELD                              
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0              ANY INPUT?                                   
         BNE   RAC06               YES                                          
*                                                                               
         LHI   R1,ACTDIS           SET DEFAULT ACTION                           
         CLI   TF.FTORG,FTOIS                                                   
         BNE   *+8                                                              
         LHI   R1,ACTBRO                                                        
         STC   R1,THSACT           SET DEFAULT ACTION                           
*                                                                               
         LA    R1,RACTTBL          DISPLAY ACTION NAME                          
         USING ACTNTBLD,R1                                                      
RAC02    CLC   THSACT,ACTNNUM                                                   
         BE    *+12                                                             
         AHI   R1,ACTNTBLL                                                      
         B     RAC02                                                            
*                                                                               
RAC04    MVC   FHDA(3),ACTNNAME    SHOW ONLY DIS/CHA/BRO ETC                    
         MVI   FHIL,3                                                           
         MVI   FHOL,3                                                           
         MVI   FHII,(FHIITH+FHIIAL)                                             
         OI    FHOI,FHOITR                                                      
         DROP  R1                                                               
*                                                                               
RAC06    CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   RAC08                                                            
         BRAS  RE,RAHELP                                                        
         BH    *+12                HIGH MEANS SELECTION MADE                    
         MVI   INHELP,YES                                                       
         B     EXITL               HIGH MEANS SELECTION MADE                    
*                                                                               
RAC08    XC    SCANBLK,SCANBLK                                                  
         GOTO1 ASCANNER,DMCB,(0,FHD),(X'83',SCANBLK)                            
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   FERN,05             ERROR IN SCANNER                             
         B     EXITL                                                            
*                                                                               
         LA    R5,SCANBLK          POINT TO ACTION LINE                         
         USING SCANBLKD,R5                                                      
         CLI   SC2NDLEN,0                                                       
         BNE   RAC18                                                            
         CLI   SC1STLEN,0          ANY INPUT?                                   
         BE    RAC18               NO                                           
         CLI   SC1STLEN,7                                                       
         BH    RAC18               ACTION NAME MUST BE LE 7                     
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                RF=L'ACTION NAME-1                           
         LA    R1,RACTTBL                                                       
         USING ACTNTBLD,R1                                                      
*                                                                               
RAC10    CLI   ACTNNAME,EOT        SEARCH ACTION TABLE                          
         BE    RAC18                                                            
         EX    RF,*+8                                                           
         BE    RAC12                                                            
         CLC   SC1STFLD(0),ACTNNAME                                             
         AHI   R1,ACTNTBLL                                                      
         B     RAC10                                                            
*                                                                               
RAC12    MVC   THSACT,ACTNNUM      SAVE ACTION VALUE                            
         DROP  R1                                                               
*                                                                               
         L     RF,AFTNTRY          NOW MAKE SURE IT VALID FOR TYPE              
         AHI   RF,FILTABL                                                       
         USING PERMTABD,RF                                                      
*                                                                               
RAC14    CLI   PKEY,EOT                                                         
         BE    RAC18                                                            
         CLC   PKEY,THSKEYN                                                     
         BNE   RAC16                                                            
         CLC   PACT,THSACT                                                      
         BE    RAC20                                                            
*                                                                               
RAC16    AHI   RF,PERMTABL                                                      
         B     RAC14                                                            
         DROP  RF                                                               
*                                                                               
RAC18    MVI   FERN,25             INVALID ACTION NAME                          
         B     EXITL                                                            
*                                                                               
RAC20    MVC   TPERMTAB,0(RF)      SAVE THIS PERMTAB ENTRY                      
         CLC   SPERMTAB,TPERMTAB   SAME AS LAST TIME?                           
         BE    RAC22               YES                                          
         MVI   SCRLOK,NO                                                        
         XC    SSTIL,SSTIL         CLEAR ANY SAVED VALUES                       
         XC    SSTIB,SSTIB                                                      
*                                                                               
RAC22    AHI   R5,SCBLKLQ      *** START BYTE                                   
         XC    STIB,STIB                                                        
         XC    DISPSB,DISPSB                                                    
*                                                                               
         CLI   SC2NDLEN,0          FORMAT IS KEYWORD=VALUE                      
         BNE   RAC28               NO                                           
         CLI   SC1STLEN,0          FORMAT IS VALUE                              
         BE    RAC34               NO                                           
*                                                                               
*                                  TEMPSTR CAN HAVE 2 SPECIALS                  
         CLC   =CL8'TEMPSTR',TF.FTNAME                                          
         BNE   RAC26                                                            
*                                                                               
         CLC   =C'CHK',SC1STFLD    CHKPNT AREA?                                 
         BNE   RAC24               NO                                           
         LH    RF,TWCHKD                                                        
         STCM  RF,15,SC1STNUM                                                   
         B     RAC32                                                            
*                                                                               
RAC24    CLC   =C'GLO',SC1STFLD    GLOBAL AREA?                                 
         BNE   RAC26               NO                                           
         LH    RF,TWGLBD                                                        
         STCM  RF,15,SC1STNUM                                                   
         B     RAC32                                                            
*                                                                               
RAC26    BRAS  RE,VFSFLD           VALIDATE NUMERIC OR HEX INPUT                
         BNE   EXITL                                                            
         B     RAC32                                                            
*                                                                               
RAC28    CLC   =C'HEX',SC1STFLD    HEX=HHHH (HEX)                               
         BE    RAC30                                                            
         MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
RAC30    BRAS  RE,VALSCHEX         VALIDATE SECOND FIELD HEX VALUE              
         BNE   EXITL                                                            
         MVC   SC1STNUM,SC2NDNUM   DUMMY UP FIELD                               
*                                                                               
RAC32    ICM   RF,15,SC1STNUM      SET VALUES                                   
         STH   RF,STIB                                                          
         STH   RF,DISPSB                                                        
*                                                                               
RAC34    AHI   R5,SCBLKLQ      *** END BYTE                                     
         XC    STIL,STIL                                                        
         CLI   SC1STLEN,0                                                       
         BE    RAC46               NO END BYTE INFO                             
         CLI   SC2NDLEN,0                                                       
         BE    RAC36               NO SECOND HALF                               
         MVI   FERN,14             ERROR INVALID END                            
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
RAC36    BRAS  RE,VFSFLD           VALIDATE NUMERIC OR HEX INPUT                
         BNE   EXITL                                                            
*                                                                               
RAC44    MVC   FERRDSP,SC1STNUM                                                 
         XR    RF,RF                                                            
         ICM   RF,7,SC1STNUM+1                                                  
         CHI   RF,24*1024          CHECK END VALUE IN RF                        
         BL    *+12                                                             
         MVI   FERN,14             ERROR INVALID END                            
         B     EXITL                                                            
         CH    RF,STIB             MAKE SURE START < END                        
         BNL   *+12                                                             
         MVI   FERN,16                                                          
         B     EXITL                                                            
         STH   RF,STIL                                                          
         MVI   FERRDSP,0                                                        
*                                                                               
RAC46    CLC   STIB,TF.FTMAXL1     MORE CHECKS ON START,END                     
         BNH   *+12                                                             
         MVI   FERN,18             ERROR START GT MAX REC LEN                   
         B     EXITL                                                            
         CLC   STIL,TF.FTMAXL1                                                  
         BNH   *+12                                                             
         MVI   FERN,17             ERROR END GT MAX REC LEN                     
         B     EXITL                                                            
*                                                                               
         MVC   HALF1,STIL          SAVE THIS FOR REDISPLAY                      
         OC    STIL,STIL                                                        
         BNZ   RAC50                                                            
         CLI   TF.FTYPE,FTFL                                                    
         BNE   RAC50                                                            
         MVC   STIL,TF.FTMAXL1     SET STIL FOR F/L FILES                       
*                                                                               
RAC50    CLI   THSACT,ACTDIS       DISPLAY                                      
         BE    RAC52                                                            
         OC    STIB,STIB                                                        
         BNZ   RAC52                                                            
         OC    STIL,STIL                                                        
         BNZ   RAC52                                                            
         B     RACX                                                             
*                                                                               
RAC52    MVC   USCANBLK,SPACES                                                  
         LA    R5,SCANBLK                                                       
         LA    RF,USCANBLK         FIRST DO NAME                                
         MVC   0(L'SCONEFLD,RF),SCONEFLD                                        
*                                                                               
         AHI   RF,20               NOW DO START NUMBER                          
         AHI   R5,SCBLKLQ                                                       
         MVC   0(L'SCONEFLD,RF),SPACES                                          
         MVC   HALF,STIB                                                        
         BRAS  RE,EDOUT            OUTPUT NUMBER CORRECTLY                      
         MVC   0(5,RF),DUB                                                      
         LHI   R0,2                R0 = COUNT OF UNSCAN FIELDS                  
*                                                                               
         OC    HALF1,HALF1         WAS AN END VALUE *REALLY* ENTERED            
         BZ    RAC58               NO                                           
*                                                                               
         AHI   RF,20               NOW DO END VALUE                             
         MVC   0(L'SCONEFLD,RF),SPACES                                          
         MVC   HALF,HALF1                                                       
         BRAS  RE,EDOUT            OUTPUT NUMBER CORRECTLY                      
         MVC   0(5,RF),DUB                                                      
         LHI   R0,3                                                             
*                                                                               
RAC58    XR    RF,RF               CLEAR FIELD AND UNSCAN INTO IT               
         IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+1)                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),SPACES                                                   
         GOTO1 AUNSCAN,DMCB,((R0),USCANBLK),FHD,0,0                             
*                                                                               
RACX     B     EXITOK                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ELEMENT ID IS OPTIONAL                                              *         
***********************************************************************         
         SPACE 1                                                                
VEID     NTR1  ,                                                                
         LA    R2,TFMEIDH          ELEMENT ID FIELD                             
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
*                                                                               
         CLI   TF.FTLOG,FTLRCV     RECOVERY FILES DEFAULT TO ELEMENT            
         BE    *+12                                                             
         CLI   TF.FTYPE,FTVLE      VLE DEFAULT IS TO FI                         
         BNE   EID02                                                            
         CLI   THSACT,ACTDIS       BUT ONLY FOR RECORD DISPLAY                  
         BNE   EID02                                                            
*                                                                               
         OC    STIB,STIB           IF START BYTE                                
         BZ    EID01                                                            
         CLI   TF.FTLOG,FTLRCV     AND RECOVERY FILE                            
         BNE   EID01                                                            
         B     EXITOK              JUST EXIT                                    
*                                                                               
EID01    CLI   FHIL,0                                                           
         BNE   EID02                                                            
         MVC   FHDA(2),=C'FI'                                                   
         MVI   FHIL,2                                                           
*                                                                               
EID02    CLI   FHIL,0              INPUT IS OPTIONAL                            
         BE    EXITOK                                                           
*                                                                               
         CLI   THSACT,ACTDIS       ONLY FOR RECORD DISPLAY                      
         BE    EID04                                                            
         MVI   FERN,06             ERROR INV EL ID                              
         B     EXITL                                                            
*                                                                               
EID04    CLI   TF.FTYPE,FTVLE      ONLY FOR VLE FILES                           
         BE    EID06                                                            
         CLI   TF.FTLOG,FTLRCV     OR RECOVERY FILES                            
         BE    EID06                                                            
         MVI   FERN,27             NOT DEFINED FOR FILE                         
         B     EXITL                                                            
*                                                                               
EID06    OC    STIL,STIL                                                        
         BZ    EID08                                                            
         LA    R2,TFMRACTH         RECORD ACTION FIELD                          
         ST    R2,FADRH                                                         
         MVI   FERN,15             END VALUE MUST BE ZERO                       
         B     EXITL                                                            
*                                                                               
EID08    LA    R4,EIDTBL                                                        
         USING KEYTBLD,R4                                                       
*                                                                               
EID10    CLI   KEYTNAME,EOT        SEARCH ID TABLE                              
         BNE   *+12                                                             
         MVI   FERN,25             INVALID ACTION NAME                          
         B     EXITL                                                            
*                                                                               
         CLC   KEYTNAME,FHDA       TRY TO MATCH NAME                            
         BE    EID12                                                            
         AHI   R4,KEYTBLL                                                       
         B     EID10                                                            
*                                                                               
EID12    MVC   THSELA,KEYTACT                                                   
         OC    STIB,STIB           ONLY IF NO START OR END                      
         BZ    EID14                                                            
*                                                                               
         LA    R2,TFMRACTH         RECORD ACTION FIELD                          
         ST    R2,FADRH                                                         
         MVI   FERN,19             START VALUE MUST BE ZERO                     
         B     EXITL                                                            
*                                                                               
EID14    ICM   RF,15,KEYTRTN                                                    
         BZ    EXITOK                                                           
         A     RF,RELO                                                          
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* ELEMENT ID S,                                                       *         
***********************************************************************         
         SPACE 1                                                                
EIDS     CLI   FHIL,3                                                           
         BH    EIDS02                                                           
         MVI   FERN,43             ERROR INVALID ELEMENT ID                     
         MVI   FERRDSP,2                                                        
         B     EXITL                                                            
*                                                                               
EIDS02   XC    SCANBLK,SCANBLK                                                  
         GOTO1 ASCANNER,DMCB,(0,FHD),(X'83',SCANBLK)                            
         CLI   4(R1),2                                                          
         BE    *+12                                                             
         MVI   FERN,51             TOO MANY INPUT PARAMETERS                    
         B     EXITL                                                            
*                                                                               
         LA    R5,SCANBLK                                                       
         AHI   R5,SCBLKLQ                                                       
         USING SCANBLKD,R5                                                      
         CLI   SC2NDLEN,0                                                       
         BE    *+12                                                             
         MVI   FERN,43             INVALID FORMAT                               
         B     EXITL                                                            
*                                                                               
         TM    SC1STVAL,SCNUMQ                                                  
         BO    EIDS04                                                           
         MVI   FERN,13             START NOT NUMERIC                            
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
EIDS04   ICM   RF,15,SC1STNUM                                                   
         STCM  RF,3,STIE                                                        
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ELEMENT ID I,                                                       *         
***********************************************************************         
         SPACE 1                                                                
EIDI     CLI   FHIL,3                                                           
         BH    EIDI02                                                           
         MVI   FERN,07             ERROR INVALID HEX                            
         B     EXITL                                                            
*                                                                               
EIDI02   XC    SCANBLK,SCANBLK                                                  
         GOTO1 ASCANNER,DMCB,(0,FHD),(X'83',SCANBLK)                            
         CLI   4(R1),2                                                          
         BE    *+12                                                             
         MVI   FERN,51             TOO MANY INPUT PARAMETERS                    
         B     EXITL                                                            
*                                                                               
         LA    R5,SCANBLK                                                       
         AHI   R5,SCBLKLQ                                                       
         USING SCANBLKD,R5                                                      
         CLI   SC2NDLEN,0                                                       
         BE    EIDI04                                                           
         MVI   FERN,43             INVALID FORMAT                               
         MVC   FERRDSP,SC2NDNUM                                                 
         B     EXITL                                                            
*                                                                               
EIDI04   TM    SC1STVAL,SCHEXQ                                                  
         BO    EIDI06                                                           
         MVI   FERN,07             INVALID HEX                                  
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
EIDI06   XR    R0,R0                                                            
         IC    R0,SC1STLEN                                                      
         GOTO1 AHEXIN,DMCB,SCONEFLD,STIE,(R0),0                                 
         L     RF,12(R1)                                                        
         STC   RF,STIEL                                                         
         CHI   RF,0                                                             
         BNE   EXITOK                                                           
         MVI   FERN,07             INVALID HEX                                  
         MVI   FERRDSP,3                                                        
         B     EXITL                                                            
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT A HALFWORD INTO DUB AS EITHER HEX OR DEC          *         
* NTRY: HALF   = NUMBER TO EDIT                                       *         
* EXIT: DUB    = NUMBER TO AS HEX OR DEC (DEPENDS ON DTYPE)           *         
***********************************************************************         
         SPACE 1                                                                
EDOUT    NTR1  ,                                                                
         MVC   DUB,SPACES                                                       
         CLI   DTYPE,DTHEX                                                      
         BE    EDOUT02                                                          
         EDIT  (B2,HALF),(5,DUB),0,ALIGN=LEFT,ZERO=NOBLANK                      
         B     EXITOK                                                           
*                                                                               
EDOUT02  LHI   R0,2                EDIT 2 OR 4 BYTES AS REQUIRED                
         LA    RF,HALF                                                          
         LH    RE,HALF                                                          
         CLI   HALF,0                                                           
         BNE   EDOUT04                                                          
         LHI   R0,1                                                             
         LA    RF,HALF+1                                                        
*                                                                               
EDOUT04  GOTO1 AHEXOUT,DMCB,(RF),DUB,(R0),0                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE FIRST INPUT FIELD AS NUMBER OR HEX              *         
* NTRY: R2        = A(FHD)                                            *         
*       R5        = A(SCANBLKD)                                       *         
* EXIT: SC1STNUM  = NUMERIC VALUE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R2                                                           
         USING SCANBLKD,R5                                                      
VFSFLD   NTR1  ,                                                                
         MVC   FERRDSP,SC1STNUM    SAVE DISPLACEMENT TO ERROR                   
*                                                                               
         CLI   PFK,4               TOGGLE THIS TIME?                            
         BNE   VFS02                                                            
         CLC   TFMRACT,STFMRACT    FIELD THE SAME AS LAST TIME?                 
         BE    VFS02               YES                                          
*                                                                               
         CLI   DTYPE,DTHEX                                                      
         BE    VFSDEC                                                           
         B     VFSHEX                                                           
*                                                                               
VFS02    CLI   DTYPE,DTHEX                                                      
         BE    VFSHEX                                                           
         B     VFSDEC                                                           
*                                                                               
VFSHEX   TM    SC1STVAL,SCHEXQ                                                  
         BO    *+12                                                             
         MVI   FERN,07             INVALID HEX                                  
         B     EXITL                                                            
*                                                                               
         CLI   SC1STLEN,4                                                       
         BNH   *+12                                                             
         MVI   FERN,28             INPUT TOO LONG                               
         B     EXITL                                                            
*                                                                               
         XC    SC1STNUM,SC1STNUM                                                
         MVC   DUB,ZEROS           JUSTIFY FIELD INTO DUB                       
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         LHI   R1,L'DUB                                                         
         SR    R1,RF                                                            
         LA    R1,DUB(R1)                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SC1STFLD                                                 
*                                                                               
         GOTO1 AHEXIN,DMCB,DUB,SC1STNUM,L'DUB                                   
         B     VFSX                                                             
*                                                                               
VFSDEC   TM    SC1STVAL,SCNUMQ                                                  
         BO    *+12                                                             
         MVI   FERN,13             INVALID number                               
         B     EXITL                                                            
*                                                                               
VFSX     MVC   SC1STNUM(1),FERRDSP                                              
         MVI   FERRDSP,0                                                        
         B     EXITOK                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A HEX NUMBER AS A SECOND PARAMETER OF A SCANBLK *         
* NTRY: R5     = A(SCANBLKD LINE)                                     *         
* EXIT: CC EQ  = SC2NDNUM FILLED IN                                   *         
*       CC NE  = INVALID HEX VALUE - ERROR SET                        *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R5                                                      
VALSCHEX NTR1  ,                                                                
         MVC   FERRDSP,SC2NDNUM                                                 
         TM    SC2NDVAL,SCHEXQ                                                  
         BO    *+12                                                             
         MVI   FERN,07             INVALID HEX                                  
         B     EXITL                                                            
*                                                                               
         XC    SC2NDNUM,SC2NDNUM                                                
         MVC   DUB,ZEROS           JUSTIFY FIELD INTO DUB                       
         XR    RF,RF                                                            
         IC    RF,SC2NDLEN                                                      
         LHI   R1,L'DUB                                                         
         SR    R1,RF                                                            
         LA    R1,DUB(R1)                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SC2NDFLD                                                 
*                                                                               
         GOTO1 AHEXIN,DMCB,DUB,SC2NDNUM,L'DUB                                   
         MVC   SC2NDNUM(1),FERRDSP                                              
         MVI   FERRDSP,0                                                        
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISERR   NTR1  ,                                                                
         LA    R2,TFMHDRH                                                       
         MVC   TFMHDR,SPACES                                                    
         USING FHD,R2                                                           
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
         A     RF,RELO                                                          
         AR    RF,R0                                                            
         MVC   0(EMSGL,R4),0(RF)                                                
*                                                                               
DERR04   ICM   R2,15,FADRH         SET CURSOR ON BAD FIELD                      
         BZ    EXITOK                                                           
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY OK MESSAGE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOK    NTR1  ,                                                                
         LA    R2,TFMHDRH                                                       
         USING FHD,R2                                                           
         MVC   TFMHDR,SPACES                                                    
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
         A     RF,RELO                                                          
         AR    RF,R0                                                            
         MVC   0(IMSGL,R4),0(RF)                                                
*                                                                               
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         ICM   RF,15,FADRH                                                      
         BNZ   *+8                                                              
         LA    RF,TFMFILEH                                                      
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD ON SCREEN                                 *         
***********************************************************************         
         SPACE 1                                                                
RECDIS   NTR1  ,                                                                
         MVI   FERN,0                                                           
         XR    R0,R0                                                            
         STH   R0,DISPSLN          SET START LINE NUM TO 1ST                    
*                                                                               
         CLI   THSACT,ACTBRO                                                    
         BNE   RDIS02                                                           
         BRAS  RE,BROWSE           GO TO BROWSE ROUTINE                         
         BL    EXITL                                                            
         B     RDIS06                                                           
*                                                                               
RDIS02   CLI   THSELA,0            ELEMENT DISPLAY?                             
         BE    RDIS04              NO                                           
         OC    STIB,STIB           START BYTE INSTEAD                           
         BNZ   RDIS04                                                           
*                                                                               
         BRAS  RE,ELEMENT          GO TO ELEMENT ROUTINE                        
         BE    RDIS06                                                           
*                                                                               
         CLI   FERN,52                                                          
         BNE   EXITL                                                            
*                                                                               
         XC    DISPSLN,DISPSLN                                                  
         XC    DISPSB,DISPSB       FIRST DISPLAY KEY/CONT/SAVE                  
         MVC   DISPDL,SLRL                                                      
*                                                                               
         MVI   DISPOP,X'0F'        PART DISP HEX,BYTES,CHRS                     
         BRAS  RE,DISP                                                          
         BL    EXITL                                                            
         MVI   FERN,52                                                          
         B     EXITL                                                            
*                                                                               
RDIS04   MVI   DISPOP,X'0F'        PART DISP HEX,BYTES,CHRS                     
         BRAS  RE,DISP                                                          
         BL    EXITL                                                            
*                                                                               
RDIS06   MVI   HDRN,1              ACTION COMPLETED                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY IN BROWSE FORMAT                                            *         
***********************************************************************         
         SPACE 1                                                                
BROWSE   NTR1  ,                                                                
*                                                                               
BRS02    MVC   DISPSB,STIB         SET START BYTE                               
         CLC   STIB,SLRL                                                        
         BL    BRS04                                                            
*                                                                               
         LH    RF,STIB             SET OUT OF RANGE MESSAGE                     
         LA    RF,IOAREA(RF)                                                    
         MVC   0(20,RF),=CL20'*Out of Range-L=XXXX'                             
         MVC   16(4,RF),WRK+3                                                   
         MVC   DISPDL,=H'20'                                                    
         B     BRS06                                                            
*                                                                               
BRS04    LH    R0,SLRL                                                          
         BCTR  R0,0                R0=RECORD END BYTE                           
         XR    RF,RF                                                            
         ICM   RF,3,STIL           RF=INPUT END BYTE                            
         BNZ   *+6                                                              
         LR    RF,R0               IF ZERO SET TO RECORD END                    
*                                                                               
         CR    RF,R0               IF RECORD SHORTER THAN INPUT                 
         BNH   *+6                                                              
         LR    RF,R0               SET TO SHORTER LENGTH                        
*                                                                               
         SH    RF,STIB                                                          
         LA    RF,1(RF)                                                         
         STH   RF,DISPDL           SET LENGTH TO DISPLAY                        
*                                                                               
BRS06    MVI   DISPOP,X'0E'        WHOLE DISP HEX,BYTES,CHRS                    
         BRAS  RE,DISP                                                          
         CLI   DISPRES,DRPART      DID IT FIT?                                  
         BNE   BRS08               YES                                          
*                                                                               
         XC    DISPRES,DISPRES                                                  
         OC    DISPSLN,DISPSLN     WAS THIS THE FIRST RECORD?                   
         BNZ   BROWSEX             NO                                           
*                                                                               
         LH    RF,VWDISP           IF THE FIRST WONT FIT                        
         MH    RF,VNDISP                                                        
         STH   RF,DISPDL           ADJUST LENGTH TO MAKE IT FIT                 
         MVC   DISPSB,STIB                                                      
         XC    DISPSLN,DISPSLN                                                  
         B     BRS06               AND TRY AGAIN                                
*                                                                               
BRS08    MVC   TFMMSG,WRK                                                       
         LH    RF,DISPSLN          BUMP TO NEXT DISPLAY LINE                    
         AH    RF,DISPNLR                                                       
         STH   RF,DISPSLN                                                       
*                                                                               
         MVC   PSSV,TP.PS02                                                     
         MVI   TP.PS02+0,IORSEQ    SET TO READ SEQUENTIAL                       
         MVI   TP.PS02+1,2                                                      
         MVI   TP.PS02+2,2                                                      
*                                                                               
         CLI   TF.FTORG,FTOIS      I/S FILE                                     
         BE    *+8                 YES                                          
         MVI   TP.PS02+2,1                                                      
*                                                                               
         BRAS  RE,DISKIO           READ NEXT RECORD                             
         BNE   EXITL                                                            
*                                                                               
         MVC   TP.PS02,PSSV        RESTORE SAVED PERMBLK ENTRY                  
         B     BRS02                                                            
*                                                                               
BROWSEX  MVI   HDRN,1                                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD IN ELEMENT FORMAT                                    *         
***********************************************************************         
         SPACE 1                                                                
ELEMENT  NTR1  ,                                                                
         XC    DISPSLN,DISPSLN                                                  
         XC    DISPSB,DISPSB       FIRST DISPLAY KEY/CONT/SAVE                  
*                                                                               
         CLI   TF.FTLOG,FTLRCV                                                  
         BNE   EDI04                                                            
         L     R3,ARCVTAB                                                       
         LA    RF,IOAREA                                                        
*                                                                               
EDI00    CLI   0(R3),EOT                                                        
         BNE   *+12                                                             
         MVI   FERN,36                                                          
         B     EXITL                                                            
*                                                                               
         CLC   0(1,RF),0(R3)                                                    
         BE    *+12                                                             
         AHI   R3,L'RCVTAB                                                      
         B     EDI00                                                            
*                                                                               
         MVC   TF.FTORG(9),1(R3)                                                
         MVC   DISPDL,=AL2(24)     SAVED LENGTH                                 
         MVI   DISPOP,X'0E'                                                     
         BRAS  RE,DISP                                                          
*                                                                               
         LH    RF,DISPSLN                                                       
         AH    RF,DISPNLR                                                       
         STH   RF,DISPSLN          BUMP DISPLAY START LINE                      
         MVC   DISPSB,=AL2(24)                                                  
*                                                                               
         CLI   TF.FTYPE,FTVLE      VLE FILE INSIDE RECOVERY HEADER?             
         BNE   EDI02               YES                                          
         XR    RF,RF                                                            
         IC    RF,TF.FTKEYL                                                     
         AHI   RF,24                                                            
         LA    RF,IOAREA(RF)       SEE IF THIS IS CLIENT RECORD                 
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,0(RF)                                                       
         AHI   R0,24                                                            
         CLM   R0,3,SLRL                                                        
         BH    *+8                                                              
         STCM  R0,3,SLRL                                                        
*                                                                               
*&&UK*&& B     EDI04                                                            
         CLI   0(R3),X'21'         SPTFILE                                      
         BNE   EDI04                                                            
         LA    RF,IOAREA+24        SEE IF THIS IS CLIENT RECORD                 
         CLI   0(RF),0                                                          
         BNE   EDI04                                                            
         MVI   THSELA,0            RESET THIS FOR SCROLL                        
*                                                                               
         OC    STIB,STIB                                                        
         BNZ   *+10                                                             
         MVC   STIB,=AL2(24)       SET DISP TO HEADER                           
         XR    RF,RF                                                            
         ICM   RF,3,STIB                                                        
         MVC   DISPSB,STIB                                                      
         XR    RF,RF                                                            
         ICM   RF,3,STIL                                                        
         BNZ   *+8                                                              
         ICM   RF,3,TF.FTMAXL1                                                  
         AHI   RF,24                                                            
         SH    RF,STIB                                                          
         AHI   RF,1                                                             
         STH   RF,DISPDL                                                        
         MVI   DISPOP,X'0F'                                                     
         BRAS  RE,DISP                                                          
         MVI   THSELA,0            FIX FOR SCROLLING                            
         B     EXITOK                                                           
*                                                                               
EDI02    LH    RF,SLRL             JUST OUTPUT RECORD AS IS                     
         AHI   RF,-24                                                           
         STCM  RF,3,DISPDL         SET LENGTH                                   
         MVI   DISPOP,X'0F'                                                     
         BRAS  RE,DISP                                                          
         B     EXITOK                                                           
*                                                                               
EDI04    XR    R5,R5                                                            
         IC    R5,TF.FTKEYL                                                     
         XR    R0,R0                                                            
         IC    R0,TF.FTCNTL                                                     
         AR    R5,R0                                                            
         IC    R0,TF.FTSYSL                                                     
         AR    R5,R0               1ST EL = L'KEY + L'CTL + L'SYS               
         STH   R5,SLEFRST                                                       
         AH    R5,DISPSB                                                        
*                                                                               
         LR    R4,R5               R1=EL LEN COUNTER                            
         LA    R5,IOAREA(R5)       R5=A(FIRST EL)                               
*                                                                               
         CLI   THSELA,EIAF         FI.. ELEMENT FOUND                           
         BE    EDI14                                                            
*                                                                               
EDI06    CLI   THSELA,EIAL         WANT LA                                      
         BNE   EDI08                                                            
         CLI   0(R5),0                                                          
         BE    EDI14               LA.. ELEMENT FOUND                           
*                                                                               
EDI08    CLI   THSELA,EIAS         WANT S,..                                    
         BNE   EDI10                                                            
         LR    RE,R5                                                            
         LA    R0,IOAREA                                                        
         SR    RE,R0                                                            
         CH    RE,STIE                                                          
         BE    EDI14               S,.. ELEMENT FOUND                           
*                                                                               
EDI10    CLI   THSELA,6            WANT I,..                                    
         BNE   EDI12                                                            
         CLC   STIEL,1(R5)         IGNORE IF EL LT INPUT LEN                    
         BH    EDI12                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,STIEL            COMPARE FOR INPUT LEN                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    EDI14               I,.. ELEMENT FOUND                           
         CLC   0(0,R5),STIE                                                     
*                                                                               
EDI12    CLI   0(R5),0             END OF RECORD?                               
         BNE   *+12                                                             
         MVI   FERN,08             ERROR ELEMENT NOT FOUND                      
         B     EXITL                                                            
*                                                                               
         CLI   1(R5),2             ELEMENT LENGTH IS OK?                        
         BNL   *+12                YES                                          
         MVI   FERN,52                                                          
         B     EXITL                                                            
*                                                                               
         XR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R4,R0               BUMP ELEMENT LENGTH COUNTER                  
*                                                                               
         LH    R3,SLRL                                                          
         LA    R3,1(R3)                                                         
         CR    R4,R3                                                            
         BNH   *+12                                                             
         MVI   FERN,52                                                          
         B     EXITL                                                            
*                                                                               
         AR    R5,R0                                                            
         B     EDI06                                                            
*                                                                               
EDI14    ST    R5,FULL1            SAVE ADDR OF FOUND EL                        
*                                                                               
EDI16    CLI   0(R5),0                                                          
         BE    EDI18                                                            
*                                                                               
         CLI   1(R5),2             ELEMENT LENGTH IS OK?                        
         BNL   *+12                YES                                          
         MVI   FERN,52                                                          
         B     EXITL                                                            
*                                                                               
         XR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R4,R0               BUMP ELEMENT LENGTH COUNTER                  
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,TF.FTMAXL1                                                  
         AHI   R1,1                                                             
         CR    R4,R1                                                            
         BNH   *+12                                                             
         MVI   FERN,52             ELEMENTS GO OVER MAX RECORD LENGTH           
         B     EXITL                                                            
*                                                                               
         AR    R5,R0                                                            
         B     EDI16                                                            
*                                                                               
EDI18    CLM   R4,3,SLRL           END OF ELS FOUND                             
         BE    EDI20                                                            
         AHI   R4,1                                                             
         CLM   R4,3,SLRL                                                        
         BE    EDI20                                                            
         MVI   FERN,52             ELEMENTS AND LENGTH DON'T MATCH              
         B     EXITL                                                            
*                                                                               
EDI20    L     R5,FULL1            GO DO DISPLAY OF ELEMENTS                    
         LA    R0,IOAREA                                                        
         SR    R5,R0                                                            
         STH   R5,SLESTRT                                                       
         BRAS  RE,ELDIS                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ELEMENTS                                         *         
***********************************************************************         
         SPACE 1                                                                
ELDIS    NTR1  ,                                                                
         MVC   DISPDL,SLEFRST      SAVED LENGTH                                 
         MVI   DISPOP,X'0E'                                                     
         BRAS  RE,DISP                                                          
*                                                                               
         LA    R5,IOAREA                                                        
         AH    R5,SLESTRT                                                       
*                                                                               
ELD02    CLI   0(R5),0             END OF RECORD                                
         BNE   ELD04                                                            
         LHI   RF,1                DISPLAY 1 BYTE (X'00') FOR EOR               
         STH   RF,DISPDL                                                        
         B     ELD06                                                            
*                                                                               
ELD04    XR    RF,RF               SET ELEMENT LENGTH                           
         IC    RF,1(R5)                                                         
         CHI   RF,2                FIX BAD ELEMENTS                             
         BH    *+8                                                              
         LHI   RF,2                                                             
         STH   RF,DISPDL                                                        
*                                                                               
ELD06    LH    RF,DISPSLN                                                       
         AH    RF,DISPNLR                                                       
         STH   RF,DISPSLN          BUMP DISPLAY START LINE                      
*                                                                               
         LR    R0,R5                                                            
         LA    R1,IOAREA                                                        
         SR    R0,R1                                                            
         STH   R0,DISPSB           DISPLACEMENT TO THIS ELEMENT                 
*                                                                               
ELD08    BRAS  RE,DISP             GO DISPLAY ELEMENT                           
*                                                                               
         CLI   0(R5),0             JUST DID EOR?                                
         BE    EXITOK              YES                                          
*                                                                               
         CLI   1(R5),2             FUNNY ELEMENT?                               
         BNL   *+12                                                             
         MVI   FERN,52             WARNING INVALID ELEMENT                      
         B     EXITL                                                            
*                                                                               
ELD10    TM    DISPOP,DOPART                                                    
         BO    EXITOK                                                           
*                                                                               
         CLI   DISPRES,DRPART      DID IT FIT?                                  
         BNE   ELD34               YES                                          
         TM    DISPOP,DOSOME       DID WE DO PART OF IT?                        
         BO    EXITOK              YES                                          
*                                                                               
         OI    DISPOP,DOPART                                                    
         B     ELD08               GO ROUND AND TRY TO PUT A BIT OUT            
*                                                                               
ELD34    XR    RF,RF               NEXT ELEMENT                                 
         ICM   RF,1,1(R5)                                                       
         AR    R5,RF                                                            
         MVI   DISPOP,X'0E'        DISPLAY WHOLE ELEMENT                        
         B     ELD02                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DOES ALL DISK I/O VIA DATA MANAGER                     *         
***********************************************************************         
         SPACE 1                                                                
DISKIO   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB,DMCB                                                        
         LA    RF,TP.PS01                                                       
         CLI   DISKIOOP,0                                                       
         BE    DSK04                                                            
         LA    RF,TP.PS02                                                       
         CLI   DISKIOOP,1                                                       
         BE    DSK04                                                            
         DC    H'0'                                                             
*                                                                               
DSK04    ST    RF,ADKNTRY                                                       
         LR    R3,RF                                                            
         USING IOACTD,R3                                                        
         XR    RE,RE                                                            
         IC    RE,IOACMD                                                        
         BCTR  RE,0                                                             
         MHI   RE,L'DMCMDS                                                      
         LA    RE,DMCMDS(RE)                                                    
         ST    RE,DMCB1            SET A(COMMAND)                               
         MVI   DMCB,X'09'          RETURN DELETES & RECORD LENGTH               
*                                                                               
         CLC   =C'DMR',0(RE)       CLEAR IOAREA BEFORE READ                     
         BE    DSK06                                                            
         CLC   =C'GET',0(RE)                                                    
         BE    DSK06                                                            
         B     DSK08                                                            
*                                                                               
DSK06    LA    R0,IOAREA                                                        
         L     R1,=A(IOAREAL)                                                   
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
DSK08    LA    RE,TF.FTNAME       SET A(FILE NAME)                              
         ST    RE,DMCB2                                                         
*                                                                               
         LA    RE,STIK            NOW SET KEY OR DISK ADDRESS                   
         CLI   IOABEF,1                                                         
         BE    DSK10                                                            
         LA    RE,SLIK                                                          
         CLI   IOABEF,2                                                         
         BE    DSK10                                                            
         ICM   RE,15,STIK                                                       
         CLI   IOABEF,3                                                         
         BE    DSK10                                                            
         DC    H'0'                                                             
*                                                                               
DSK10    ST    RE,DMCB3                                                         
*                                                                               
         LA    RE,IOAREA                                                        
         ST    RE,DMCB4                                                         
         LA    R6,DMWORK                                                        
         ST    RE,DMCB5                                                         
*                                                                               
         XC    DMCB6,DMCB6                                                      
         MVC   DMCB6+2(2),SLRL     SET V/L TO LAST READ LEN                     
         CLI   TF.FTYPE,FTFL                                                    
         BNE   DSK12                                                            
         XR    R0,R0                                                            
         ICM   R0,3,TF.FTMAXL1     SET F/L TO MAX LENGTH                        
         AHI   R0,1                                                             
         STH   R0,DMCB6+2                                                       
*                                                                               
DSK12    CLC   =CL8'TEMPSTR',TF.FTNAME                                          
         BNE   DSK14                                                            
         MVC   DMCB6(2),=C'L='                                                  
         MVC   DMCB6+2(2),TWAL                                                  
         B     DSK16                                                            
*                                                                               
DSK14    CLC   =CL8'TEMPEST',TF.FTNAME                                          
         BNE   DSK16                                                            
         MVC   DMCB6(2),=C'L='                                                  
         MVC   DMCB6+2(2),TMSL                                                  
*                                                                               
DSK16    GOTO1 ADATAMGR,DMCB       NOW GO DO THE ACTUAL I/O                     
*                                                                               
         TM    DMCB3,X'FC'                                                      
         BC    5,DSKERR            EOF/ERR/DKEY/NOTF/PHL/SECL                   
*                                                                               
         CLI   TF.FTSTRT,FTSTRTNO  RECORD LEN IN RECORD                         
         BE    DSK18               NO                                           
         XR    RF,RF               YES SET IN DMCB                              
         IC    RF,TF.FTSTRT                                                     
         LA    RF,IOAREA(RF)                                                    
         MVC   DMCB6+2(2),0(RF)                                                 
*                                                                               
DSK18    XR    RE,RE               SET READ INDICATOR + LENGTH                  
         IC    RE,IOAAFT                                                        
         AHI   RE,1                                                             
         STC   RE,SLRI                                                          
         MVC   SLRL,DMCB6+2        SAVE RECORD LENGTH                           
         MVC   SLRF,TF.FTNAME                                                   
*                                                                               
         MVI   HDRN,0              NOW GO OUTPUT RL=XXX,RA=XXX MSG              
         XR    R0,R0                                                            
         ICM   R0,3,SLRL                                                        
         XC    WRK,WRK                                                          
         MVC   WRK(3),=C'RL='                                                   
*                                                                               
         CLI   DTYPE,DTHEX                                                      
         BE    DSK20                                                            
         EDIT  (R0),(5,WRK+3),0,ALIGN=LEFT,ZERO=NOBLANK                         
         B     DSK22                                                            
*                                                                               
DSK20    LHI   R0,2                IF HEX DISPLAY, SHOW AS HEX LENGTH           
         LA    RF,SLRL                                                          
         CLI   SLRL,0                                                           
         BNE   *+12                                                             
         LHI   R0,1                                                             
         LA    RF,SLRL+1                                                        
         GOTO1 AHEXOUT,PLIST,(RF),WRK+3,(R0),0                                  
*                                                                               
DSK22    CLI   IOAAFT,0            SET KEY AS INSTRUCTED                        
         BE    EXITOK                                                           
         CLI   IOAAFT,1            D/A TO OUTPUT?                               
         BNE   DSK24               NO                                           
*                                                                               
         L     RF,DMCB3            HEXOUT D/A                                   
         LHI   R0,4                                                             
         MVC   WRK+9(3),=C'DA='                                                 
         GOTO1 AHEXOUT,PLIST,(RF),WRK+12,(R0),0                                 
*                                                                               
         L     RF,DMCB3            SET A(D/A)                                   
         LHI   RE,4                SET L'D/A                                    
         B     DSK30                                                            
*                                                                               
DSK24    CLI   IOAAFT,2                                                         
         BNE   DSK26                                                            
         LA    RF,IOAREA                                                        
         XR    RE,RE                                                            
         IC    RE,TF.FTKEYL                                                     
         B     DSK30                                                            
*                                                                               
DSK26    CLI   IOAAFT,3                                                         
         BNE   DSK28                                                            
         LA    RF,STIK                                                          
         SR    RE,RE                                                            
         IC    RE,STIKL                                                         
         B     DSK30                                                            
*                                                                               
DSK28    DC    H'0'                                                             
*                                                                               
DSK30    LA    R0,SLIK             RF=A(KEY/ADR) RE=L'KEY/ADR                   
         CR    RF,R0                                                            
         BE    *+10                                                             
         XC    SLIK,SLIK                                                        
         STC   RE,SLIKL                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SLIK(0),0(RF)                                                    
         B     EXITOK                                                           
*                                                                               
DSKERR   MVI   SLRI,0                                                           
         MVI   FERN,29                                                          
         TM    DMCB3,X'40'         DISKERROR                                    
         BNZ   EXITL                                                            
         MVI   FERN,30                                                          
         TM    DMCB3,X'80'         EOF                                          
         BNZ   EXITL                                                            
         MVI   FERN,31                                                          
         TM    DMCB3,X'10'         NOTFOUND                                     
         BNZ   EXITL                                                            
         MVI   FERN,32                                                          
         TM    DMCB3,X'2C'         DUPKEYADD/PHYSLOCK/SECLOCK                   
         BNZ   EXITL                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE DISPLAYS AS MUCH DATA OF THE RECORD IN IOAREA THAT WILL FIT *         
* ON SCREEN STARTING AT DISPLAY LINE NUM DISPSLN                      *         
* NTRY: DISPSLN  = DISPLAY LINE NUMBER                                *         
*       DISPOP   = BITS - X'01'=1 DISP PART , X'01'=0 DISP WHOLE      *         
*                         X'02'=1 DISP HEX                            *         
*                         X'04'=1 DISP BYTES                          *         
*                         X'08'=1 DISP CHARS                          *         
***********************************************************************         
         SPACE 1                                                                
DISP     NTR1  BASE=*,LABEL=*                                                   
         LH    RF,VWDISP           SET WIDTH                                    
         STH   RF,DISPLW                                                        
         LH    RF,VNDISP           SET NUMBER OF LINES                          
         STH   RF,DISPNL                                                        
         MVI   DISPRES,DRALL       SET DISPLAY ALL                              
*                                                                               
         LH    R0,DISPDL           LENGTH OF DATA TO BE DISPLAYED               
         SRDL  R0,32                                                            
         LH    RF,VWDISP           DIVIDED BY WIDTH                             
         DR    R0,RF                                                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1                                                             
         STH   R1,DISPNLR          R1=# LINES TO FULLY DISPLAY DATA             
*                                                                               
         LH    R0,DISPSLN          START LINE                                   
         AR    R0,R1                                                            
         CH    R0,DISPNL           WILL DATA FIT?                               
         BNH   DISP02              YES                                          
*                                                                               
         MVI   DISPRES,DRPART      SET DISPLAY ONLY PART                        
         TM    DISPOP,DOPART       ALLOWED TO DO THIS?                          
         BZ    DISPX               NO                                           
*                                                                               
         LH    RF,DISPNL           ONLY DO WHAT WILL FIT                        
         SH    RF,DISPSLN                                                       
         STH   RF,DISPNLR          SET NEW LINES REQ TO REM                     
         MH    RF,DISPLW                                                        
         STH   RF,DISPDL           SET NEW DISPLAY LENGTH                       
         LTR   RF,RF                                                            
         BZ    DISPX                                                            
*                                                                               
DISP02   OI    DISPOP,DOSOME       SET DATA DISPLAYED FLAG                      
         LHI   R4,DLINEL                                                        
         MH    R4,DISPSLN                                                       
         LA    R4,TFEL1OH(R4)      R4=A(DISPLAY LINE IN TWA)                    
         USING DLINED,R4                                                        
*                                                                               
         LH    R3,DISPSB           CALLER SETS DISPSB                           
         LA    R3,IOAREA(R3)       R3=A(DATA IN IOAREA)                         
*                                                                               
         MVC   DISPCT,DISPDL       COUNT OF RESIDUAL BYTES                      
         LH    R5,DISPNLR          R5=LOOP COUNTER                              
*                                                                               
DISP04   CHI   R5,1                                                             
         BNE   *+10                                                             
         MVC   DISPLW,DISPCT       PARTIAL DISPLAY FOR LAST LINE                
*                                                                               
         TM    DISPOP,DOHEX        DISPLAYING HEX?                              
         BZ    DISP06              NO                                           
         LA    R2,ODHH                                                          
         USING FHD,R2                                                           
         LH    R0,DISPLW                                                        
         GOTO1 AHEXOUT,DMCB,(R3),FHDA,(R0),0                                    
*                                                                               
DISP06   TM    DISPOP,DOBYTES      DISPLAYING BYTES?                            
         BZ    DISP10              NO                                           
         LA    R2,ODBH                                                          
         USING FHD,R2                                                           
*                                                                               
         LH    R0,DISPSB           SET UP SSSSS-FFFFF                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  FHDA(5),DUB                                                      
         MVI   FHDA+5,C' '                                                      
*                                                                               
         CLI   DTYPE,DTHEX         DISPLAYING HEX?                              
         BNE   DISP08              NO                                           
         ST    R0,FULL                                                          
         GOTO1 AHEXOUT,DMCB,FULL,DUB,L'FULL,0                                   
         MVC   FHDA(5),DUB+3                                                    
*                                                                               
DISP08   CLC   DISPCT,DISPDL       FIRST LINE?                                  
         BNE   DISP10              NO                                           
         OI    FHAT,FHATHI         SET HIGH INTENSITY                           
         MVI   FHII,FHIXAT                                                      
         MVI   FHIL,7              SET IT WHITE                                 
*                                                                               
         MVI   FHDA+5,C'-'         SET 1ST LINE FULL                            
         CLI   DISPRES,DRPART      PARTIAL DISPLAY?                             
         BNE   DISP10              NO                                           
         MVI   FHDA+5,C'*'         SET 1ST LINE PARTIAL                         
*                                                                               
DISP10   LH    R0,DISPSB           SET DISPSB TO END OF LINE                    
         AH    R0,DISPLW           R6=L'DISPLAY LINE DATA                       
         BCTR  R0,0                                                             
         STH   R0,DISPSB                                                        
*                                                                               
         TM    DISPOP,DOBYTES                                                   
         BZ    DISP14                                                           
*                                                                               
         CVD   R0,DUB              SET SECOND PART OF DISPLAY                   
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  FHDA+6(5),DUB                                                    
*                                                                               
         CLI   DTYPE,DTHEX         DISPLAYING HEX?                              
         BNE   DISP12              NO                                           
         ST    R0,FULL                                                          
         GOTO1 AHEXOUT,DMCB,FULL,DUB,L'FULL,0                                   
         MVC   FHDA+6(5),DUB+3                                                  
*                                                                               
DISP12   MVI   FHOL,11                                                          
         OI    FHOI,FHOITR                                                      
*                                                                               
DISP14   TM    DISPOP,DOCHARS      DISPLAYING CHARACTERS                        
         BZ    DISP16              NO                                           
         LA    R2,ODCH                                                          
         USING FHD,R2                                                           
         LH    RF,DISPLW                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),0(R3)       MOVE OUT                                     
         L     R1,ADISPTBL                                                      
         EX    RF,DISPTR           TRANSLATE                                    
         AHI   RF,1                                                             
         STC   RF,FHOL                                                          
         OI    FHOI,FHOITR                                                      
         B     DISP16                                                           
*                                                                               
DISPTR   TR    FHDA(0),0(R1)                                                    
*                                                                               
DISP16   LH    R0,DISPSB           UP DISPSB TO NEXT LINE                       
         AHI   R0,1                                                             
         STH   R0,DISPSB                                                        
         LH    R0,DISPCT           DOWN RESIDUAL COUNT                          
         SH    R0,DISPLW                                                        
         STH   R0,DISPCT                                                        
*                                                                               
         AH    R3,DISPLW           UP IOAREA POINTER                            
         AHI   R4,DLINEL                                                        
         BCT   R5,DISP04           NEXT LINE                                    
*                                                                               
DISPX    B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ TWA 11                                                         *         
***********************************************************************         
         SPACE 1                                                                
READTWA  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,SRPAGENO         SET UP UNIQUE KEY FOR TWA 11                 
         SLL   R0,32-8                                                          
         L     RF,AUTL                                                          
         ICM   R0,3,TNUM-UTLD(RF)                                               
         MVC   DMCB6(2),=C'L='                                                  
         MVC   DMCB6+2(2),TWAL                                                  
*                                                                               
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),=CL8'TEMPSTR',(R0),ATIA             
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
*                                                                               
*                                                                               
         L     R2,ATIA                                                          
         USING SRSD,R2                                                          
         LA    R7,SR$DUMP          POINT TO THE SAVED DATA                      
         ST    R7,SAVEAREA         SAVE THE ADDRESS FOR LATER                   
*                                                                               
         MVI   SCRLOK,YES          ALLOWED TO SCROLL                            
         CLC   =C'$TFM',TFMWORD                                                 
         BE    EXITOK              NO                                           
         MVI   SCRLOK,NO           NOT ALLOWED TO SCROLL                        
*                                                                               
         LR    R0,R7               CLEAR SAVE AREA                              
         LHI   R1,TFMSAVEL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   TFMWORD,=C'$TFM'                                                 
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE TWA 11                                                        *         
***********************************************************************         
         SPACE 1                                                                
WRTTWA   NTR1  BASE=*,LABEL=*                                                   
         MVC   SFILTAB,TFILTAB                                                  
         MVC   SPERMTAB,TPERMTAB                                                
         MVI   BEFORE,YES                                                       
         MVC   SSTIB,STIB          SAVE VALUES                                  
         MVC   SSTIL,STIL                                                       
         MVC   STFMRACT,TFMRACT                                                 
*                                                                               
         LHI   R0,SRPAGENO         SET UP UNIQUE KEY FOR TWA 11                 
         SLL   R0,32-8                                                          
         L     RF,AUTL                                                          
         ICM   R0,3,TNUM-UTLD(RF)                                               
         MVC   DMCB6(2),=C'L='                                                  
         MVC   DMCB6+2(2),TWAL                                                  
         GOTO1 ADATAMGR,DMCB,(X'00',DMWRT),=CL8'TEMPSTR',(R0),ATIA              
         CLI   8(R1),0                                                          
         BE    EXITOK              SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* HELP DISPLAY ROUTINE FOR FILE NAME FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
FNHELP   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,(X'FD',TFMLOADH),0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,TFDL1H                                                        
         USING FHD,R3                                                           
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
         LHI   R0,L'FTNAME+2                                                    
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0               FIND OUT HOW MANY FIT ON A LINE              
         STH   RF,FULL+2           FULL+2(2) = NUMBER ON A LINE                 
*                                                                               
         L     R2,AFILTAB                                                       
H        USING FILTABD,R2                                                       
*                                                                               
FNH02    LA    R0,TFDPFKH          MAKE SURE IT STILL FITS ON SCREEN            
         CR    R0,R3                                                            
         BNH   FNH06                                                            
*                                                                               
         LH    R0,FULL+2           LOOP ROUND SETTING INFO ON LINES             
         LA    RF,FHDA                                                          
*                                                                               
FNH04    XR    R1,R1                                                            
         ICM   R1,3,H.FTDISP                                                    
         BZ    FNH06                                                            
         MVC   0(L'FTNAME,RF),H.FTNAME                                          
         AR    R2,R1                                                            
         AH    RF,FULL                                                          
         BCT   R0,FNH04                                                         
*                                                                               
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         B     FNH02                                                            
         DROP  H                                                                
*                                                                               
FNH06    CLI   PFK,2               'SELECT' CURSOR PRESSED                      
         BE    FNH08                YES                                         
         LA    R3,TFDL1H                                                        
         ST    R3,FADRH                                                         
         MVI   FERN,0                                                           
         MVI   HDRN,2                                                           
         B     EXITL                                                            
*                                                                               
FNH08    LA    R3,TFDL1H                                                        
         CLC   SCURSOR,FHAD                                                     
         BL    FNH14                                                            
         LA    R3,TFDPFKH                                                       
         CLC   SCURSOR,FHAD                                                     
         BNL   FNH14                                                            
*                                                                               
         LA    R3,TFDL1H           SET FIRST ADDRESS                            
         ST    R3,FULL1                                                         
         LA    RF,TFDPFKH                                                       
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
FNH10    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    FNH12                                                            
         ST    R3,FULL1                                                         
         IC    RE,FHLN                                                          
         BXLE  R3,RE,FNH10                                                      
         DC    H'0'                                                             
*                                                                               
FNH12    L     R3,FULL1            CURSOR FIELD IS IN FULL1                     
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         LH    RE,SCURSOR                                                       
         SR    RE,RF               GET DISP INTO FIELD                          
         SRDL  RE,32                                                            
         LH    R1,FULL                                                          
         DR    RE,R1                                                            
         MH    RF,FULL             GET 1ST CHARACTER OF THIS ENTRY              
         LA    RF,FHDA(RF)                                                      
*                                                                               
         AHI   R1,-3               REMEMBER THE LENGTH+2 ABOVE                  
         EX    R1,*+8                                                           
         BNH   FNH14                                                            
         CLC   0(0,RF),SPACES                                                   
         MVC   TFMFILE,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TFMFILE(0),0(RF)                                                 
         LA    R3,TFMFILEH                                                      
         ST    R3,FADRH                                                         
         AHI   R1,1                                                             
         STC   R1,FHIL                                                          
         B     EXITH               SHOW SELECT DONE                             
*                                                                               
FNH14    MVI   FERN,45             INVALID CURSOR POSITION                      
         B     EXITL                                                            
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* HELP DISPLAY ROUTINE FOR RECORD ID FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
RIHELP   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,(X'FD',TFMLOADH),0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WSS,WSS             BUILD LIST OF ACTIONS IN WSS                 
         L     RF,AFTNTRY                                                       
         AHI   RF,FILTABL                                                       
         USING PERMTABD,RF                                                      
         LA    R1,WSS                                                           
RIH00    CLI   PKEY,0                                                           
         BE    RIH01                                                            
         MVC   0(L'PKEY,R1),PKEY                                                
         AHI   R1,L'PKEY                                                        
         AHI   RF,PERMTABL                                                      
         B     RIH00                                                            
*                                                                               
RIH01    BRAS  RE,SORT             NOW REMOVE DUPES (SWAP FOR FF)               
*                                                                               
         LA    R3,TFDL1H                                                        
         USING FHD,R3                                                           
         LA    R4,WSS                                                           
*                                                                               
RIH02    CLI   0(R4),0             END OF VALID LIST                            
         BE    RIH06                                                            
*                                                                               
         CLI   0(R4),X'FF'         SKIP THIS VALUE?                             
         BNE   *+12                NO                                           
         AHI   R4,1                                                             
         B     RIH02                                                            
*                                                                               
         LA    R0,TFDPFKH          MAKE SURE VALUE FITS ON SCREEN               
         CR    R0,R3                                                            
         BNH   RIH06                                                            
*                                                                               
         LA    R2,RIDTBL                                                        
         USING KEYTBLD,R2                                                       
RIH04    CLI   KEYTNAME,EOT        END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEYTACT,0(R4)       MATCH ACTION?                                
         BE    *+12                YES                                          
         AHI   R2,KEYTBLL                                                       
         B     RIH04                                                            
*                                                                               
         MVC   FHDA(L'KEYTNAME),KEYTNAME                                        
         MVC   FHDA+L'KEYTNAME+2(L'KEYTEXT),KEYTEXT                             
*                                                                               
         AHI   R4,1                NEXT VALUE IN LIST                           
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         B     RIH02                                                            
*                                                                               
RIH06    CLI   PFK,2               'SELECT' CURSOR PRESSED                      
         BE    RIH08                                                            
         LA    R3,TFDL1H                                                        
         ST    R3,FADRH                                                         
         MVI   FERN,0                                                           
         MVI   HDRN,2                                                           
         B     EXITOK                                                           
*                                                                               
RIH08    LA    R3,TFDL1H                                                        
         CLC   SCURSOR,FHAD                                                     
         BL    RIH14                                                            
         LA    R3,TFDPFKH                                                       
         CLC   SCURSOR,FHAD                                                     
         BNL   RIH14                                                            
*                                                                               
         LA    R3,TFDL1H           SET FIRST ADDRESS                            
         ST    R3,FULL1                                                         
         LA    RF,TFDPFKH                                                       
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
RIH10    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    RIH12                                                            
         ST    R3,FULL1                                                         
         IC    RE,FHLN                                                          
         BXLE  R3,RE,RIH10                                                      
         DC    H'0'                                                             
*                                                                               
RIH12    L     R3,FULL1            CURSOR FIELD IS IN FULL1                     
         LHI   R1,L'KEYTNAME-1                                                  
         EX    R1,*+8                                                           
         BNH   RIH14                                                            
         CLC   FHDA(0),SPACES                                                   
         MVC   TFMRID,SPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TFMRID(0),FHDA                                                   
         LA    R3,TFMRIDH                                                       
         ST    R3,FADRH                                                         
         AHI   R1,1                                                             
         STC   R1,FHIL                                                          
         B     EXITH               SHOW SELECT DONE                             
*                                                                               
RIH14    MVI   FERN,45             INVALID CURSOR POSITION                      
         B     EXITL                                                            
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SORT ROUTINE FOR VALUES IN WSS (LENGTH 1)                           *         
***********************************************************************         
         SPACE 1                                                                
SORT     NTR1  BASE=*,LABEL=*                                                   
         LA    R1,WSS                                                           
         LA    RF,WSS                                                           
SRT02    CLI   0(R1),0             END OF SORT?                                 
         BE    SRT10               YES                                          
*                                                                               
SRT04    CLI   0(RF),0             END OF PASS                                  
         BE    SRT06               YES                                          
         CLC   0(1,RF),0(R1)       LOWER VALUE FOUND?                           
         BL    SRT08               YES                                          
         AHI   RF,L'PKEY                                                        
         B     SRT04                                                            
*                                                                               
SRT06    AHI   R1,L'PKEY           NEXT IN SORT                                 
         LR    RF,R1                                                            
         B     SRT02                                                            
*                                                                               
SRT08    XC    0(1,R1),0(RF)       SWAP THE TWO ENTRIES                         
         XC    0(1,RF),0(R1)                                                    
         XC    0(1,R1),0(RF)                                                    
         LR    RF,R1               AND CONTINUE                                 
         B     SRT04                                                            
*                                                                               
SRT10    LA    R1,WSS              NOW REMOVE DUPES (SET TO FF)                 
*                                                                               
SRT12    CLI   0(R1),0                                                          
         BE    EXITOK                                                           
         CLC   0(1,R1),1(R1)                                                    
         BNE   *+8                                                              
         MVI   0(R1),X'FF'                                                      
         AHI   R1,1                                                             
         B     SRT12                                                            
         EJECT                                                                  
***********************************************************************         
* HELP DISPLAY ROUTINE FOR RECORD ACTION FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
RAHELP   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,(X'FD',TFMLOADH),0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WSS,WSS             BUILD LIST OF ACTIONS IN WSS                 
         L     RF,AFTNTRY                                                       
         AHI   RF,FILTABL                                                       
         USING PERMTABD,RF                                                      
         LA    R1,WSS                                                           
RAH02    CLI   PKEY,0                                                           
         BE    RAH04                                                            
         CLC   PKEY,THSKEYN                                                     
         BNE   *+14                                                             
         MVC   0(L'PACT,R1),PACT                                                
         AHI   R1,L'PACT                                                        
         AHI   RF,PERMTABL                                                      
         B     RAH02                                                            
         DROP  RF                                                               
*                                                                               
RAH04    LA    R3,TFDL1H                                                        
         USING FHD,R3                                                           
         LA    R4,WSS                                                           
*                                                                               
RAH06    CLI   0(R4),0             END OF VALID ACTION LIST                     
         BE    RAH10                                                            
         LA    R0,TFDPFKH          MAKE SURE VALUE FITS ON SCREEN               
         CR    R0,R3                                                            
         BNH   RAH10                                                            
*                                                                               
         LA    R2,RACTTBL                                                       
         USING ACTNTBLD,R2                                                      
RAH08    CLI   ACTNNAME,EOT        END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   ACTNNUM,0(R4)       MATCH ACTION?                                
         BE    *+12                YES                                          
         AHI   R2,ACTNTBLL                                                      
         B     RAH08                                                            
*                                                                               
         MVC   FHDA(L'ACTNNAME),ACTNNAME                                        
         AHI   R4,1                NEXT VALUE IN LIST                           
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         B     RAH06                                                            
         DROP  R2                                                               
*                                                                               
RAH10    CLI   PFK,2               'SELECT' CURSOR PRESSED                      
         BE    RAH12                                                            
         LA    R3,TFDL1H                                                        
         ST    R3,FADRH                                                         
         MVI   FERN,0                                                           
         MVI   HDRN,2                                                           
         B     EXITOK                                                           
*                                                                               
RAH12    LA    R3,TFDL1H                                                        
         CLC   SCURSOR,FHAD                                                     
         BL    RAH18                                                            
         LA    R3,TFDPFKH                                                       
         CLC   SCURSOR,FHAD                                                     
         BNL   RAH18                                                            
*                                                                               
         LA    R3,TFDL1H           SET FIRST ADDRESS                            
         ST    R3,FULL1                                                         
         LA    RF,TFDPFKH                                                       
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
RAH14    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    RAH16                                                            
         ST    R3,FULL1                                                         
         IC    RE,FHLN                                                          
         BXLE  R3,RE,RAH14                                                      
         DC    H'0'                                                             
*                                                                               
RAH16    L     R3,FULL1            CURSOR FIELD IS IN FULL1                     
         LHI   R1,L'ACTNNAME-1                                                  
         EX    R1,*+8                                                           
         BNH   RAH18                                                            
         CLC   FHDA(0),SPACES                                                   
         MVC   TFMRACT,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TFMRACT(0),FHDA                                                  
         LA    R3,TFMRACTH                                                      
         ST    R3,FADRH                                                         
         AHI   R1,1                                                             
         MVI   FHIL,3                                                           
         B     EXITH               SHOW SELECT DONE                             
*                                                                               
RAH18    MVI   FERN,45             INVALID CURSOR POSITION                      
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* HANDY ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
LITERALS DS    0D               <= ADDRESSING POINT FOR R9                      
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
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND EQUATES                                                *         
***********************************************************************         
         SPACE 1                                                                
EOT      EQU   0                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
ACTDIS   EQU   1                   ACTION EQUATES                               
ACTBRO   EQU   4                                                                
*                                                                               
EMSGL    EQU   48                  LENGTH OF AN ERROR MESSAGE                   
IMSGL    EQU   45                  LENGTH OF AN OK MESSAGE                      
*                                                                               
IOADD    EQU   1                                                                
IOREAD   EQU   2                                                                
IORSEQ   EQU   3                                                                
IOHIGH   EQU   4                                                                
IODEL    EQU   5                                                                
IOWRT    EQU   6                                                                
IORDIR   EQU   7                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
IOKEY    EQU   1                                                                
IOADDR   EQU   3                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
WDISPH   DC    H'16'               DISPLAY CHR WIDTH                            
WDISP    DC    H'20'               DISPLAY CHR WIDTH                            
NDISP    DC    H'16'               NUM OF DISPLAY LINES                         
*                                                                               
ZEROS    DC    8C'0'                                                            
EFFS     DC    8X'FF'                                                           
SPACES   DC    80C' '                                                           
*                   12345678901234567890                                        
PF04     DC    CL20'PF04      Toggle    '                                       
PF06     DC    CL20'PF06      Nxt Record'                                       
PF07     DC    CL20'PF07      Page Up   '                                       
PF08     DC    CL20'PF08      Page Down '                                       
*                                                                               
HEXHDR   DC    CL40'0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9.'                   
HEXHDRH  DC    CL40'0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.        '                   
DECHDR   DC    CL20'0.2.4.6.8.0.2.4.6.8.'                                       
DECHDRH  DC    CL20'0.2.4.6.8.A.C.E.    '                                       
         EJECT                                                                  
***********************************************************************         
* I/O COMMAND EQUATES                                                 *         
***********************************************************************         
         SPACE 1                                                                
DMCMDS   DS    0CL8                EQUATES IOXXX ARE INDEX TO TABLE             
         DC    CL8'DMADD '                                                      
DMREAD   DC    CL8'DMREAD'                                                      
         DC    CL8'DMRSEQ'                                                      
         DC    CL8'DMRDHI'                                                      
         DC    CL8'DMDEL '                                                      
DMWRT    DC    CL8'DMWRT '                                                      
         DC    CL8'DMRDIR'                                                      
         EJECT                                                                  
***********************************************************************         
* TABLE ADDRESSES                                                     *         
***********************************************************************         
         SPACE 1                                                                
AERRMSGS DC    A(ERRMSGS)                                                       
AOKMSGS  DC    A(OKMSGS)                                                        
         EJECT                                                                  
***********************************************************************         
* ACTION TABLES FOR RECORD ACTION AND ELEMENT ACTION FIELDS           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
EACTTBL  DC    C'DISPLAY',AL1(ACTDIS)                                           
         DC    AL1(EOT)                                                         
*                                                                               
RACTTBL  DC    C'DISPLAY',AL1(ACTDIS)                                           
         DC    C'BROWSE ',AL1(ACTBRO)                                           
         DC    AL1(EOT)                                                         
*                                                                               
ACTNTBLD DSECT                                                                  
ACTNNAME DS    CL7                                                              
ACTNNUM  DS    X                                                                
ACTNTBLL EQU   *-ACTNTBLD                                                       
*                                                                               
TFM00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* KEY TABLES                                                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F   12345678901234567890123456789012                            
RIDTBL   DC    C'K,',AL1(KYAK,00),AL4(RIDK)                                     
         DC    CL32'Uses DECODE (Same as PFM) - For '                           
         DC    CL32'TEMPSTR, must be 00-11 or 81-95 '                           
         DC    C'A,',AL1(KYAA,00),AL4(RIDA)                                     
         DC    CL32'TTTTBBNN 8 byte hex format. TTTT'                           
         DC    CL32' + BB must both be >1           '                           
         DC    C'FI',AL1(KYAF,00),AL4(RIDF)                                     
         DC    CL32'Finds first record on the file  '                           
         DC    CL32'                                '                           
         DC    C'NE',AL1(KYAN,00),AL4(RIDN)                                     
         DC    CL32'Finds next record (browse) - Mus'                           
         DC    CL32't be preceeded by prior read    '                           
         DC    C'I=',AL1(KYAI,00),AL4(RIDI)                                     
         DC    CL32'Finds first record for this test'                           
         DC    CL32' id.                            '                           
         DC    C'ME',AL1(KYAM,00),AL4(RIDM)                                     
         DC    CL32'FINDS FIRST RECORD FOR YOUR TEST'                           
         DC    CL32' id.                            '                           
         DC    AL1(EOT)                                                         
*                                                                               
KYAK     EQU   1                                                                
KYAA     EQU   2                                                                
KYAF     EQU   3                                                                
KYAN     EQU   4                                                                
KYAS     EQU   5                                                                
KYAI     EQU   6                                                                
KYAM     EQU   7                                                                
*                                                                               
EIAS     EQU   5                                                                
EIAI     EQU   6                                                                
EIAF     EQU   3                                                                
EIAL     EQU   7                                                                
EIAN     EQU   4                                                                
*                                                                               
         DS    0F                                                               
EIDTBL   DC    C'S,',AL1(EIAS,00),AL4(EIDS)                                     
         DC    CL32'                                '                           
         DC    CL32'                                '                           
         DC    C'I,',AL1(EIAI,00),AL4(EIDI)                                     
         DC    CL32'                                '                           
         DC    CL32'                                '                           
         DC    C'FI',AL1(EIAF,00),AL4(0)                                        
         DC    CL32'                                '                           
         DC    CL32'                                '                           
         DC    C'NE',AL1(EIAN,00),AL4(0)                                        
         DC    CL32'                                '                           
         DC    CL32'                                '                           
         DC    C'FU',AL1(00,00),AL4(0)                                          
         DC    CL32'                                '                           
         DC    CL32'                                '                           
EIDTBLX  DC    AL1(EOT)                                                         
*                                                                               
KEYTBLD  DSECT                                                                  
KEYTNAME DS    CL2   FIRST TWO CHRS OF NAME                                     
KEYTACT  DS    XL1   NUMBER                                                     
KEYTVAL  DS    XL1   N/D                                                        
KEYTRTN  DS    AL4   A(EXTRA VALIDATION ROUTINE)                                
KEYTEXT  DS    CL64  HELP TEXT                                                  
KEYTBLL  EQU   *-KEYTBLD                                                        
*                                                                               
TFM00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS TABLE CONTAINS HEADER MSGS INDEXED BY HDRN                     *         
***********************************************************************         
         SPACE 1                                                                
OKMSGS   DS    0CL(IMSGL)                                                       
OK1      DC    CL(IMSGL)'Action completed'                                      
OK2      DC    CL(IMSGL)'Help displayed. select with PFK or type name'          
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES INDEXED BY FERN                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'Missing file name - enter ''?'' for help'              
ERR02    DC    CL(EMSGL)'Invalid file name - enter ''?'' for help'              
ERR03    DC    CL(EMSGL)'Missing record id - enter ''?'' for help'              
ERR04    DC    CL(EMSGL)'Invalid record id - enter ''?'' for help'              
ERR05    DC    CL(EMSGL)'Input syntax must be recognised by SCANNER'            
ERR06    DC    CL(EMSGL)'Input only valid for Display and Change action*        
               s'                                                               
ERR07    DC    CL(EMSGL)'Value entered must be valid HEX'                       
ERR08    DC    CL(EMSGL)'Element not found'                                     
ERR09    DC    CL(EMSGL)'Disk address must be 8 digit and valid HEX'            
ERR10    DC    CL(EMSGL)'TTTT in D/A must be 0001 or higher'                    
ERR11    DC    CL(EMSGL)'No previous record for file'                           
ERR12    DC    CL(EMSGL)'Invalid start value'                                   
ERR13    DC    CL(EMSGL)'Value entered must be a valid number'                  
ERR14    DC    CL(EMSGL)'Invalid end value'                                     
ERR15    DC    CL(EMSGL)'End value must be zero'                                
ERR16    DC    CL(EMSGL)'Start value is greater than end value'                 
ERR17    DC    CL(EMSGL)'End value is greater than record length'               
ERR18    DC    CL(EMSGL)'Start value is greater than record length'             
ERR19    DC    CL(EMSGL)'Start value must be zero'                              
ERR20    DC    CL(EMSGL)'Missing end value             '                        
ERR21    DC    CL(EMSGL)'End value too large           '                        
ERR22    DC    CL(EMSGL)'End value is not equal to record length-1'             
ERR23    DC    CL(EMSGL)'End value is too small'                                
ERR24    DC    CL(EMSGL)'No TSTTAB entry defined for this Facpak'               
ERR25    DC    CL(EMSGL)'Invalid action name'                                   
ERR26    DC    CL(EMSGL)'No TSTTAB entry matches this input'                    
ERR27    DC    CL(EMSGL)'This file type does not have elements'                 
ERR28    DC    CL(EMSGL)'Input too long                '                        
ERR29    DC    CL(EMSGL)'Disk Error                    '                        
ERR30    DC    CL(EMSGL)'EOF - No record found         '                        
ERR31    DC    CL(EMSGL)'Record not found'                                      
ERR32    DC    CL(EMSGL)'Contention Lockout            '                        
ERR33    DC    CL(EMSGL)'Record already exists'                                 
ERR34    DC    CL(EMSGL)'You need to enter a valid test id'                     
ERR35    DC    CL(EMSGL)'This PFKey is not valid at this time'                  
ERR36    DC    CL(EMSGL)'I don''t know about this record type'                  
ERR37    DC    CL(EMSGL)'SPARESPARESPARESPARESPARESPARE'                        
ERR38    DC    CL(EMSGL)'SPARESPARESPARESPARESPARESPARE'                        
ERR39    DC    CL(EMSGL)'SPARESPARESPARESPARESPARESPARE'                        
ERR40    DC    CL(EMSGL)'SPARESPARESPARESPARESPARESPARE'                        
ERR41    DC    CL(EMSGL)'SPARESPARESPARESPARESPARESPARE'                        
ERR42    DC    CL(EMSGL)'SPARESPARESPARESPARESPARESPARE'                        
ERR43    DC    CL(EMSGL)'Invalid element id code'                               
ERR44    DC    CL(EMSGL)'Key is X''PP00TTTT'' - PP is Page and TTTT is *        
               Terminal'                                                        
ERR45    DC    CL(EMSGL)'Invalid position for cursor'                           
ERR46    DC    CL(EMSGL)'You need to enter a valid disk address'                
ERR47    DC    CL(EMSGL)'BB in D/A must be 01 or higher'                        
ERR48    DC    CL(EMSGL)'You need to enter a valid key'                         
ERR49    DC    CL(EMSGL)'Input invalid for this file. Enter ''?''for he*        
               lp'                                                              
ERR50    DC    CL(EMSGL)'Record Id/Action combo invalid enter ''?'' for*        
               help'                                                            
ERR51    DC    CL(EMSGL)'Too many input parameters'                             
ERR52    DC    CL(EMSGL)'*WARNING* Invalid Element in record'                   
ERR53    DC    CL(EMSGL)'TTTTT in D/A must be 00001 or higher'                  
ERR54    DC    CL(EMSGL)'B in D/A must be 0'                                    
ERR55    DC    CL(EMSGL)'RR in D/A must be 01 or higher'                        
         EJECT                                                                  
***********************************************************************         
* THIS TABLE DEFINES THE DISPLAYABLE CHARACTERS                       *         
***********************************************************************         
         SPACE 1                                                                
DISPTBLL DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  00-0F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  10-1F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  20-2F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  30-3F                    
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'  40-4F                    
         DC    XL16'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'  60-6F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4B8182838485868788894B4B4B4B4B4B'  80-8F                    
         DC    XL16'4B9192939495969798994B4B4B4B4B4B'  90-9F                    
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B'  A0-AF                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  D0-D1                    
         DC    XL16'E04BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  F0-FF                    
         EJECT                                                                  
***********************************************************************         
* FILE TABLE                                                          *         
***********************************************************************         
         SPACE 1                                                                
FILTAB   DS    0F                                                               
*                                                                               
ADRFILE  DC    AL2(ADRFILEX-ADRFILE)   ADRFILE FILE                             
         DC    CL8'ADRFILE '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(06399)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAA,ACTBRO)        ACTN = A,/BRO                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAF,ACTDIS)        ACTN = FI/DIS                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAF,ACTBRO)        ACTN = FI/BRO                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
ADRFILEX EQU   *                                                                
*=====================================================================*         
CTFILE   DC    AL2(CTFILEX-CTFILE)     KWXFILE                                  
         DC    CL8'CTFILE  '                                                    
         DC    AL1(FTOIS,FTLZERO,FTVLE)                                         
         DC    AL2(01999)                                                       
         DC    AL1(25,03,00,25,24,00)                                           
*                                                                               
         DC    AL1(KYAK,ACTDIS)        ACTN = K,/DIS                            
         DC    AL1(04,01,02),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAK,ACTBRO)        ACTN = K,/BRO                            
         DC    AL1(04,01,02),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAF,ACTDIS)        ACTN = FI/DIS                            
         DC    AL1(04,01,02),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAF,ACTBRO)        ACTN = FI/BRO                            
         DC    AL1(04,01,02),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAN,ACTBRO)        ACTN = NE/BRO                            
         DC    AL1(02,02,02),AL1(03,02,02)                                      
*                                                                               
         DC    AL1(KYAN,ACTDIS)        ACTN = NE/BRO                            
         DC    AL1(02,02,02),AL1(03,02,02)                                      
*                                                                               
         DC    AL1(EOT)                                                         
CTFILEX  EQU   *                                                                
*=====================================================================*         
DMPFILE  DC    AL2(DMPFILEX-DMPFILE)   DUMP FILE                                
         DC    CL8'DMPFILE '                                                    
         DC    AL1(FTODA,FTLZERO,FTVL)                                          
         DC    AL2(08191)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(EOT)                                                         
DMPFILEX EQU   *                                                                
*=====================================================================*         
EASIWK   DC    AL2(EASIWKX-EASIWK)     EASIWK FILE                              
         DC    CL8'EASIWK  '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(03659)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
EASIWKX  EQU   *                                                                
*=====================================================================*         
EDCTA    DC    AL2(EDCTAX-EDCTA)       EDCTA FILE                               
         DC    CL8'EDCTA   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(18431)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAA,ACTBRO)        ACTN = A,/BRO                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(EOT)                                                         
EDCTAX   EQU   *                                                                
*=====================================================================*         
EDCTR    DC    AL2(EDCTRX-EDCTR)       EDCTR FILE                               
         DC    CL8'EDCTR   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(18431)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAA,ACTBRO)        ACTN = A,/BRO                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(EOT)                                                         
EDCTRX   EQU   *                                                                
*=====================================================================*         
FACWRK   DC    AL2(FACWRKX-FACWRK)     FACWRK FILE                              
         DC    CL8'FACWRK  '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
*&&UK*&& DC    AL2(03659)                                                       
*&&US*&& DC    AL2(06139)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
FACWRKX  EQU   *                                                                
*=====================================================================*         
KWXFILE  DC    AL2(KWXFILEX-KWXFILE)   KWXFILE                                  
         DC    CL8'KWXFILE '                                                    
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(00999)                                                       
         DC    AL1(04,00,00,00,03,00)                                           
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAA,ACTBRO)        ACTN = A,/BRO                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAF,ACTDIS)        ACTN = FI/DIS                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAF,ACTBRO)        ACTN = FI/BRO                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
KWXFILEX EQU   *                                                                
*=====================================================================*         
PRGMS    DC    AL2(PRGMSX-PRGMS)       PRGMS FILE                               
         DC    CL8'PRGMS   '                                                    
         DC    AL1(FTODA,FTLZERO,FTVL)                                          
         DC    AL2(56663)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,IOKEY,01),AL1(00,00,00)                               
*                                                                               
         DC    AL1(EOT)                                                         
PRGMSX   EQU   *                                                                
*=====================================================================*         
PRTQ1    DC    AL2(PRTQ1X-PRTQ1)       PRTQ1 FILE                               
         DC    CL8'PRTQ1   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ1X   EQU   *                                                                
*=====================================================================*         
PRTQ2    DC    AL2(PRTQ2X-PRTQ2)       PRTQ2 FILE                               
         DC    CL8'PRTQ2   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ2X   EQU   *                                                                
*=====================================================================*         
PRTQ3    DC    AL2(PRTQ3X-PRTQ3)       PRTQ3 FILE                               
         DC    CL8'PRTQ3   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ3X   EQU   *                                                                
*=====================================================================*         
PRTQ4    DC    AL2(PRTQ4X-PRTQ4)       PRTQ4 FILE                               
         DC    CL8'PRTQ4   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ4X   EQU   *                                                                
*=====================================================================*         
PRTQ5    DC    AL2(PRTQ5X-PRTQ5)       PRTQ5 FILE                               
         DC    CL8'PRTQ5   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ5X   EQU   *                                                                
*=====================================================================*         
PRTQ6    DC    AL2(PRTQ6X-PRTQ6)       PRTQ6 FILE                               
         DC    CL8'PRTQ6   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ6X   EQU   *                                                                
*=====================================================================*         
PRTQ7    DC    AL2(PRTQ7X-PRTQ7)       PRTQ7 FILE                               
         DC    CL8'PRTQ7   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ7X   EQU   *                                                                
*=====================================================================*         
PRTQ8    DC    AL2(PRTQ8X-PRTQ8)       PRTQ8 FILE                               
         DC    CL8'PRTQ8   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ8X   EQU   *                                                                
*=====================================================================*         
PRTQ9    DC    AL2(PRTQ9X-PRTQ9)       PRTQ9 FILE                               
         DC    CL8'PRTQ9   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQ9X   EQU   *                                                                
*=====================================================================*         
PRTQA    DC    AL2(PRTQAX-PRTQA)       PRTQA FILE                               
         DC    CL8'PRTQA   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQAX   EQU   *                                                                
*=====================================================================*         
PRTQB    DC    AL2(PRTQBX-PRTQB)       PRTQB FILE                               
         DC    CL8'PRTQB   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQBX   EQU   *                                                                
*=====================================================================*         
PRTQC    DC    AL2(PRTQCX-PRTQC)       PRTQC FILE                               
         DC    CL8'PRTQC   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQCX   EQU   *                                                                
*=====================================================================*         
PRTQD    DC    AL2(PRTQDX-PRTQD)       PRTQD FILE                               
         DC    CL8'PRTQD   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQDX   EQU   *                                                                
*=====================================================================*         
PRTQE    DC    AL2(PRTQEX-PRTQE)       PRTQE FILE                               
         DC    CL8'PRTQE   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
PRTQEX   EQU   *                                                                
*=====================================================================*         
PRTQF    DC    AL2(PRTQFX-PRTQF)       PRTQF FILE                               
         DC    CL8'PRTQF   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQFX   EQU   *                                                                
*=====================================================================*         
PRTQG    DC    AL2(PRTQGX-PRTQG)       PRTQG FILE                               
         DC    CL8'PRTQG   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
PRTQGX   EQU   *                                                                
*=====================================================================*         
STATS    DC    AL2(STATSX-STATS)       STATS FILE                               
         DC    CL8'STATS   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(2415)                                                        
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAA,ACTBRO)        ACTN = A,/BRO                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAF,ACTDIS)        ACTN = FI/DIS                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAF,ACTBRO)        ACTN = FI/BRO                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
STATSX   EQU   *                                                                
*=====================================================================*         
TEMPSTR  DC    AL2(TEMPSTRX-TEMPSTR)   TEMPSTR FILE                             
         DC    CL8'TEMPSTR '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(14335)                                                       
         DC    AL1(04,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAK,ACTDIS)        ACTN = K,/DIS                            
         DC    AL1(02,03,00),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
TEMPSTRX EQU   *                                                                
*=====================================================================*         
TEMPEST  DC    AL2(TEMPESTX-TEMPEST)   TEMPEST FILE                             
         DC    CL8'TEMPEST '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(14335)                                                       
         DC    AL1(04,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAK,ACTDIS)        ACTN = K,/DIS                            
         DC    AL1(02,03,00),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(EOT)                                                         
TEMPESTX EQU   *                                                                
*=====================================================================*         
TSTRCVR  DC    AL2(TSTRCVRX-TSTRCVR)   RECOVERY FILE                            
         DC    CL8'TSTRCVR '                                                    
         DC    AL1(FTODA,FTLRCV,FTVL)                                           
         DC    AL2(04095)                                                       
         DC    AL1(24,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAA,ACTBRO)        ACTN = A,/BRO                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAF,ACTDIS)        ACTN = FI/DIS                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAF,ACTBRO)        ACTN = FI/BRO                            
         DC    AL1(03,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(KYAI,ACTDIS)        ACTN = I=/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(KYAM,ACTDIS)        ACTN = ME/DIS                            
         DC    AL1(IORDIR,01,01),AL1(00,00,00)                                  
*                                                                               
         DC    AL1(EOT)                                                         
TSTRCVRX EQU   *                                                                
*=====================================================================*         
WKFILE   DC    AL2(WKFILEX-WKFILE)     WKFILE FILE                              
         DC    CL8'WKFILE  '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(03659)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WKFILEX  EQU   *                                                                
*=====================================================================*         
WRKF1    DC    AL2(WRKF1X-WRKF1)       WRKF1 FILE                               
         DC    CL8'WRKF1   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF1X   EQU   *                                                                
*=====================================================================*         
WRKF2    DC    AL2(WRKF2X-WRKF2)       WRKF2 FILE                               
         DC    CL8'WRKF2   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF2X   EQU   *                                                                
*=====================================================================*         
WRKF3    DC    AL2(WRKF3X-WRKF3)       WRKF3 FILE                               
         DC    CL8'WRKF3   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF3X   EQU   *                                                                
*=====================================================================*         
WRKF4    DC    AL2(WRKF4X-WRKF4)       WRKF4 FILE                               
         DC    CL8'WRKF4   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF4X   EQU   *                                                                
*=====================================================================*         
WRKF5    DC    AL2(WRKF5X-WRKF5)       WRKF5 FILE                               
         DC    CL8'WRKF5   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF5X   EQU   *                                                                
*=====================================================================*         
WRKF6    DC    AL2(WRKF6X-WRKF6)       WRKF6 FILE                               
         DC    CL8'WRKF6   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF6X   EQU   *                                                                
*=====================================================================*         
WRKF7    DC    AL2(WRKF7X-WRKF7)       WRKF7 FILE                               
         DC    CL8'WRKF7   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF7X   EQU   *                                                                
*=====================================================================*         
WRKF8    DC    AL2(WRKF8X-WRKF8)       WRKF8 FILE                               
         DC    CL8'WRKF8   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF8X   EQU   *                                                                
*=====================================================================*         
WRKF9    DC    AL2(WRKF9X-WRKF9)       WRKF9 FILE                               
         DC    CL8'WRKF9   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKF9X   EQU   *                                                                
*=====================================================================*         
WRKFA    DC    AL2(WRKFAX-WRKFA)       WRKFA FILE                               
         DC    CL8'WRKFA   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKFAX   EQU   *                                                                
*=====================================================================*         
WRKFB    DC    AL2(WRKFBX-WRKFB)       WRKFB FILE                               
         DC    CL8'WRKFB   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKFBX   EQU   *                                                                
*=====================================================================*         
WRKFC    DC    AL2(WRKFCX-WRKFC)       WRKFC FILE                               
         DC    CL8'WRKFC   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKFCX   EQU   *                                                                
*=====================================================================*         
WRKFD    DC    AL2(WRKFDX-WRKFD)       WRKFD FILE                               
         DC    CL8'WRKFD   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKFDX   EQU   *                                                                
*=====================================================================*         
WRKFE    DC    AL2(WRKFEX-WRKFE)       WRKFE FILE                               
         DC    CL8'WRKFE   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKFEX   EQU   *                                                                
*=====================================================================*         
WRKFF    DC    AL2(WRKFFX-WRKFF)       WRKFF FILE                               
         DC    CL8'WRKFF   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKFFX   EQU   *                                                                
*=====================================================================*         
WRKFG    DC    AL2(WRKFGX-WRKFG)       WRKFG FILE                               
         DC    CL8'WRKFG   '                                                    
         DC    AL1(FTODA,FTLZERO,FTFL)                                          
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKFGX   EQU   *                                                                
*=====================================================================*         
WRKZ1    DC    AL2(WRKZ1X-WRKZ1)       WRKZ1 FILE                               
         DC    CL8'WRKZ1   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ1X   EQU   *                                                                
*=====================================================================*         
WRKZ2    DC    AL2(WRKZ2X-WRKZ2)       WRKZ2 FILE                               
         DC    CL8'WRKZ2   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ2X   EQU   *                                                                
*=====================================================================*         
WRKZ3    DC    AL2(WRKZ3X-WRKZ3)       WRKZ3 FILE                               
         DC    CL8'WRKZ3   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ3X   EQU   *                                                                
*=====================================================================*         
WRKZ4    DC    AL2(WRKZ4X-WRKZ4)       WRKZ4 FILE                               
         DC    CL8'WRKZ4   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ4X   EQU   *                                                                
*=====================================================================*         
WRKZ5    DC    AL2(WRKZ5X-WRKZ5)       WRKZ5 FILE                               
         DC    CL8'WRKZ5   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ5X   EQU   *                                                                
*=====================================================================*         
WRKZ6    DC    AL2(WRKZ6X-WRKZ6)       WRKZ6 FILE                               
         DC    CL8'WRKZ6   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ6X   EQU   *                                                                
*=====================================================================*         
WRKZ7    DC    AL2(WRKZ7X-WRKZ7)       WRKZ7 FILE                               
         DC    CL8'WRKZ7   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ7X   EQU   *                                                                
*=====================================================================*         
WRKZ8    DC    AL2(WRKZ8X-WRKZ8)       WRKZ8 FILE                               
         DC    CL8'WRKZ8   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ8X   EQU   *                                                                
*=====================================================================*         
WRKZ9    DC    AL2(WRKZ9X-WRKZ9)       WRKZ9 FILE                               
         DC    CL8'WRKZ9   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZ9X   EQU   *                                                                
*=====================================================================*         
WRKZA    DC    AL2(WRKZAX-WRKZA)       WRKZA FILE                               
         DC    CL8'WRKZA   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZAX   EQU   *                                                                
*=====================================================================*         
WRKZB    DC    AL2(WRKZBX-WRKZB)       WRKZB FILE                               
         DC    CL8'WRKZB   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZBX   EQU   *                                                                
*=====================================================================*         
WRKZC    DC    AL2(WRKZCX-WRKZC)       WRKZC FILE                               
         DC    CL8'WRKZC   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZCX   EQU   *                                                                
*=====================================================================*         
WRKZD    DC    AL2(WRKZDX-WRKZD)       WRKZD FILE                               
         DC    CL8'WRKZD   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZDX   EQU   *                                                                
*=====================================================================*         
WRKZE    DC    AL2(WRKZEX-WRKZE)       WRKZE FILE                               
         DC    CL8'WRKZE   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZEX   EQU   *                                                                
*=====================================================================*         
WRKZF    DC    AL2(WRKZFX-WRKZF)       WRKZF FILE                               
         DC    CL8'WRKZF   '                                                    
         DC    AL1(FTODA,FTL20,FTFL)                                            
         DC    AL2(13679)                                                       
         DC    AL1(00,00,00,FTSTRTNO,03,00)                                     
*                                                                               
         DC    AL1(KYAA,ACTDIS)        ACTN = A,/DIS                            
         DC    AL1(02,01,01),AL1(00,00,00)                                      
*                                                                               
         DC    AL1(EOT)                                                         
WRKZFX   EQU   *                                                                
*=====================================================================*         
FILTABX  DC    AL2(EOT)                                                         
*                                                                               
FILTABD  DSECT                                                                  
FTDISP   DS    AL2                 DISP TO NEXT FILE   00 = EOT                 
FTNAME   DS    CL8                 NAME                                         
FTORG    DS    XL1                 TYPE ORGANISATION                            
FTOSQ    EQU   1                    SEQ                                         
FTOIS    EQU   2                    I/S                                         
FTODA    EQU   3                    D/A                                         
FTODL    EQU   4                    DAL                                         
FTLOG    DS    XL1                 TYPE LOGICAL                                 
FTLZERO  EQU   0                                                                
FTLREQ   EQU   1                    REQUEST                                     
FTLRCV   EQU   2                    RECOVERY                                    
FTL20    EQU   3                    20BIT D/A                                   
FTYPE    DS    XL1                 RECORD TYPE                                  
FTFL     EQU   1                    F/L                                         
FTVL     EQU   2                    V/L                                         
FTVLE    EQU   3                    V/L/E                                       
FTMAXL1  DS    XL2                 MAX REC LEN - 1                              
FTKEYL   DS    XL1                 KEY LEN                                      
FTCNTL   DS    XL1                 CONTROL LENGTH                               
FTSYSL   DS    XL1                 SYSTEM SAVE LENGTH                           
FTSTRT   DS    XL1                 START BYTE FOR RECORD LENGTH                 
FTSTRTNO EQU   X'FF'                NOT IN RECORD                               
FT1STKY  DS    XL1                 1ST KEY BYTE NUM                             
FT1STKNO EQU   X'FF'                NO 1ST KEY SETTING                          
FTFILL   DS    XL1                 KEY FILL CHARACTER                           
FILTABL  EQU   *-FILTABD                                                        
*                                                                               
PERMTABD DSECT                                                                  
PKEY     DS    X                   KEY NUMBER                                   
PACT     DS    X                   ACTION NUMBER                                
*                                                                               
PS01     DS    XL3                                                              
PS02     DS    XL3                                                              
PERMTABL EQU   *-PERMTABD                                                       
*                                                                               
IOACTD   DSECT                                                                  
IOACMD   DS    X                   IO COMMAND TO DMGR                           
IOABEF   DS    X                   IO - WHAT TO DO FOR P3 BEFORE READ           
IOAAFT   DS    X                   IO - WHAT TO DO AFTER READ                   
IOACTL   EQU   *-IOACTD                                                         
*                                                                               
TFM00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FILE INFORMATION FOR RECOVERY FILE FILES                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
RCVTAB   DS    0XL10                                                            
*&&US                                                                           
RSPTFILE DC    X'21'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(3971)                                                        
         DC    AL1(13,03,08,13)                                                 
*                                                                               
RSTATION DC    X'22'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(116)                                                         
         DC    AL1(17,01,00,FTSTRTNO)                                           
*                                                                               
RSPTDIR  DC    X'23'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(0017)                                                        
         DC    AL1(13,01,00,FTSTRTNO)                                           
*                                                                               
RREQUEST DC    X'25'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RUNTDIR  DC    X'27'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(24)                                                          
         DC    AL1(20,01,04,FTSTRTNO)                                           
*                                                                               
RUNTFILE DC    X'2A'                                                            
         DC    AL1(FTODL,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(20,03,04,20)                                                 
*                                                                               
RTRFFILE DC    X'32'                                                            
         DC    AL1(FTODL,FTLZERO,FTVLE)                                         
         DC    AL2(3972)                                                        
         DC    AL1(13,03,08,13)                                                 
*                                                                               
RTRFDIR  DC    X'33'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(17)                                                          
         DC    AL1(13,01,00,FTSTRTNO)                                           
*                                                                               
RXSPDIR  DC    X'36'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(39)                                                          
         DC    AL1(32,04,00,FTSTRTNO)                                           
*                                                                               
RXSPFILE DC    X'37'                                                            
         DC    AL1(FTODL,FTLZERO,FTVLE)                                         
         DC    AL2(3971)                                                        
         DC    AL1(32,06,04,32)                                                 
*                                                                               
RPRTDIR  DC    X'40'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(30)                                                          
         DC    AL1(25,02,00,FTSTRTNO)                                           
*                                                                               
RPUBDIR  DC    X'41'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(30)                                                          
         DC    AL1(25,02,00,FTSTRTNO)                                           
*                                                                               
RPRTFILE DC    X'42'                                                            
         DC    AL1(FTODL,FTLZERO,FTVLE)                                         
         DC    AL2(3999)                                                        
         DC    AL1(25,04,04,25)                                                 
*                                                                               
RPUBFILE DC    X'43'                                                            
         DC    AL1(FTODL,FTLZERO,FTVLE)                                         
         DC    AL2(3999)                                                        
         DC    AL1(25,04,04,25)                                                 
*                                                                               
RPRQUEST DC    X'45'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RREPDIR  DC    X'81'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(31)                                                          
         DC    AL1(27,01,00,FTSTRTNO)                                           
*                                                                               
RREPFIL  DC    X'82'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(2000)                                                        
         DC    AL1(27,03,04,27)                                                 
*                                                                               
RREPREQ  DC    X'83'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RTALDIR  DC    X'71'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(37)                                                          
         DC    AL1(42,02,00,FTSTRTNO)                                           
*                                                                               
RTALFIL  DC    X'72'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(42,04,04,42)                                                 
*                                                                               
RTALREQ  DC    X'73'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RCHKDIR  DC    X'75'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(37)                                                          
         DC    AL1(42,02,00,FTSTRTNO)                                           
*                                                                               
RCHKFIL  DC    X'76'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(42,04,04,42)                                                 
*&&                                                                             
*&&UK                                                                           
RMEDDIR  DC    X'41'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(31)                                                          
         DC    AL1(20,08,00,FTSTRTNO)                                           
*                                                                               
RCHKFIL  DC    X'42'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(20,10,04,20)                                                 
*                                                                               
RREQUEST DC    X'43'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RPERDIR  DC    X'E1'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(41)                                                          
         DC    AL1(46,02,00,FTSTRTNO)                                           
*                                                                               
RPERFIL  DC    X'E2'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1023)                                                        
         DC    AL1(46,04,04,46)                                                 
*                                                                               
RPERREQ  DC    X'E3'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RDEMOLD  DC    X'A8'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(0999)                                                        
         DC    AL1(14,06,04,14)                                                 
*                                                                               
RDEMNEW  DC    X'A9'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(0999)                                                        
         DC    AL1(14,06,04,14)                                                 
*                                                                               
RDEMDIR  DC    X'AA'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(21)                                                          
         DC    AL1(14,04,00,FTSTRTNO)                                           
*&&                                                                             
RMPLREQ  DC    X'53'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RMPQFLA  DC    X'57'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(42,06,04,42)                                                 
*                                                                               
RMPRFLA  DC    X'59'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(42,06,04,42)                                                 
*                                                                               
RBUDFIL  DC    X'5B'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(42,06,04,42)                                                 
*                                                                               
RACCOUNT DC    X'61'                                                            
         DC    AL1(FTOIS,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(42,03,04,42)                                                 
*                                                                               
RACCREQ  DC    X'65'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RACCDAY  DC    X'66'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RACCDIR  DC    X'69'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(53)                                                          
         DC    AL1(42,08,00,FTSTRTNO)                                           
*                                                                               
RACCMST  DC    X'6A'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(42,10,04,42)                                                 
*                                                                               
RCTFILE  DC    X'A1'                                                            
         DC    AL1(FTOIS,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(25,03,00,25)                                                 
*                                                                               
RCTREQ   DC    X'A3'                                                            
         DC    AL1(FTODA,FTLREQ,FTFL)                                           
         DC    AL2(0105)                                                        
         DC    AL1(02,00,00,FTSTRTNO)                                           
*                                                                               
RGENDIR  DC    X'AE'                                                            
         DC    AL1(FTOIS,FTLZERO,FTFL)                                          
         DC    AL2(39)                                                          
         DC    AL1(32,04,00,FTSTRTNO)                                           
*                                                                               
RGENFILE DC    X'AF'                                                            
         DC    AL1(FTODA,FTLZERO,FTVLE)                                         
         DC    AL2(1999)                                                        
         DC    AL1(32,06,04,32)                                                 
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
TFM00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                   TEMP AREA                                    
FULL     DS    F                                                                
FULL1    DS    F                                                                
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
HALF     DS    H                                                                
HALF1    DS    H                                                                
*                                                                               
IPARMS   DS    0XL32               INPUT PARAMETERS                             
ASYSFAC  DS    A                   A(SYSFAC)                                    
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL)                                       
ACOMFAC  DS    A                   A(COMFACS)                                   
ASELIST  DS    A                   A(SELIST)                                    
ATWA     DS    A                   A(TWA)                                       
APHSMAP  DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR TWA DATA)                       
*                                                                               
DMCB     DS    0XL24               DATAMGR                                      
DMCB1    DS    F                                                                
DMCB2    DS    F                                                                
DMCB3    DS    F                                                                
DMCB4    DS    F                                                                
DMCB5    DS    F                                                                
DMCB6    DS    F                                                                
*                                                                               
PLIST    DS    XL24                EXTRA PARAMETER LIST                         
*                                                                               
ADATAMGR DS    A                   DATA MANAGER                                 
ACALLOV  DS    A                   CALL OVERLAY                                 
ATSTTAB  DS    A                   TSTTAB                                       
ASSB     DS    A                   SSB                                          
AHEXIN   DS    A                   HEXIN                                        
AHEXOUT  DS    A                   HEXOUT                                       
ADECODE  DS    A                   DECODE                                       
ASCANNER DS    A                   SCANNER                                      
AUNSCAN  DS    A                   UNSCAN                                       
*                                                                               
SAVEAREA DS    A                   SAVED STORAGE (IN TIA)                       
ADISPTBL DS    F                   TRANSLATOR                                   
*                                                                               
WSS      DS    CL60                                                             
WRK      DS    CL60                                                             
WORK     DS    CL20                                                             
PSSV     DS    XL(L'PS02)                                                       
*                                                                               
SYSNAME  DS    CL4                 SYSTEM NAME                                  
*                                                                               
AFILTAB  DS    A                   FILE TABLE ENTRY (FILTABD)                   
ARCVTAB  DS    A                                                                
AFTNTRY  DS    A                   A(FILTABD ENTRY FOR CURRENT FILE)            
ADKNTRY  DS    A                   A(IOACTD ENTRY FOR DISKIO ACTION)            
*                                                                               
TWAL     DS    H                   TEMPSTR RECORD LENGTH                        
TMPSTRMX DS    H                   TEMPSTR MAX NUMBER OF PAGES                  
TWCHKD   DS    H                   TEMPSTR CHECKPOINT DISPLACEMENT              
TWGLBD   DS    H                   TEMPSTR GLOBAL DISPLACEMENT                  
TMSL     DS    H                   TEMPEST RECORD LENGTH                        
*                                                                               
SCURSOR  DS    H                   DISPLACEMENT TO CURSOR IN TWA                
PFK      DS    X                   PFKEY PRESSED                                
SCRLOK   DS    X                   C'N' IF SCROLL TO BE SUPPRESSED              
*                                                                               
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
THSNAME  DS    CL8                 FILENAME                                     
THSKEYN  DS    CL1                 KEY NUM                                      
THSACT   DS    CL1                 ACTION                                       
THSELA   DS    CL1                 ELEMENT ACTION                               
*                                                                               
VWDISP   DS    H                   BYTES PER DISPLAY LINE                       
VNDISP   DS    H                   NUMBER OF DISPLAY LINES                      
*                                                                               
DISPOP   DS    X                   DISPLAY OPTION                               
DOPART   EQU   X'01'               DISPLAY PART ALLOWED                         
DOHEX    EQU   X'02'               DISPLAY HEX                                  
DOBYTES  EQU   X'04'               DISPLAY BYTES                                
DOCHARS  EQU   X'08'               DISPLAY CHARS                                
DOSOME   EQU   X'80'               DISPLAYED SOMETHING                          
*                                                                               
DISPRES  DS    X                   RESULT OF DISPLAY                            
DRPART   EQU   1                   PART DISPLAYED                               
DRALL    EQU   0                   ALL DISPLAYED                                
*                                                                               
DISKIOOP DS    X                   DISKIO - FIRST OR SECOND READ                
*                                                                               
DISPSLN  DS    H                   START LINE NUM (FIRST=0)                     
DISPNLR  DS    H                   NUM LINES DISPLAYED                          
DISPDL   DS    H                   NUM BYTES DISPLAYED                          
DISPNL   DS    H                   NUM LINES                                    
DISPLW   DS    H                   LINE WIDTH                                   
DISPSB   DS    H                   DISPLAY START BYTE                           
DISPCT   DS    H                   DISPLAY COUNTER                              
*                                                                               
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
STIKL    DS    CL1                 KEY LEN                                      
STIB     DS    H                   START BYTE                                   
STIL     DS    H                   END BYTE                                     
STIK     DS    CL48                KEY VALUE                                    
*                                                                               
STIE     DS    CL16                ELEMENT ID VALUE                             
STIEL    DS    CL1                 ELEMENT ID LENGTH TO MATCH                   
INHELP   DS    XL1                 HELP BEING DISPLAYED                         
*                                                                               
SCANBLK  DS    CL128               SCAN BLOCK                                   
USCANBLK DS    CL80                UNSCAN BLOCK                                 
*                                                                               
TFILTAB  DS    XL(FILTABL)         CURRENT FILE TABLE ENTRY                     
TPERMTAB DS    XL(PERMTABL)        PERMUTATION TABLE ENTRY                      
*                                                                               
DMWORK   DS    20D                 WORK AREA FOR DATAMGR                        
IOAREA   DS    56664C              MAXIMUM RECORD SIZE                          
IOAREAL  EQU   *-IOAREA                                                         
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECTS                                                       *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE FATWA                                                          
*SRTFMFFD                                                                       
       ++INCLUDE SRTFMFFD                                                       
         ORG   TFMLOADH                                                         
*SRTFMFED                                                                       
       ++INCLUDE SRTFMFED                                                       
         ORG   TFMLOADH                                                         
*SRTFMFDD                                                                       
       ++INCLUDE SRTFMFDD                                                       
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
TFMSAVED DSECT           SAVE STORAGE AND TWA SCREEN FIELDS                     
TFMWORD  DS    CL4       $TFM$ IF GOOD                                          
BEFORE   DS    C         C'Y' IF WE HAVE BEEN HERE BEFORE                       
DTYPE    DS    C         DISPLAY TYPE                                           
DTHEX    EQU   X'FF'                                                            
DTDEC    EQU   X'00'                                                            
SSTIB    DS    H         SAVED START BYTE                                       
SSTIL    DS    H         SAVED END BYTE                                         
STFMRACT DS    CL24                                                             
*                                                                               
SFILTAB  DS    XL(FILTABL)         LAST FILE TABLE ENTRY                        
SPERMTAB DS    XL(PERMTABL)        LAST PERMUTATION TABLE ENTRY                 
STSTACCS DS    XL256               SAVED TSTTAB DETAILS                         
*                                                                               
SLELINFO DS    0CL8                                                             
SLEACTN  DS    CL1       ELEMENT ACTION                                         
SLENL    DS    CL1       ELEMENT NEW LENGTH                                     
SLEID    DS    CL2       ELEMENT ID                                             
SLESTRT  DS    H         ELEMENT START BYTE                                     
SLEFRST  DS    H         ELEMENT START BYTE OF FIRST ELEMENT                    
*                                                                               
SLRIINFO DS    0CL11                                                            
SLRI     DS    CL1       LAST I/O INDICATOR                                     
SLRF     DS    CL8       LAST I/O FILE NAME                                     
SLRL     DS    H         LAST I/O RECORD LENGTH                                 
*                                                                               
SLIKL    DS    CL1       RECORD KEY LEN                                         
SLIRA    DS    CL1       RECORD ACTION                                          
SLIRNEW  DS    CL1       RECORD NEW LEN FLAG                                    
SLIB     DS    H         RECORD START BYTE                                      
SLIL     DS    H         RECORD END BYTE                                        
SLIK     DS    CL48      RECORD KEY VALUE                                       
*                                                                               
TFMSAVEL EQU   *-TFMSAVED                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER DISPLAY LINE - CORRESPONDS TO SRTFMFE                *         
***********************************************************************         
         SPACE 1                                                                
DLINED   DSECT           DISPLAY LINE FIELDS                                    
ODBH     DS    CL8                                                              
ODB      DS    CL11      START-END                                              
ODHH     DS    CL8                                                              
ODH      DS    CL40      HEX                                                    
ODCH     DS    CL8                                                              
ODC      DS    CL20      CHRS                                                   
DLINEL   EQU   *-DLINED                                                         
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
*FASYSFAC                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*FASSB                                                                          
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*DDFH                                                                           
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*FACHKPT                                                                        
         PRINT OFF                                                              
       ++INCLUDE FACHKPT                                                        
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*FATSTTAB                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATSTTAB                                                       
         PRINT ON                                                               
*FASRS                                                                          
         PRINT OFF                                                              
       ++INCLUDE FASRS                                                          
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRTFM00   11/08/10'                                      
         END                                                                    
