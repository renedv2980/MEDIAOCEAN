*          DATA SET NEPUP26    AT LEVEL 006 AS OF 05/01/02                      
*          DATA SET NEPUP26    AT LEVEL 089 AS OF 04/25/90                      
*PHASE T32226A,*                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'T32226 - PRB4 REPORT'                                           
T32226   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32226**,RA,R6,RR=R2                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   VAL2                                                             
         LA    RE,DATA          CLEAR SAVE AREA                                 
         LA    RF,TEMPEND-DATA                                                  
         XCEF                                                                   
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
VAL2     CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD FOR REPORTS                                      
         SPACE 3                                                                
VREC     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
**       MVC   8(7,R2),DPTNAME                                                  
**       OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         OC    PLANCODE,PLANCODE                                                
         BNZ   VRC2                                                             
         MVC   PLANSV,PUPPLAN                                                   
         SPACE 1                                                                
VRC2     LA    R2,PUPOPTH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         SPACE 1                                                                
         LA    R2,PUPQSTLH                                                      
         MVI   QTITLE,0                                                         
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
         MVC   TITLE,PUPQSTL                                                    
         MVI   QTITLE,1                                                         
         SPACE 1                                                                
VRECX    B     XIT                                                              
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
EDITOPT  NTR1                                                                   
         MVI   BOXOPT,C'Y'                                                      
         MVI   FIELDERR,1                                                       
         MVI   REQLEN,0                                                         
         MVI   REQRTFLG,0                                                       
         MVI   ROUND,C'Y'                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(3,R4),=C'LEN '   LENGTH FILTERING OPTION                      
         BNE   OPT6                                                             
         MVC   REQLEN,11(R4)                                                    
         CLI   REQLEN,1                                                         
         BL    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(1,R4),=C'Q'      QUARTER OPTION                               
         BNE   OPT8                                                             
         MVI   REQRTFLG,C'Y'                                                    
         MVC   REQUART,11(R4)                                                   
         CLI   REQUART,4           CHANGE 4TH QUARTER TO 0                      
         BNE   OPTEND                                                           
         MVI   REQUART,0                                                        
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(4,R4),=C'DATA'                                                
         BNE   OPT10                                                            
         MVI   DATA,1                                                           
         CLC   22(2,R4),=C'RI'                                                  
         BE    OPTEND                                                           
         MVI   DATA,2                                                           
         CLC   22(2,R4),=C'VR'                                                  
         BE    OPTEND                                                           
         MVI   DATA,3                                                           
         CLC   22(2,R4),=C'VI'                                                  
         BE    OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(5,R4),=C'ROUND'                                               
         BNE   OPTX                                                             
         MVC   ROUND,22(R4)                                                     
         CLI   ROUND,C'Y'                                                       
         BE    OPTEND                                                           
         CLI   ROUND,C'N'                                                       
         BE    OPTEND                                                           
OPTX     DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL REPORTS                                                  
         SPACE 3                                                                
PREP     NTR1                                                                   
*                                                                               
*                               * SET UP BINSRCH PARAMETERS                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         LA    R1,BUFF             A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,BINRLENE         LENGTH OF REC                                
         LA    R4,31               DISP OF KEY INTO REC                         
         L     R5,=F'175'        MAX RECS IN BINTBL                             
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         LA    RE,BUFF          CLEAR BINTABLE                                  
         L     RF,=F'6000'                                                      
         XCEF                                                                   
*                                                                               
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
*              HANDLE I/O FOR PLAN                                              
         SPACE                                                                  
         GOTO1 PUPIO,DMCB,PUPHOOK                                               
         B     PLANEND                                                          
*                                                                               
PUPHOOK  NTR1                                                                   
         CLI   PUPMODE,PROGMD                                                   
         BNE   PHX                                                              
         BAS   RE,POSTPROG                                                      
PHX      B     XIT                                                              
*                                                                               
         SPACE 1                                                                
PLANEND  DS    0H                  FLUSH DATA AREAS/PRINT                       
         CLI   DATA,0              .IF RTG/IMP/VPH REQUESTED                    
         BNE   PL5                                                              
         CLI   NDEMOS,4                                                         
         BNH   PL5                                                              
         MVI   NDEMOS,4            .MAXIMUM IS 4 DEMOS                          
PL5      BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         LA    R3,WORK                                                          
         USING BIND,R3                                                          
         XC    0(BINRLENE,R3),0(R3)                                             
         MVC   BINDAY,PROGDAYC                                                  
         MVC   BINTIME,PROGMIL                                                  
         MVC   BINPROG,PROGCODE                                                 
         MVC   BINDATA,KEY                                                      
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R3)),RR=RELO                             
         CLI   0(R1),X'01'        IF REC FOUND                                  
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT                                   
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*    STEP THROUGH BINTABLE AND RE-READ THE PROGRAM RECS, NOW                    
*    IN DAY/TIME ORDER                                                          
*                                                                               
PRINTIT  NTR1                                                                   
         USING BIND,R3                                                          
         LA    R3,BUFF                                                          
         OC    BINDATA,BINDATA                                                  
         BZ    XIT                                                              
PRT3     XC    KEY,KEY             GET PLAN FOR THE COMING PROG                 
         LA    R2,KEY                                                           
         USING NPLRECD,R2                                                       
         LA    R4,BINDATA                                                       
         USING NPURECD,R4                                                       
         MVI   NPLKTYPE,X'20'                                                   
         MVC   NPLKAM,NPUKAM                                                    
         MVC   NPLKCLT,NPUKCLT                                                  
         MVC   NPLKNET,NPUKNET                                                  
         MVC   NPLKDPT,NPUKDPT                                                  
         MVC   NPLKPLAN,NPUKPLAN                                                
         CLC   PLANKYSV,KEY                                                     
         BE    PRT3C                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PLANKYSV,KEY                                                     
         GOTO1 GETREC                                                           
         GOTO1 VEXTPLAN                                                         
         DROP  R2,R4                                                            
         CLI   DATA,0              .IF RTG/IMP/VPH REQUESTED                    
         BNE   PRT3C                                                            
         CLI   NDEMOS,4                                                         
         BNH   PRT3C                                                            
         MVI   NDEMOS,4            .MAXIMUM IS 4 DEMOS                          
*                                                                               
PRT3C    XC    KEY,KEY                                                          
         MVC   KEY(20),BINDATA                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   KEYSV,0             FIRST TIME                                   
         BNE   PRT4                                                             
         MVC   KEYSV,KEY           YES/SET KEYSV                                
         B     PRT5                                                             
*                                                                               
PRT4     CLC   KEYSV(9),KEY           SAME PROGRAM RECS                         
         BNE   PRT4C                                                            
         CLC   KEYSV+13(6),KEY+13                                               
         BE    PRT5                                                             
PRT4C    BAS   RE,PUTOP            NO/PRINT THE SAVE AREA                       
         LA    RE,TEMPSV              CLEAR IT                                  
         LA    RF,TEMPEND-TEMPSV                                                
         XCEF                                                                   
         MVC   KEYSV,KEY                                                        
*                                                                               
PRT5     GOTO1 VEXTPROG                                                         
         BAS   RE,DODETS                                                        
         LA    R3,BINRLENE(R3)                                                  
         OC    0(5,R3),0(R3)                                                    
         BNZ   PRT3                                                             
         BAS   RE,PUTOP                                                         
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
*                                                                               
DODETS   NTR1                                                                   
         ZIC   R2,PLANNLEN                                                      
         LA    R3,PLANLENS                                                      
         CLI   REQLEN,0                                                         
         BE    DD2                                                              
         MVC   LENGTH,REQLEN                                                    
         LA    R2,1                                                             
         B     *+10                                                             
DD2      MVC   LENGTH,0(R3)                                                     
         BAS   RE,GETDEMS          GETS DEMOS FOR LENGTH                        
         LA    R3,1(R3)                                                         
         BCT   R2,DD2                                                           
DDX      B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
* SETS DEMOS FOR REQUESTED LENGTH INTO TEMPSV AREA                              
*                   LENGTH IS SET AT THIS POINT                                 
*                                                                               
GETDEMS  NTR1                                                                   
         LA    R4,TEMPSV                                                        
         USING TEMPSVD,R4                                                       
         LA    R2,6                                                             
GTD1     OC    0(4,R4),0(R4)                                                    
         BZ    GTD2                                                             
         ZIC   R1,LENGTH                                                        
         C     R1,0(R4)            SAME LENGTH                                  
         BE    GTD2                                                             
         LA    R4,TEMPDLEN(R4)                                                  
         BCT   R2,GTD1                                                          
         DC    H'0'                TOO MANY LENGTHS                             
GTD2     ZIC   R1,LENGTH                                                        
         ST    R1,0(R4)                                                         
         ZIC   R2,PLANNPER                                                      
         LA    R3,PLANPLST                                                      
GTD3     CLI   REQRTFLG,C'Y'       ARE WE FILTERING ON QUARTER                  
         BNE   GTD5                                                             
         CLC   REQUART,2(R3)                                                    
         BNE   GTD10                                                            
GTD5     MVC   PERIOD,0(R3)                                                     
*                                                                               
         GOTO1 VEXTUNS             GET UNITS                                    
         ZIC   R1,UNITS                                                         
         A     R1,UNITSV                                                        
         ST    R1,UNITSV                                                        
*                                  GET DEMOS                                    
         MVC   GDDEMO,=X'000001'       HOMES FIRST                              
*        MVI   GDDEMO,1                HOMES FIRST                              
         GOTO1 VGETDEM                                                          
***      LA    R4,HOMESV                                                        
         L     R1,8(R4)                                                         
         MVC   HALF,HUT            HUT                                          
         L     R5,GDUNITS                                                       
         MH    R5,HALF                                                          
         AR    R1,R5                                                            
         ST    R1,8(R4)                                                         
*                                                                               
         L     R1,12(R4)            SHARE                                       
         MVC   HALF,SHARE                                                       
         L     R5,GDUNITS                                                       
         MH    R5,HALF                                                          
         AR    R1,R5                                                            
         ST    R1,12(R4)                                                        
*                                                                               
         L     R5,GDTGRP           RATING                                       
         A     R5,16(R4)                                                        
         ST    R5,16(R4)                                                        
         L     R5,20(R4)           IMP                                          
         A     R5,GDTIMP                                                        
         ST    R5,20(R4)                                                        
         BAS   RE,OTHDEMS          GET OTHER DEMS                               
GTD10    LA    R3,4(R3)            BUMP DEMOS                                   
         BCT   R2,GTD3                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
OTHDEMS  NTR1                                                                   
         ZIC   R2,NDEMOS                                                        
         LA    R5,DEMSV                                                         
         LA    R3,DEMOS                                                         
OTH2     MVC   GDDEMO,0(R3)                                                     
*OTH2     MVC   GDDEMO,2(R3)                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDVPH                                                         
         MH    R1,GDUNITS+2                                                     
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         L     R1,GDTGRP                                                        
         A     R1,4(R5)                                                         
         ST    R1,4(R5)                                                         
         L     R1,GDTIMP                                                        
         A     R1,8(R5)                                                         
         ST    R1,8(R5)                                                         
         LA    R3,3(R3)                                                         
         LA    R5,12(R5)                                                        
         BCT   R2,OTH2                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
PUTOP    NTR1                          0(R3) HAS SPOT LENGTH                    
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         LA    R4,TEMPSV                                                        
         USING TEMPSVD,R4                                                       
PTP0     OC    UNITSV,UNITSV                                                    
         BZ    PTPXX                                                            
         MVC   PPROG,PROGNAME                                                   
         MVC   PDAY,PROGDAY                                                     
         MVC   PTIME,PROGTIME                                                   
PTP1     EDIT  (B4,0(R4)),(2,PSECLEN)                                           
         EDIT  (B4,UNITSV),(3,PUNITS)                                           
         MVC   UNITEMP,UNITSV             SAVE UNITS                            
         BAS   RE,DEMRTN                                                        
         CLI   DATA,0              TEST DATA TYPE                               
         BNE   PTP5                                                             
         BAS   RE,DEFLT                                                         
         B     PTPX                                                             
DEFLT    NTR1                                                                   
         ZIC   R3,NDEMOS           DEFAULTS TO VPH/RTG/IMP                      
         LA    R4,DEMSV                                                         
         LA    R2,PDVPH                                                         
PTP3     BAS   RE,DNRVPH           VPH                                          
         LR    R5,R2                                                            
         BAS   RE,EDVPH                                                         
         LA    R2,5(R2)                                                         
         LA    R4,4(R4)            RTG                                          
         BAS   RE,DNR                                                           
         LR    R5,R2                                                            
         BAS   RE,EDRTG                                                         
         LA    R2,5(R2)                                                         
         LA    R4,4(R4)            IMP                                          
         BAS   RE,DNR                                                           
         LR    R5,R2                                                            
         BAS   RE,EDIMP                                                         
         LA    R2,6(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,PTP3                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
PTP5     CLI   DATA,1              DATA = 1 = RTG/IMP                           
         BNE   PTP10                                                            
         BAS   RE,DATA1                                                         
         B     PTPX                                                             
*                                                                               
DATA1    NTR1                                                                   
         ZIC   R3,NDEMOS                                                        
         LA    R4,DEMSV+4          POINT TO RTG                                 
         LA    R2,PDRTG1                                                        
PTP7     BAS   RE,DNR                                                           
         LR    R5,R2                                                            
         BAS   RE,EDRTG                                                         
         LA    R2,5(R2)                                                         
         LR    R5,R2                                                            
         LA    R4,4(R4)                                                         
         BAS   RE,DNR                                                           
         BAS   RE,EDIMP                                                         
         LA    R2,6(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,PTP7                                                          
         B     XIT                                                              
*                                                                               
PTP10    CLI   DATA,2              DATA=2=VPH,RTG                               
         BNE   PTP15                                                            
         BAS   RE,DATA2                                                         
         B     PTPX                                                             
*                                                                               
DATA2    NTR1                                                                   
         ZIC   R3,NDEMOS                                                        
         LA    R4,DEMSV            POINT TO VPH                                 
         LA    R2,PDVPH2                                                        
PTP12    BAS   RE,DNRVPH                                                        
         LR    R5,R2                                                            
         BAS   RE,EDVPH                                                         
         LA    R4,4(R4)                                                         
         LA    R2,5(R2)                                                         
         LR    R5,R2                                                            
         BAS   RE,DNR                                                           
         BAS   RE,EDRTG                                                         
         LA    R4,8(R4)                                                         
         LA    R2,5(R2)                                                         
         BCT   R3,PTP12                                                         
         B     XIT                                                              
*                                                                               
PTP15    BAS   RE,DATA3                                                         
         B     PTPX                                                             
*                                                                               
DATA3    NTR1                                                                   
         ZIC   R3,NDEMOS           DATA=3=VPH/IMP                               
         LA    R4,DEMSV                                                         
         LA    R2,PDVPH3                                                        
PTP17    BAS   RE,DNRVPH                                                        
         LR    R5,R2                                                            
         BAS   RE,EDVPH                                                         
         LA    R4,8(R4)                                                         
         LA    R2,5(R2)                                                         
         LR    R5,R2                                                            
         BAS   RE,DNR                                                           
         BAS   RE,EDIMP                                                         
         LA    R4,4(R4)                                                         
         LA    R2,6(R2)                                                         
         BCT   R3,PTP17                                                         
         B     XIT                                                              
*                                                                               
PTPX     BAS   RE,WRITIT                                                        
         LA    R2,P                                                             
PTPXX    LA    R4,TEMPDLEN(R4)                                                  
         OC    0(4,R4),0(R4)                                                    
         BNZ   PTP0                                                             
         B     XIT                                                              
         EJECT                                                                  
*************************************                                           
*                                                                               
DEMRTN   NTR1                                                                   
         LA    R4,HOMESV           HUT                                          
         BAS   RE,DNR                                                           
         LA    R5,PHUT                                                          
         BAS   RE,EDHUT                                                         
         LA    R4,4(R4)            SHR                                          
         BAS   RE,DNR                                                           
         LA    R5,PSHR                                                          
         BAS   RE,EDSHR                                                         
         LA    R4,4(R4)            RTG                                          
         BAS   RE,DNR                                                           
         LA    R5,PRTG                                                          
         BAS   RE,EDRTG                                                         
         LA    R4,4(R4)            IMP                                          
         BAS   RE,DNR                                                           
         LA    R5,PIMP                                                          
         BAS   RE,EDIMP                                                         
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*        R4 POINTS TO DATA                                                      
*        DIVIDE BY UNITS AND ROUND AND RETURN IN FULL                           
*                                                                               
DNR      NTR1                                                                   
         L     R2,0(R4)                                                         
         SR    R3,R3                                                            
         SRDA  R2,31                                                            
         OC    UNITEMP,UNITEMP                                                  
         BZ    XIT                                                              
         D     R2,UNITEMP                                                       
         LTR   R3,R3                                                            
         BM    *+8                                                              
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         ST    R3,FULL                                                          
         B     XIT                                                              
         SPACE 2                                                                
DNRVPH   NTR1                 SAME AS DNR BUT NO ROUNDING                       
         L     R3,0(R4)                                                         
         SR    R2,R2                                                            
         OC    UNITEMP,UNITEMP                                                  
         BZ    XIT                                                              
         D     R2,UNITEMP                                                       
         ST    R3,FULL                                                          
         B     XIT                                                              
         SPACE 2                                                                
ROUNDIT  NTR1                                                                   
         L     R3,FULL                                                          
         SR    R2,R2                                                            
         AH    R3,=H'5'                                                         
         D     R2,=F'10'                                                        
         ST    R3,FULL                                                          
         B     XIT                                                              
*                                                                               
*                                                                               
EDSHR    NTR1                                                                   
         CLI   ROUND,C'Y'                                                       
         BNE   EDSHR2                                                           
         BAS   RE,ROUNDIT                                                       
         EDIT  (B4,FULL),(4,0(R5))                                              
         B     XIT                                                              
EDSHR2   EDIT  (B4,FULL),(4,0(R5)),1                                            
         B     XIT                                                              
*                                                                               
EDHUT    NTR1                                                                   
         CLI   ROUND,C'Y'                                                       
         BNE   EDHUT2                                                           
         BAS   RE,ROUNDIT                                                       
         EDIT  (B4,FULL),(4,0(R5))                                              
         B     XIT                                                              
EDHUT2   EDIT  (B4,FULL),(4,0(R5)),1                                            
         B     XIT                                                              
*                                                                               
EDRTG    DS    0H                                                               
         EDIT  (B4,FULL),(4,0(R5)),1                                            
         BR    RE                                                               
*                                                                               
EDIMP    DS    0H                                                               
         EDIT  (B4,FULL),(5,0(R5))                                              
         BR    RE                                                               
*                                                                               
EDVPH    DS    0H                                                               
         EDIT  (B4,FULL),(4,0(R5))                                              
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVI   HEAD2+1,C'_'     UNDERLINE SYSTEM NAME                           
         MVC   HEAD2+2(21),HEAD2+1                                              
         MVC   WORK(40),TITLE      REPORT TITLE                                 
         OC    WORK,SPACES                                                      
         CLI   WORK,C' '                                                        
         BH    HOOK2                                                            
         SPACE 1                                                                
         CLI   QTITLE,0                                                         
         BE    *+14                                                             
         MVC   WORK(40),TITLE                                                   
         B     HOOK2                                                            
         MVC   WORK(40),=CL40'PROGRAM EVALUATION'                               
         SPACE 1                                                                
HOOK2    GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+41(40),WORK                                                
         SPACE 1                                                                
         MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+15(20),PUPCLIN                                             
         MVC   HEAD5+9(4),PUPNET                                                
         MVC   HEAD4+62(8),PUPDPT                                               
         MVC   HEAD5+59(4),PUPPLAN                                              
         LA    R2,HEAD5+64                                                      
         ZIC   R1,PLANYEAR                                                      
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,0(R2))                                                   
         OC    PLANSV,PLANSV                                                    
         BZ    PUP2C                                                            
         XC    HEAD5+59(10),HEAD5+59                                            
         MVC   HEAD5+61(5),PLANSV                                               
PUP2C    CLI   REQRTFLG,C'Y'                                                    
         BNE   HK2C                                                             
         LA    R3,QUALPHA                                                       
         ZIC   R1,REQUART                                                       
         MH    R1,=H'6'                                                         
         AR    R3,R1                                                            
         MVC   HEAD6+54(6),0(R3)                                                
         MVC   HEAD6+61(7),=C'QUARTER'                                          
HK2C     LA    R2,HEAD8                                                         
         USING PLINED,R2                                                        
         MVC   PHUT(20),=C'-----HOUSEHOLDS-----'                                
         LA    R2,PDVPH+2                                                       
         ZIC   R5,NDEMOS                                                        
         LA    R3,DEMOS                                                         
         GOTO1 VSETDB                                                           
HK3      GOTO1 DEMOCON,DMCB,(0,0(R3)),(7,WORK),(C'S',DBLOCK)                    
         CLI   DATA,0                                                           
         BNE   HK3A                                                             
         MVC   3(7,R2),WORK                                                     
         LA    R2,16(R2)                                                        
         B     HK3D                                                             
HK3A     CLI   DATA,2                                                           
         BNE   HK3B                                                             
         MVC   0(7,R2),WORK                                                     
         LA    R2,10(R2)                                                        
         B     HK3D                                                             
HK3B     MVC   0(7,R2),WORK                                                     
         LA    R2,11(R2)                                                        
HK3D     LA    R3,3(R3)                                                         
         BCT   R5,HK3                                                           
         LA    R2,HEAD9                                                         
         MVC   PPROG,=C'----PROGRAM-----'                                       
         MVC   PDAY,=C'DAY'                                                     
         MVC   PTIME,=C'---TIME----'                                            
         MVC   PSECLEN,=C'LEN'                                                  
         MVC   PSECLEN+4(5),=C'UNITS'                                           
         MVC   PHUT+1(3),=C'HUT'                                                
         MVC   PSHR+1(3),=C'SHR'                                                
         MVC   PRTG+1(3),=C'RTG'                                                
         MVC   PIMP+2(3),=C'IMP'                                                
         CLI   DATA,0                                                           
         BNE   HK10                                                             
         ZIC   R1,NDEMOS                                                        
         LA    R2,PDVPH                                                         
HK7      MVC   1(3,R2),=C'VPH'                                                  
         MVC   6(3,R2),=C'RTG'                                                  
         MVC   12(3,R2),=C'IMP'                                                 
         LA    R2,16(R2)                                                        
         BCT   R1,HK7                                                           
         B     HOOKX                                                            
HK10     DS    0H                                                               
         ZIC   R1,NDEMOS                                                        
         LA    R2,PDRTG1                                                        
         CLI   DATA,1                                                           
         BNE   HK15                                                             
HK12     MVC   1(3,R2),=C'RTG'                                                  
         MVC   7(3,R2),=C'IMP'                                                  
         LA    R2,11(R2)                                                        
         BCT   R1,HK12                                                          
         B     HOOKX                                                            
HK15     CLI   DATA,2                                                           
         BNE   HK18                                                             
HK17     MVC   1(3,R2),=C'VPH'                                                  
         MVC   6(3,R2),=C'RTG'                                                  
         LA    R2,10(R2)                                                        
         BCT   R1,HK17                                                          
         B     HOOKX                                                            
HK18     MVC   1(3,R2),=C'VPH'                                                  
         MVC   7(3,R2),=C'IMP'                                                  
         LA    R2,11(R2)                                                        
         BCT   R1,HK18                                                          
*                                                                               
*                                                                               
         DROP  R2                                                               
HOOKX    CLI   BOXOPT,C'N'                                                      
         BE    HOOKXX                                                           
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    HOOKXX                                                           
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         LA    R3,BOXROWS                                                       
         LA    R3,6(R3)                                                         
         MVI   0(R3),C'T'                                                       
         LA    R3,3(R3)                                                         
         MVI   0(R3),C'M'                                                       
         LA    R3,65(R3)                                                        
         MVI   0(R3),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R3,BOXCOLS                                                       
         USING PLINED,R3                                                        
         MVI   0(R3),C'L'                                                       
         LA    R3,PIMP+5                                                        
         ZIC   R5,NDEMOS                                                        
HK29     MVI   0(R3),C'C'                                                       
         CLI   DATA,0                                                           
         BNE   HK30                                                             
         LA    R3,16(R3)                                                        
         B     HK40                                                             
HK30     CLI   DATA,2                                                           
         BNE   HK34                                                             
         LA    R3,10(R3)                                                        
         B     HK40                                                             
HK34     LA    R3,11(R3)                                                        
HK40     BCT   R5,HK29                                                          
         MVI   0(R3),C'R'                                                       
HOOKXX   B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 1                                                                
*                                                                               
         SPACE 3                                                                
WRITIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
BADQUART MVC   CONHEAD(L'MSGQUART),MSGQUART                                     
         B     MYEND                                                            
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
DRQLIST  DC    AL1(4,1,2,3)                                                     
         SPACE 1                                                                
MSGQUART DC    C'** ERROR ** QUARTER MUST BE 1-4'                               
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
INVERR   DC    C'** ERROR ** INVALID INPUT'                                     
         SPACE 1                                                                
QUALPHA  DC    C'FOURTH'                                                        
         DC    C'FIRST '                                                        
         DC    C'SECOND'                                                        
         DC    C'THIRD '                                                        
         SPACE 1                                                                
SAVEPROF DC    X'0000'                                                          
         EJECT                                                                  
*              SPECS FOR PHASE                                                  
         SPACE 3                                                                
HEDSPECS DS    0D                                                               
         SSPEC H1,2,C'NETWORK UPFRONT SYSTEM'                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'NETWORK'                                                  
         SSPEC H4,55,C'DAYPART'                                                 
         SSPEC H5,55,C'PLAN      1988'                                          
         SPACE 1                                                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H5,115,PAGE                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         SPACE 1                                                                
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPD6D                                                       
         EJECT                                                                  
         SPACE 1                                                                
*                             WORK AREA                                         
DATA     DS    CL1                                                              
REQLEN   DS    CL1                                                              
REQUART  DS    CL1                                                              
REQRTFLG DS    CL1                                                              
RELO     DS    F                                                                
DISP     DS    F                                                                
AP       DS    F                                                                
ABIN     DS    F                                                                
BINDMCB  DS    6F                                                               
FINTOTS  DS    5F                                                               
PLANSV   DS    CL5                                                              
MYHALF   DS    CL2                                                              
MYWORK   DS    CL20                                                             
PLANKYSV DS    CL20                                                             
TITLE    DS    CL40                                                             
QTITLE   DS    CL1                                                              
ROUND    DS    CL1                                                              
KEYSV    DS    CL19                                                             
UNITEMP  DS    F                                                                
         DS    0F                                                               
TEMPSV   DS    CL576               ROOM FOR 6 SPOT LENGTHS                      
TEMPEND  EQU   *                                                                
*                                                                               
*                                                                               
TEMPSVD  DSECT                     DSECT FOR TEMPSV                             
LENSV    DS    F                                                                
UNITSV   DS    F                                                                
HOMESV   DS    4F                  HUT/SHR/RTG/IMP                              
DEMSV    DS    18F                 6 X (VPH/RTG/IMP)                            
TEMPDLEN EQU   *-LENSV                                                          
         SPACE 3                                                                
*                                                                               
BIND     DSECT                  DSECT FOR BINSRCH RECORDS                       
BINDAY   DS    CL1                                                              
BINTIME  DS    CL4                                                              
BINPROG  DS    CL6                                                              
BINDATA  DS    CL20                PUP PROGRAM REC KEY                          
BINRLENE EQU   *-BINDAY                                                         
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
         DS    CL1                                                              
PPROG    DS    CL16                                                             
         DS    CL1                                                              
PDAY     DS    CL3                                                              
         DS    CL2                                                              
PTIME    DS    CL11                                                             
         DS    CL2                                                              
PSECLEN  DS    CL3                                                              
         DS    CL2                                                              
PUNITS   DS    CL3                                                              
         DS    CL2                                                              
PHUT     DS    CL4                                                              
         DS    CL1                                                              
PSHR     DS    CL4                                                              
         DS    CL1                                                              
PRTG     DS    CL4                                                              
         DS    CL1                                                              
PIMP     DS    CL5                                                              
         DS    CL1                                                              
PDVPH    DS    CL4                 DATA=0 VPH/RTG/IMP                           
         DS    CL1                                                              
PDRTG    DS    CL4                                                              
         DS    CL1                                                              
PDIMP    DS    CL5                                                              
         DS    CL1                                                              
         DS    CL48                                                             
PRLENE   EQU   *-PLINED                                                         
         ORG   PDVPH                                                            
PLINED1  DS    0CL1                DATA=1 RTG/IMP                               
PDRTG1   DS    CL4                                                              
         DS    CL1                                                              
PDIMP1   DS    CL5                                                              
         DS    CL1                                                              
         DS    CL55                                                             
         ORG   PDVPH                                                            
PLINED2  DS    0CL1                DATA=2 VPH/RTG                               
PDVPH2   DS    CL4                                                              
         DS    CL1                                                              
PDRTG2   DS    CL4                                                              
         DS    CL1                                                              
         DS    CL50                                                             
         ORG   PDVPH                                                            
PLINED3  DS    0CL1                DATA=3 VPH/IMP                               
PDVPH3   DS    CL4                                                              
         DS    CL1                                                              
PDIMP3   DS    CL5                                                              
         DS    CL1                                                              
         DS    CL55                                                             
*                                                                               
*                                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEPUP26   05/01/02'                                      
         END                                                                    
