*          DATA SET RERMP1C    AT LEVEL 187 AS OF 01/11/11                      
*PHASE T8101CC,*                                                                
*INCLUDE UPOUT                                                                  
*INCLUDE COVAIL                                                                 
         TITLE 'T8101C - RERMP1C - DEMO TAPE CREATE'                            
*                                                                               
***********************************************************************         
*                                                                     *         
*- RERMP1C -- DEMO TAPE CREATE                                        *         
*                                                                     *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
* 15APR09 (KUI) SUPPORT NEW INVENTORY RECORD                          *         
*                                                                     *         
* 11JAN11 (KUI) FIX GETKSRC K MODE INPUT PARAMETER                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
         SPACE 2                                                                
T8101C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8101C**,RR=RE                                                 
         USING T8101C,RB,R8                                                     
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
*                                                                               
*        L     R8,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
*        USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
         L     R7,ACOMFACS         ESTABLLISH COMMON SUBROUTINES                
         USING COMFACSD,R7                                                      
*                                                                               
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         OI    GENSTAT2,NOREQDET                                                
*                                                                               
         MVI   BLANKS,X'40'                                                     
         MVC   BLANKS+1(131),BLANKS                                             
*                                                                               
*        L     R1,XSORT                                                         
*        A     R1,RELO                                                          
*        ST    R1,ATAPEDCB                                                      
*                                                                               
*                                                                               
         L     R1,TWADCONS                                                      
         A     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,ASAVE                                                         
         LA    R1,200(R1)                                                       
         ST    R1,ATAPEDCB                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   RP4                                                              
         BAS   RE,SETTAPE                                                       
         B     EXXMOD                                                           
         SPACE 1                                                                
RP4      CLI   MODE,PRINTREP                                                    
         BNE   RP6                                                              
         BAS   RE,REPMODE                                                       
         B     EXXMOD                                                           
         SPACE 1                                                                
RP6      CLI   MODE,VALKEY                                                      
         BNE   RP8                                                              
         BAS   RE,VALREQ                                                        
         B     EXXMOD                                                           
         SPACE 1                                                                
RP8      CLI   MODE,RUNLAST                                                     
         BNE   EXXMOD                                                           
         L     R2,ATAPEDCB                                                      
         CLOSE ((R2),)                                                          
*                                                                               
*  PRINT STATION REPORT                                                         
*                                                                               
         L     R6,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R6                                                        
         GOTO1 OPENPQ                                                           
*                                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         L     R1,=A(HOOK)                                                      
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R3,P                                                             
         LA    R4,15                                                            
         L     RE,ASAVE                                                         
         MVC   ASTATAB(8),0(RE)     GET ADDRESSES                               
         L     R5,ASTATAB                                                       
*                                                                               
RP200    CLI   0(R5),X'FF'                                                      
         BE    RP250                                                            
         MVC   0(5,R3),0(R5)                                                    
         LA    R3,8(R3)                                                         
         LA    R5,5(R5)                                                         
         BCT   R4,RP200                                                         
*                                                                               
RP250    GOTO1 SPOOL,DMCB,(R6)                                                  
         CLI   0(R5),X'FF'                                                      
         BE    EXXMOD                                                           
         LA    R3,P                                                             
         LA    R4,15                                                            
         B     RP200                                                            
*                                                                               
RP300    L     R3,ASTATAB          FREE UP GLOBAL STORAGE                       
         L     R5,=F'4000'                                                      
         GOTO1 =V(COVAIL),DMCB,C'FREE',(R3),(R5)                                
*                                                                               
EXXMOD   XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*-- OPEN UP THE TAPE                                                            
*                                                                               
SETTAPE  NTR1                                                                   
*                                                                               
*        L     RE,XSORT                                                         
*        LA    RF,FILOUTA                                                       
*        MVC   0(128,RE),0(RF)                                                  
*                                                                               
         L     RE,ATAPEDCB                                                      
         LA    RF,FILOUTA                                                       
         MVC   0(128,RE),0(RF)                                                  
*                                                                               
         MVC   WORK(20),=CL20'REPTAPE.RE0DT  1'                                 
         MVC   WORK+13(2),AGENCY   SET 1ST LETTER OF EMPLOYER                   
***      GOTO1 =V(DYNALLOC),DMCB,(0,=CL8'FILOUTA'),(0,WORK)                     
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         PRINT GEN                                                              
         GOTO1 (RF),DMCB,(0,=CL8'FILOUTA'),(0,WORK)                             
         PRINT NOGEN                                                            
*                                                                               
         L     R4,ATAPEDCB                                                      
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFOPEN',0(R4),C'DUMP',200,=C'1D'             
         OPEN  ((R4),(OUTPUT))                                                  
*                                                                               
         L     RE,ATWA                                                          
         MVI   29(RE),2                                                         
*                                                                               
         L     R5,=F'4000'       GET STORAGE FOR STATION REPORT                 
         ST    R5,DMCB+4                                                        
         ST    R5,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   R6,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ASTATAB                                                       
         MVI   0(R6),X'FF'                                                      
         LA    R6,0(R5,R6)                                                      
         ST    R6,ASTATABX                                                      
         L     RE,ASAVE                                                         
         MVC   0(8,RE),ASTATAB      SAVE ADDRESSES                              
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
VALREQ   NTR1                                                                   
*  IF AUTO TRANSFER CANNOT REQUEST LTRANS                                       
         CLC   CONACT(2),=CL2'LT'                                               
         BNE   VALSTA                                                           
*                                                                               
*--CHECK FOR AUTO GENERATOR                                                     
         LR    R2,RA                                                            
         AH    R2,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R2                                                       
         TM    SVPGPBIT,X'10'                                                   
         BO    AUTOERR                                                          
         SPACE 1                                                                
VALSTA   LA    R2,TITSTAH          STATION FIELD                                
         XC    WORK,WORK                                                        
         GOTO1 VALISTA             VALIDATE STATION                             
         CLI   WORK+40,X'40'                                                    
         BNH   *+10                                                             
         MVC   CSTAT+4(1),WORK+40                                               
*                                                                               
*        VALIDATE START DATE                                                    
*                                                                               
         LA    R2,TITSDTH          POINT TO START DATE                          
         XC    STRTOPT,STRTOPT     INIT START DATE OPTIONS                      
         XC    STRTOPTC,STRTOPTC   INIT START DATE OPTIONS                      
*                                                                               
         CLI   5(R2),0             OKAY IF NO START DATE ENTERED                
         BE    VREND                                                            
*                                                                               
         CLC   8(3,R2),=C'ALL'     OKAY IF 'ALL' ENTERED                        
         BE    VREND                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK   VALIDATE ENTERED DATE               
*                                                                               
         CLI   DMCB+3,0                                                         
         BNE   VRSTRT1                                                          
         B     DATERR                                                           
*                                                                               
VRSTRT1  GOTO1 DATCON,DMCB,(0,WORK),(3,STRTOPT)  SAVE START DATE                
         GOTO1 DATCON,DMCB,(0,WORK),(2,STRTOPTC) SAVE START DATE                
*                                                                               
*        VALIDATE END DATE                                                      
*                                                                               
VREND    LA    R2,TITEDTH          POINT TO END DATE                            
         MVC   ENDOPT,=X'FFFFFF'   INIT INTERNAL END DATE                       
         MVC   ENDOPTC,=X'FFFF'    INIT INTERNAL END DATE                       
*                                                                               
         CLI   5(R2),0             OKAY IF NO END DATE ENTERED                  
         BE    VRENDX                                                           
*                                                                               
         CLC   8(3,R2),=C'ALL'     IKAY IF 'ALL' ENTERED                        
         BE    VRENDX                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  VALIDATE ENTERED DATE                
         CLI   DMCB+3,0                                                         
         BNE   VREND1                                                           
         B     DATERR                                                           
*                                                                               
VREND1   GOTO1 DATCON,DMCB,(0,WORK),(3,ENDOPT) SAVE END DATE                    
         GOTO1 DATCON,DMCB,(0,WORK),(2,ENDOPTC) SAVE END DATE                   
*                                                                               
VRENDX   CLC   STRTOPT,ENDOPT      CHECK START LESS THEN END                    
         BH    DATERR                                                           
*                                                                               
*        VALIDATE COMPETITIVE STATION                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         MVI   COMPSTA,C'N'        DEFAULT                                      
*                                                                               
         LA    R2,TITCSTH                                                       
         CLI   5(R2),0                                                          
         BE    EXXMOD                                                           
         MVC   COMPSTA,TITCST                                                   
         CLI   TITCST,C'N'                                                      
         BE    EXXMOD                                                           
         CLI   TITCST,C'Y'                                                      
         BNE   ERREND                                                           
         B     EXXMOD                                                           
*                                                                               
DATERR   MVC   RERROR,=AL2(INVDATE) ERROR - INVALID DATE                        
         GOTO1 MYERROR                                                          
*                                                                               
AUTOERR  MVC   RERROR,=AL2(INVACT) ERROR - INVALID DATE                         
         LA    R2,CONACTH                                                       
         GOTO1 MYERROR                                                          
         EJECT                                                                  
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
REPMODE  NTR1                                                                   
*  SET STATION IN SAVE LIST FOR REPORT                                          
         L     RE,ASAVE                                                         
         MVC   ASTATAB(8),0(RE)     GET ADDRESSES                               
         L     RE,ASTATAB                                                       
         LA    RF,800                                                           
RPM10    CLI   0(RE),X'FF'                                                      
         BE    RPM20                                                            
         LA    RE,5(RE)                                                         
         BCT   RF,RPM10                                                         
         B     *+14                                                             
RPM20    MVC   0(5,RE),CSTAT                                                    
         MVI   5(RE),X'FF'                                                      
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'REP',AIO3,C'DUMP',700,=C'1D'                  
*                                                                               
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
*                                                                               
         BAS   RE,BLDHEAD          HEADER RECORD                                
         BAS   RE,BLDDPT           DAYPART RECORD                               
*                                                                               
RPM40    BAS   RE,GTINV            READ INVENTORY RECORD                        
         CLI   KEY,X'FF'                                                        
         BE    RPMEX                                                            
*                                                                               
INVKEYD  USING RINVKEY,KEY                                                      
         CLI   INVKEYD.RINVKRTP,0                                               
         BNE   RPM100                                                           
         BAS   RE,BLDINVH          INV HEADER RECORD                            
         B     RPM40                                                            
*                                                                               
RPM100   DS    0H                                                               
         CLI   INVKEYD.RINVKRTP,X'FF'                                           
         BNE   RPM200                                                           
         BAS   RE,BLDCOMM          COMMENT RECORD                               
         B     RPM40                                                            
*                                                                               
RPM200   DS    0H                                                               
         CLI   INVKEYD.RINVKRTP,C'M'                                            
         BE    RPM40                                                            
         CLI   INVKEYD.RINVKRTP,C'S'                                            
         BE    RPM40                                                            
         BAS   RE,BLDSRVY          SURVEY RECORD                                
         BAS   RE,BLDOVR           OVERRIDE RECORD                              
         B     RPM40                                                            
*                                                                               
RPMEX    BAS   RE,BLDTRL           TRAILER                                      
         B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*--READ AND FILTER INVENTORY RECORDS                                            
*                                                                               
GTINV    NTR1                                                                   
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
*                                                                               
         CLI   KEYSAVE,RINVKTYQ    FIRST TIME                                   
         BE    GTINV200            NO GET NEXT RECORD                           
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,CSTAT                                                   
*        MVC   RINVKINV,=CL4'0000'                                              
         GOTO1 HIGH                                                             
*                                  CHECK UP TO STATION                          
         CLC   KEY(RINVKINV-RINVKEY),KEYSAVE                                    
         BE    *+12                                                             
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
INVKEYD  USING RINVKEY,KEY                                                      
         XC    INVKEYD.RINVKSTD(RINVLEN-RINVKSTD),INVKEYD.RINVKSTD              
         MVC   INVKEYD.RINVKSTD(3),STRTOPT   MOVE DATE IN                       
         GOTO1 HIGH                                                             
*                                  CHECK UP TO STATION                          
         CLC   KEY(RINVKINV-RINVKEY),KEYSAVE                                    
         BE    GTINV300                                                         
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
* GET NEXT RECORD IF A HEADER CHECK IF IT FITS THE REQUEST                      
*                                                                               
GTINV200 GOTO1 SEQ                                                              
*                                  CHECK UP TO STATION                          
         CLC   KEY(RINVKINV-RINVKEY),KEYSAVE                                    
         BE    *+12                                                             
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
         CLI   RINVKRTP,0          IS RECORD A HEADER                           
         BE    GTINV300                                                         
*                                                                               
         GOTO1 GETREC                                                           
         B     EXXMOD                                                           
*                                                                               
GTINV300 CLC   RINVKSTD,ENDOPT     IS INV START > REQ END                       
         BH    GTINV400            SKIP TO NEXT INV NUMBER                      
*                                                                               
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLC   RINVKSTD,STRTOPT    IS INV START >= REQ START                    
         BNL   EXXMOD              RECORD CAN BE PROCESSED                      
*                                                                               
         CLC   RINVPEFF+2,STRTOPTC IS INV END >= REQ START                      
         BNL   EXXMOD              RECORD CAN BE PROCESSED                      
*                                                                               
*  CURRENT HEADERS DATE OUT OF RANGE SKIP READ TO NEXT DATE                     
*                                                                               
         LA    R4,KEY                                                           
         ICM   RE,7,RINVKSTD                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,7,RINVKSTD                                                    
         GOTO1 HIGH                                                             
*                                  CHECK UP TO STATION                          
         CLC   KEY(RINVKINV-RINVKEY),KEYSAVE                                    
         BE    *+12                                                             
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
         CLI   RINVKRTP,0                                                       
         BE    GTINV300                                                         
         DC    H'0'                                                             
*                                                                               
*  CURRENT HEADER DOES NOT QUALIFY SKIP READ TO INV NUMBER                      
*                                                                               
GTINV400 MVC   FULL,RINVKINV                                                    
         ICM   R5,15,RINVKINV                                                   
         A     R5,=F'1'                                                         
         STCM  R5,15,RINVKINV                                                   
*                                                                               
INVKEYD  USING RINVKEY,KEY                                                      
         XC    INVKEYD.RINVKSTD(RINVLEN-RINVKSTD),INVKEYD.RINVKSTD              
         GOTO1 HIGH                                                             
*                                  CHECK UP TO STATION                          
         CLC   KEY(RINVKINV-RINVKEY),KEYSAVE                                    
         BE    *+12                                                             
GTINV420 MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
         XC    INVKEYD.RINVKSTD(RINVLEN-RINVKSTD),INVKEYD.RINVKSTD              
         MVC   INVKEYD.RINVKSTD(3),STRTOPT   MOVE DATE IN                       
         GOTO1 HIGH                                                             
*                                  CHECK UP TO STATION                          
         CLC   KEY(RINVKINV-RINVKEY),KEYSAVE                                    
         BE    GTINV300                                                         
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
*--BUILD THE HEADER RECORD                                                      
*                                                                               
BLDHEAD  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING HDREC,R3                                                         
         XC    0(250,R3),0(R3)                                                  
*                                                                               
         LA    RE,28                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   HDID,=CL2'H1'                                                    
         MVC   HDSRC,=CL4'DDS '                                                 
         MVC   HDVER,=CL2'01'                                                   
         MVC   HDREP,AGENCY                                                     
         GOTO1 DATCON,DMCB,(5,DUB),(0,HDSVDAT)                                  
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                   
         MVC   HDSVTIM(2),DUB                                                   
         MVI   HDSVTIM+2,C':'                                                   
         MVC   HDSVTIM+3(2),DUB+2                                               
         MVI   HDSVTIM+5,C':'                                                   
         MVC   HDSVTIM+6(2),DUB+4                                               
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*--BUILD THE DAYPART RECORDS                                                    
*                                                                               
BLDDPT   NTR1                                                                   
         LA    R5,KEY                                                           
         USING RRDPRECD,R5                                                      
*                                                                               
*  READ THE DAYPART RECORDS                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RRDPKTYP,X'3C'                                                   
         MVC   RRDPKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         B     BDP100                                                           
*                                                                               
BDP80    GOTO1 SEQ                                                              
*                                                                               
BDP100   CLC   KEY(26),KEYSAVE                                                  
         BNE   BDPEX                                                            
         MVC   AIO,AIO1                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING DPTREC,R3                                                        
         XC    0(250,R3),0(R3)                                                  
*                                                                               
         LA    RE,27                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   DPID,=CL2'D1'                                                    
         MVC   DPCODE,RRDPKDPT                                                  
         MVC   DPSNAME(3),RRDPSNAM                                              
         MVC   DPLNAME,RRDPLNAM                                                 
         OI    DPSNAME+3,X'40'                                                  
         OI    DPLNAME+15,X'40'                                                 
         BAS   RE,WRITREC                                                       
         B     BDP80                                                            
*                                                                               
BDPEX    B     EXXMOD                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE INVENTORY RECORD                                                   
*                                                                               
BLDINVH  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING IMREC,R3                                                         
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,454                                                           
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   IMID,=CL2'I1'                                                    
         MVC   IMSTA,RINVKSTA                                                   
         CLI   IMSTA+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   IMSTA+4,X'40'                                                    
         MVC   IMINVN,RINVKINV                                                  
         MVC   IMDYPTS,RINVDP                                                   
         MVC   IMFLTER,RINVPFLT                                                 
         MVC   IMMARKET,CMKTNAM                                                 
         OC    IMMARKET,BLANKS                                                  
         MVC   IMPOWER,RINVKREP                                                 
         MVC   IMCOMPST,COMPSTA    COMP STATION INDICATOR                       
         CLI   RINVTCHG,C'S'                                                    
         BNE   *+10                                                             
         MVC   IMTIMCHG,=CL3'+60'                                               
         CLI   RINVTCHG,C'F'                                                    
         BNE   *+10                                                             
         MVC   IMTIMCHG,=CL3'-60'                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(X'20',IMEFFS)                          
         OC    IMEFFE,BLANKS                                                    
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    BDI100                                                           
         LA    R4,RINVPEFF+2                                                    
         GOTO1 DATCON,DMCB,(2,(R4)),(X'20',IMEFFE)                              
*                                                                               
*  PEPARE FOR THE YEAR 2000                                                     
         CLI   IMEFFS,X'FA'                                                     
         BL    BDI50                                                            
         ZIC   RE,IMEFFS                                                        
         SH    RE,=H'10'                                                        
         STCM  RE,1,IMEFFS                                                      
*                                                                               
BDI50    CLI   IMEFFE,X'FA'                                                     
         BL    BDI100                                                           
         ZIC   RE,IMEFFE                                                        
         SH    RE,=H'10'                                                        
         STCM  RE,1,IMEFFE                                                      
*                                                                               
*  DAY/TIME LOGIC                                                               
*                                                                               
BDI100   MVI   IMDAY1,X'40'        BLANK OUT THE FIELD                          
         MVC   IMDAY1+1(175),IMDAY1                                             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'04',AIO),0                          
         CLI   12(R1),0                                                         
         BE    BDI300              DO AVAIL MOVE                                
*                                                                               
*  FILL DAY/TIME FIELDS FROM REGULART DAY/TIME ELEMENTS                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,12(R1)                                                        
         LA    R6,IMDAY1                                                        
BDI200   XC    WORK(11),WORK                                                    
         GOTO1 UNDAY,DMCB,2(R4),WORK              DAY                           
         MVC   0(11,6),WORK                                                     
         GOTO1 UNTIME,DMCB,3(R4),(0,11(R6))       TIME                          
         OC    0(22,R6),BLANKS     BLANK FILL THE FIELDS                        
         OC    5(2,R4),5(R4)                                                    
         BNZ   BDI260                                                           
*  IF NO END TIME MOVE A ,B AFTER THE START                                     
         LA    RE,11(R6)                                                        
         LA    RF,11                                                            
BDI220   CLI   0(RE),X'40'                                                      
         BNH   BDI240                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,BDI220                                                        
         DC    H'0'                                                             
BDI240   MVC   0(2,RE),=C',B'                                                   
*                                                                               
BDI260   LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),2                                                          
         BNE   BDI350                                                           
         B     BDI200                                                           
*                                                                               
*  FILL DAY/TIME FIELDS FROM AVAIL INFO                                         
*                                                                               
BDI300   L     R4,12(R1)                                                        
         LA    R6,IMDAY1                                                        
BDI320   MVC   0(22,R6),2(R4)                                                   
         OC    0(22,R6),=22X'40'                                                
         LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),4                                                          
         BNE   BDI350                                                           
         B     BDI320                                                           
*                                                                               
*  SAVE DAY TIME FOR TRACKS                                                     
*                                                                               
BDI350   XC    DAYTMHD,DAYTMHD                                                  
         LA    R6,DAYTMHD                                                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*  SAVE DAY TIME                                                                
BDI370   MVC   0(5,R6),2(R4)                                                    
*                                                                               
*        SEE IF MORE DAY TIME ELEMENTS                                          
*                                                                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),2                                                          
         BNE   BDI400                                                           
         B     BDI370                                                           
*                                                                               
*  PROGRAM NAME LOGIC                                                           
*                                                                               
BDI400   MVI   IMPROG1,X'40'       BLANK OUT THE FIELD                          
         MVC   IMPROG1+1(215),IMPROG1                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*  SAVE FIRST PROGRAM LINE FOR THE TRACK RECORDS                                
         L     R4,12(R1)                                                        
*                                                                               
         MVC   SAVPROG,BLANKS                                                   
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVPROG(0),2(R4)                                                 
*                                                                               
         LA    R6,IMPROG1                                                       
BDI440   ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     BDI460                                                           
         MVC   0(0,R6),2(R4)                                                    
*                                                                               
BDI460   OC    0(27,R6),BLANKS                                                  
         LA    R6,27(R6)                                                        
*        SEE IF MORE PROG NAME ELEMENTS EXIST                                   
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),3                                                          
         BNE   BDIEX                                                            
         B     BDI440                                                           
*                                                                               
BDIEX    BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE SURVEY RECORD                                                      
*                                                                               
BLDSRVY  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING SRVREC,R3                                                        
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,750                                                           
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   SRVID,=C'S1'                                                     
         MVC   SRVSTA,RINVKSTA                                                  
         CLI   SRVSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   SRVSTA+4,X'40'                                                   
         MVC   SRVINV,RINVKINV                                                  
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(X'20',SRVSEFF)                         
         MVC   SRVBOOK,RINVKBK                                                  
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK(1),RINVKRSR                                                 
         MVC   WORK+1(1),RINVKQLF                                               
         MVC   WORK+3(1),RINVKBTP                                               
         GOTO1 VGETKSRC,DMCB,(C'K',WORK),WORK+10,ACOMFACS                       
         MVC   SRVSRC,WORK+10      SERVICE                                      
         MVC   SRVLOOK,WORK+11     LOOKUP TYPE                                  
         MVC   SRVBTYP,WORK+14     BOOK TYPE                                    
         CLI   SRVBTYP,0                                                        
         BNE   *+8                                                              
         MVI   SRVBTYP,X'40'                                                    
*                                                                               
         MVC   SRVPROG,SAVPROG     GET DEFAULT PROGRAM NAME                     
*        CLI   RINVKSRC,C'O'       IF PROJECTED                                 
*        BE    BLDSR070            US DEFAULT                                   
         CLI   RINVKRSR,C'N'                                                    
         BNE   BLDSR050                                                         
         CLI   RINVKQLF,X'44'                                                   
         BNE   BLDSR050                                                         
         CLI   RINVKBTP,0                                                       
         BE    BLDSR070                                                         
*                                                                               
BLDSR050 DS    0H                                                               
         MVC   SRVPROG,BLANKS                                                   
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'01',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR070                                                         
         L     R4,12(R1)                                                        
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         S     R4,=F'16'                                                        
         MVC   SRVPROG(16),0(R4)                                                
*                                                                               
BLDSR070 MVC   SRVBRCD,=CL2'PV'                                                 
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CD',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR080                                                         
         L     R4,12(R1)                                                        
         CLI   2(R4),X'40'                                                      
         BE    BLDSR080                                                         
         MVC   SRVBRCD(2),2(R4)                                                 
                                                                                
*                                                                               
BLDSR080 XC    SRVFRBK(9),SRVFRBK                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR090                                                         
         L     R4,12(R1)                                                        
         USING RINVFREL,R4                                                      
         MVC   SRVFRBK,RINVFRBK+1                                               
         MVC   SRVFRTP,RINVFRTY                                                 
         MVC   SRVFRFN,RINVFRPR                                                 
         MVC   SRVFRST,RINVFRST                                                 
         DROP  R4                                                               
*                                                                               
BLDSR090 MVC   SRVUPGF,BLANKS                                                   
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'05',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR100                                                         
         L     R4,12(R1)                                                        
         GOTO1 =V(UPOUT),DMCB,(R4),UPGRADST                                     
         MVC   SRVUPGF,UPGRADST                                                 
         OC    SRVUPGF,BLANKS                                                   
*                                                                               
*  FILL DAY/TIME FIELDS FROM REGULART DAY/TIME ELEMENTS                         
*                                                                               
BLDSR100 MVI   SRVDAY1,X'40'       BLANK OUT THE FIELD                          
         MVC   SRVDAY1+1(175),SRVDAY1                                           
*                                                                               
*        GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CE',AIO),0                          
*        CLI   12(R1),0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        L     R4,12(R1)                                                        
*                                                                               
         OC    DAYTMHD+1(4),DAYTMHD+1                                           
         BNZ   *+6                                                              
         DC    H'0'                MUST BE 1 DAY TIME                           
         LA    R4,DAYTMHD                                                       
         LA    R6,SRVDAY1                                                       
BLDSR140 XC    WORK(11),WORK                                                    
         GOTO1 UNDAY,DMCB,0(R4),WORK              DAY                           
         MVC   0(11,6),WORK                                                     
         GOTO1 UNTIME,DMCB,1(R4),(0,11(R6))       TIME                          
         OC    0(22,R6),BLANKS     BLANK FILL THE FIELDS                        
         LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
*        ZIC   RE,1(R4)                                                         
*        AR    R4,RE                                                            
*        CLI   0(R4),X'CE'                                                      
*        BE    BLDSR140                                                         
         LA    R4,5(R4)                                                         
         OC    1(4,R4),1(R4)                                                    
         BNZ   BLDSR140                                                         
*                                                                               
* MOVE THE DEMOS OUT                                                            
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVC   DBAREC,AIO                                                       
         L     R1,DBAREC                                                        
         LA    R1,34(R1)           POINT TO FIRST ELEMENT POSITION              
         ST    R1,DBAQUART                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
*  MOVE OUT THE STATION LEVEL DEMOS                                             
*                                                                               
         SPACE 1                                                                
         GOTO1 CDEMOUT,DMCB,(C'L',STADEMS),DBLOCK,AIO2                          
         SPACE 1                                                                
*                                                                               
*  DO THE RATINGS                                                               
*                                                                               
         L     RE,AIO2                                                          
         LA    RF,33                                                            
         LA    R1,STPRAT                                                        
*                                                                               
BLDSR300 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR300                                                      
*                                                                               
*  DO THE IMPS                                                                  
*                                                                               
         LA    RF,34                                                            
         LA    R1,STPIMP                                                        
*                                                                               
BLDSR350 MVC   0(4,R1),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   RF,BLDSR350                                                      
*                                                                               
*  DO THE VARIOUS DEMOS                                                         
*                                                                               
         LA    RF,9                                                             
         LA    R1,STHLDRTG                                                      
*                                                                               
BLDSR400 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR400                                                      
*                                                                               
*  MOVE OUT THE MARKET LEVEL DEMOS                                              
*                                                                               
         SPACE 1                                                                
         GOTO1 CDEMOUT,DMCB,(C'L',MKTDEMS),DBLOCK,AIO2                          
         SPACE 1                                                                
*                                                                               
*  DO THE RATINGS                                                               
*                                                                               
         L     RE,AIO2                                                          
         LA    RF,33                                                            
         LA    R1,MKPRAT                                                        
*                                                                               
BLDSR500 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR500                                                      
*                                                                               
*  DO THE IMPS                                                                  
*                                                                               
         LA    RF,34                                                            
         LA    R1,MKPIMP                                                        
*                                                                               
BLDSR550 MVC   0(4,R1),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   RF,BLDSR550                                                      
*                                                                               
*  DO THE VARIOUS DEMOS                                                         
*                                                                               
         LA    RF,9                                                             
         LA    R1,MKHLDRTG                                                      
*                                                                               
BLDSR600 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR600                                                      
*                                                                               
* SET MARKET SHARE VALUES TO 100                                                
*                                                                               
         MVC   MKHLDSHR,=F'1000'                                                
         MVC   MKMTASHR,=F'1000'                                                
         MVC   MKMTBSHR,=F'1000'                                                
*                                                                               
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3,R5                                                            
TYPTAB   DC    X'C1',CL2'MA'      SOURCE = A                                    
         DC    X'09',CL2'MA'                                                    
         DC    X'0A',CL2'MA'                                                    
         DC    X'0B',CL2'MA'                                                    
         DC    X'0C',CL2'MA'                                                    
         DC    X'0D',CL2'MA'                                                    
         DC    X'0E',CL2'MA'                                                    
         DC    X'C2',CL2'MP'      SOURCE = B                                    
         DC    X'11',CL2'MP'                                                    
         DC    X'12',CL2'MP'                                                    
         DC    X'13',CL2'MP'                                                    
         DC    X'14',CL2'MP'                                                    
         DC    X'15',CL2'MP'                                                    
         DC    X'16',CL2'MP'                                                    
         DC    X'C3',CL2'MT'      SOURCE = C                                    
         DC    X'19',CL2'MT'                                                    
         DC    X'1A',CL2'MT'                                                    
         DC    X'1B',CL2'MT'                                                    
         DC    X'1C',CL2'MT'                                                    
         DC    X'1D',CL2'MT'                                                    
         DC    X'1E',CL2'MT'                                                    
         DC    X'C4',CL2'MS'      SOURCE = D                                    
         DC    X'21',CL2'MS'                                                    
         DC    X'22',CL2'MS'                                                    
         DC    X'23',CL2'MS'                                                    
         DC    X'24',CL2'MS'                                                    
         DC    X'25',CL2'MS'                                                    
         DC    X'26',CL2'MS'                                                    
         DC    X'C5',CL2'ME'      SOURCE = E                                    
         DC    X'29',CL2'ME'                                                    
         DC    X'2A',CL2'ME'                                                    
         DC    X'2B',CL2'ME'                                                    
         DC    X'2C',CL2'ME'                                                    
         DC    X'2D',CL2'ME'                                                    
         DC    X'2E',CL2'ME'                                                    
         DC    X'D5',CL2'NA'      SOURCE = N                                    
         DC    X'31',CL2'NA'                                                    
         DC    X'32',CL2'NA'                                                    
         DC    X'33',CL2'NA'                                                    
         DC    X'34',CL2'NA'                                                    
         DC    X'35',CL2'NA'                                                    
         DC    X'36',CL2'NA'                                                    
         DC    X'D6',CL2'NP'      SOURCE = M                                    
         DC    X'39',CL2'NP'                                                    
         DC    X'3A',CL2'NP'                                                    
         DC    X'3B',CL2'NP'                                                    
         DC    X'3C',CL2'NP'                                                    
         DC    X'3D',CL2'NP'                                                    
         DC    X'3E',CL2'NP'                                                    
         DC    X'D7',CL2'NM'      SOURCE = O                                    
         DC    X'41',CL2'NM'                                                    
         DC    X'42',CL2'NM'                                                    
         DC    X'43',CL2'NM'                                                    
         DC    X'44',CL2'NM'                                                    
         DC    X'45',CL2'NM'                                                    
         DC    X'46',CL2'NM'                                                    
         DC    X'D8',CL2'NS'      SOURCE = P                                    
         DC    X'49',CL2'NS'                                                    
         DC    X'4A',CL2'NS'                                                    
         DC    X'4B',CL2'NS'                                                    
         DC    X'4C',CL2'NS'                                                    
         DC    X'4D',CL2'NS'                                                    
         DC    X'4E',CL2'NS'                                                    
         DC    X'D9',CL2'NE'      SOURCE = Q                                    
         DC    X'51',CL2'NE'                                                    
         DC    X'52',CL2'NE'                                                    
         DC    X'53',CL2'NE'                                                    
         DC    X'54',CL2'NE'                                                    
         DC    X'55',CL2'NE'                                                    
         DC    X'56',CL2'NE'                                                    
         DC    X'E3',CL2'SA'      SOURCE = T                                    
         DC    X'59',CL2'SA'                                                    
         DC    X'5A',CL2'SA'                                                    
         DC    X'5B',CL2'SA'                                                    
         DC    X'5C',CL2'SA'                                                    
         DC    X'5D',CL2'SA'                                                    
         DC    X'5E',CL2'SA'                                                    
         DC    X'E4',CL2'SP'      SOURCE = U                                    
         DC    X'61',CL2'SP'                                                    
         DC    X'62',CL2'SP'                                                    
         DC    X'63',CL2'SP'                                                    
         DC    X'64',CL2'SP'                                                    
         DC    X'65',CL2'SP'                                                    
         DC    X'66',CL2'SP'                                                    
         DC    X'E7',CL2'SE'      SOURCE = X                                    
         DC    X'69',CL2'SE'                                                    
         DC    X'6A',CL2'SE'                                                    
         DC    X'6B',CL2'SE'                                                    
         DC    X'6C',CL2'SE'                                                    
         DC    X'6D',CL2'SE'                                                    
         DC    X'6E',CL2'SE'                                                    
         DC    X'FF'                                                            
*        EJECT                                                                  
* TYPTAB DC    CL3'AAA'                                                         
*        DC    CL3'BAP'                                                         
*        DC    CL3'CAT'                                                         
*        DC    CL3'DAS'                                                         
*        DC    CL3'EAE'                                                         
*        DC    CL3'NNA'                                                         
*        DC    CL3'ONP'                                                         
*        DC    CL3'PNM'                                                         
*        DC    CL3'QNS'                                                         
*        DC    CL3'RNE'                                                         
*        DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE OVERRIDE RECORD                                                    
*                                                                               
BLDOVR   NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING OVRREC,R3                                                        
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,33                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   OVRID,=C'O1'                                                     
         MVC   OVRSTA,RINVKSTA                                                  
         CLI   OVRSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   OVRSTA+4,X'40'                                                   
         MVC   OVRINV,RINVKINV                                                  
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,OVRSEFF)                             
         PRINT NOGEN                                                            
*                                                                               
*  SET THE DBLOCK FOR DEMOCON                                                   
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVI   DBSELMED,C'U'                                                    
*                                                                               
*  GET OVERRIDE ELEMENTS                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'DE',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDOVEX                                                          
*                                                                               
         L     R4,12(R1)                                                        
*                                                                               
BLDOV440 CLI   1(R4),12                                                         
         BNE   BLDOV460                                                         
*                                                                               
         XC    DUB,DUB                                                          
         PRINT GEN                                                              
         GOTO1 DEMOCON,DMCB,(0,3(R4)),(6,DUB),DBLOCK                            
         PRINT NOGEN                                                            
         MVC   OVRCAT,DUB                                                       
         CLI   DUB,C'R'                                                         
         BNE   *+10                                                             
         MVC   OVRCAT,DUB+1                                                     
         OC    OVRCAT,BLANKS                                                    
         MVI   OVRVLTYP,C'R'                                                    
         CLI   4(R4),C'R'                                                       
         BE    BLDOV450                                                         
         MVI   OVRVLTYP,C'S'                                                    
         CLI   4(R4),C'S'                                                       
         BE    BLDOV450                                                         
         MVI   OVRVLTYP,C'P'                                                    
         CLI   4(R4),C'P'                                                       
         BE    BLDOV450                                                         
         MVI   OVRVLTYP,C'I'                                                    
BLDOV450 MVC   OVRDEMO,8(R4)                                                    
*                                                                               
         BAS   RE,WRITREC                                                       
*        SEE IF MORE OVERRIDE ELEMENTS EXIST                                    
BLDOV460 ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'DE'                                                      
         BE    BLDOV440                                                         
*                                                                               
BLDOVEX  B     EXXMOD                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE COMMENT RECORD                                                     
*                                                                               
BLDCOMM  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING CMREC,R3                                                         
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVC   CMID,=CL2'C1'                                                    
         MVC   CMSTA,RINVKSTA                                                   
         CLI   CMSTA+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   CMSTA+4,X'40'                                                    
         MVC   CMINVN,RINVKINV                                                  
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,CMEFFS)                              
         EDIT  (2,RINVKTXT),(3,CMTXTN),FILL=0                                   
*  GET FILTER INFORMATION                                                       
         XC    CMBOOK(3),CMBOOK                                                 
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDCM50                                                          
*                                                                               
         L     R6,12(R1)                                                        
         USING RINVFEL,R6                                                       
*                                                                               
         MVC   CMBOOK,RINVFBK                                                   
*  GET BOOK TYPE                                                                
         LA    R1,SVCLST                                                        
BLDCM20  CLC   RINVFBKT,3(R1)                                                   
         BE    BLDCM30                                                          
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCM20                                                          
         DC    H'0'                                                             
BLDCM30  MVC   CMBKTYP,1(R1)                                                    
         CLI   1(R1),C' '                                                       
         BNE   BLDCM50                                                          
         MVI   CMBKTYP,C'A'                                                     
*                                                                               
*  MOVE TEXT OUT                                                                
*                                                                               
BLDCM50  LA    R4,CMTXT1                                                        
         L     R6,AIO                                                           
         LA    R6,34(R6)           POINT R6 TO FIRST ELEMENT                    
         USING RINVTEL,R6                                                       
*                                                                               
         LA    RF,27               RECORD LENGTH                                
*                                                                               
BLDCM100 CLI   0(R6),1                                                          
         BNE   BLDCMEX                                                          
         XC    0(60,R4),0(R4)      CLEAR THE BUFFER OUT                         
         ZIC   R1,RINVTLEN                                                      
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RINVTEXT                                                 
         OC    0(60,R4),BLANKS     SPACE FILL                                   
         LA    R4,60(R4)                                                        
         LA    RF,60(RF)                                                        
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
         ZIC   RE,RINVTLEN                                                      
         AR    R6,RE                                                            
         B     BLDCM100                                                         
*                                                                               
BLDCMEX  STCM  RF,3,RECLEN                                                      
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3,R5,R6                                                         
*                                                                               
*--BUILD THE TRAILER RECORD                                                     
*                                                                               
BLDTRL   NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING TRREC,R3                                                         
*                                                                               
         LA    RE,17                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   TRID,=CL2'T1'                                                    
         MVC   TRSTA,CSTAT                                                      
         CLI   TRSTA+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   TRSTA+4,X'40'                                                    
         EDIT  (4,TAPECNT),(6,TRRECCNT),FILL=0                                  
*                                                                               
BLDTREX  BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
WRITREC  NTR1                                                                   
         L     RE,TAPECNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,TAPECNT                                                       
*                                                                               
         L     R4,AIO3                                                          
         XC    0(4,R4),0(R4)                                                    
         MVC   0(2,R4),RECLEN                                                   
         PRINT GEN                                                              
         L     R2,ATAPEDCB                                                      
         PUT   (R2),(R4)                                                        
         PRINT NOGEN                                                            
*                                                                               
*****    GOTO1 =V(PRNTBL),DMCB,=C'REC',AIO3,C'DUMP',700,=C'1D'                  
         B     EXXMOD                                                           
         EJECT                                                                  
HOOK     NTR1                                                                   
         L     R6,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R6                                                        
*                                                                               
         LA    RE,H8                                                            
         LA    R1,H9                                                            
         LA    RF,15                                                            
HOOK1    MVC   0(8,RE),=CL8'STA     '                                           
         MVC   0(8,R1),=CL8'_____   '                                           
         LA    RE,8(RE)                                                         
         LA    R1,8(R1)                                                         
         BCT   RF,HOOK1                                                         
         B     EXXMOD                                                           
         DROP  R6                                                               
*                                                                               
QTRTAB   DC    XL1'03',C'01'                                                    
         DC    XL1'06',C'02'                                                    
         DC    XL1'09',C'03'                                                    
         DC    XL1'0C',C'04'                                                    
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         LTORG                                                                  
*                                                                               
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,053,C'LTRANS STATION REQUEST LIST'                            
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,053,27C'-'                                                    
         PSPEC H2,099,RUN                                                       
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
*                                                                               
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
*                                                                               
STADEMS  DC    X'00D97A'           CH2-11                                       
         DC    X'00D97B'           CH6-11                                       
         DC    X'00D941'           WW18+                                        
         DC    X'00D97F'           A2+ (V2+)                                    
         DC    X'00D919'           W12-17                                       
         DC    X'00D91C'           W12-24                                       
         DC    X'00D92D'           W18+                                         
         DC    X'00D929'           W18-34                                       
         DC    X'00D92A'           W18-49                                       
         DC    X'00D947'           W21-49                                       
         DC    X'00D92F'           W25-49                                       
         DC    X'00D930'           W25-54                                       
         DC    X'00D931'           W25-64                                       
         DC    X'00D935'           W35-64                                       
         DC    X'00D95F'           M18+                                         
         DC    X'00D95B'           M18-34                                       
         DC    X'00D95C'           M18-49                                       
         DC    X'00D973'           M21-49                                       
         DC    X'00D961'           M25-49                                       
         DC    X'00D962'           M25-54                                       
         DC    X'00D963'           M25-64                                       
         DC    X'00D967'           M35-64                                       
         DC    X'00D97D'           TN12-17                                      
         DC    X'00D980'           A12-24 (V12-24)                              
         DC    X'00D981'           A12-34 (V12-34)                              
         DC    X'00D991'           A18+                                         
         DC    X'00D98D'           A1834                                        
         DC    X'00D98E'           A1849                                        
         DC    X'00D9BF'           A2149                                        
         DC    X'00D993'           A25-49                                       
         DC    X'00D994'           A25-54                                       
         DC    X'00D995'           A25-64                                       
         DC    X'00D999'           A35-64                                       
*                                                                               
         DC    X'00E37A'           CH2-11                                       
         DC    X'00E37B'           CH6-11                                       
         DC    X'00E341'           WW18+                                        
         DC    X'00E37F'           A2+ (V2+)                                    
         DC    X'00E319'           W12-17                                       
         DC    X'00E31C'           W12-24                                       
         DC    X'00E32D'           W18+                                         
         DC    X'00E329'           W18-34                                       
         DC    X'00E32A'           W18-49                                       
         DC    X'00E347'           W21-49                                       
         DC    X'00E32F'           W25-49                                       
         DC    X'00E330'           W25-54                                       
         DC    X'00E331'           W25-64                                       
         DC    X'00E335'           W35-64                                       
         DC    X'00E35F'           M18+                                         
         DC    X'00E35B'           M18-34                                       
         DC    X'00E35C'           M18-49                                       
         DC    X'00E373'           M21-49                                       
         DC    X'00E361'           M25-49                                       
         DC    X'00E362'           M25-54                                       
         DC    X'00E363'           M25-64                                       
         DC    X'00E367'           M35-64                                       
         DC    X'00E37D'           TN12-17                                      
         DC    X'00E380'           A12-24 (V12-24)                              
         DC    X'00E381'           A12-34 (V12-34)                              
         DC    X'00E391'           A18+                                         
         DC    X'00E38D'           A1834                                        
         DC    X'00E38E'           A1849                                        
         DC    X'00E3BF'           A2149                                        
         DC    X'00E393'           A25-49                                       
         DC    X'00E394'           A25-54                                       
         DC    X'00E395'           A25-64                                       
         DC    X'00E399'           A35-64                                       
*                                                                               
         DC    X'00E301'           THOMES                                       
         DC    X'00D901'           RHOMES                                       
         DC    X'00E201'           SHOMES                                       
         DC    X'00D701'           PHOMES                                       
         DC    X'00D902'           RMETRO-A                                     
         DC    X'00E202'           SMETRO-A                                     
         DC    X'00D702'           PMETRO-A                                     
         DC    X'00D903'           RMETRO-B                                     
         DC    X'00E203'           SMETRO-B                                     
         DC    X'00D703'           PMETRO-B                                     
         DC    X'FF'                                                            
*                                                                               
MKTDEMS  DC    X'00D77A'           CH2-11                                       
         DC    X'00D77B'           CH6-11                                       
         DC    X'00D741'           WW18+                                        
         DC    X'00D77F'           A2+ (V2+)                                    
         DC    X'00D719'           W12-17                                       
         DC    X'00D71C'           W12-24                                       
         DC    X'00D72D'           W18+                                         
         DC    X'00D729'           W18-34                                       
         DC    X'00D72A'           W18-49                                       
         DC    X'00D747'           W21-49                                       
         DC    X'00D72F'           W25-49                                       
         DC    X'00D730'           W25-54                                       
         DC    X'00D731'           W25-64                                       
         DC    X'00D735'           W35-64                                       
         DC    X'00D75F'           M18+                                         
         DC    X'00D75B'           M18-34                                       
         DC    X'00D75C'           M18-49                                       
         DC    X'00D773'           M21-49                                       
         DC    X'00D761'           M25-49                                       
         DC    X'00D762'           M25-54                                       
         DC    X'00D763'           M25-64                                       
         DC    X'00D767'           M35-64                                       
         DC    X'00D77D'           TN12-17                                      
         DC    X'00D780'           A12-24 (V12-24)                              
         DC    X'00D781'           A12-34 (V12-34)                              
         DC    X'00D791'           A18+                                         
         DC    X'00D78D'           A1834                                        
         DC    X'00D78E'           A1849                                        
         DC    X'00D7BF'           A2149                                        
         DC    X'00D793'           A25-49                                       
         DC    X'00D794'           A25-54                                       
         DC    X'00D795'           A25-64                                       
         DC    X'00D799'           A35-64                                       
*                                                                               
         DC    X'00D87A'           CH2-11                                       
         DC    X'00D87B'           CH6-11                                       
         DC    X'00D841'           WW18+                                        
         DC    X'00D87F'           A2+ (V2+)                                    
         DC    X'00D819'           W12-17                                       
         DC    X'00D81C'           W12-24                                       
         DC    X'00D82D'           W18+                                         
         DC    X'00D829'           W18-34                                       
         DC    X'00D82A'           W18-49                                       
         DC    X'00D847'           W21-49                                       
         DC    X'00D82F'           W25-49                                       
         DC    X'00D830'           W25-54                                       
         DC    X'00D831'           W25-64                                       
         DC    X'00D835'           W35-64                                       
         DC    X'00D85F'           M18+                                         
         DC    X'00D85B'           M18-34                                       
         DC    X'00D85C'           M18-49                                       
         DC    X'00D873'           M21-49                                       
         DC    X'00D861'           M25-49                                       
         DC    X'00D862'           M25-54                                       
         DC    X'00D863'           M25-64                                       
         DC    X'00D867'           M35-64                                       
         DC    X'00D87D'           TN12-17                                      
         DC    X'00D880'           A12-24 (V12-24)                              
         DC    X'00D881'           A12-34 (V12-34)                              
         DC    X'00D891'           A18+                                         
         DC    X'00D88D'           A1834                                        
         DC    X'00D88E'           A1849                                        
         DC    X'00D8BF'           A2149                                        
         DC    X'00D893'           A25-49                                       
         DC    X'00D894'           A25-54                                       
         DC    X'00D895'           A25-64                                       
         DC    X'00D899'           A35-64                                       
*                                                                               
         DC    X'00D801'           THOMES                                       
         DC    X'00D701'           RHOMES                                       
         DC    X'00D701'           SHOMES                                       
         DC    X'00D701'           PHOMES                                       
         DC    X'00D702'           RMETRO-A                                     
         DC    X'00D702'           SMETRO-A                                     
         DC    X'00D702'           PMETRO-A                                     
         DC    X'00D703'           RMETRO-B                                     
         DC    X'00D703'           SMETRO-B                                     
         DC    X'00D703'           PMETRO-B                                     
         DC    X'FF'                                                            
         EJECT                                                                  
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 2                                                                
       ++INCLUDE RESVCTAB                                                       
*                                                                               
*                                                                               
*  LAYOUT FOR TAPE HEADER                                                       
HDREC    DSECT                                                                  
HDID     DS    CL2                 ID (H1)                                      
HDSRC    DS    CL4                 TAPE SOURCE                                  
HDVER    DS    CL2                 VERSION NUMBER                               
HDREP    DS    CL2                 REP CODE                                     
HDSVDAT  DS    CL6                 SAVE DATE                                    
HDSVTIM  DS    CL8                 SAVE TIME                                    
*                                                                               
*  LAYOUT FOR DAYPART RECORD                                                    
DPTREC   DSECT                                                                  
DPID     DS    CL2                 ID (D1)                                      
DPCODE   DS    CL1                 DAYPART CODE                                 
DPSNAME  DS    CL4                 DAYPART SHORT NAME                           
DPLNAME  DS    CL16                DAYPART LONG NAME                            
*                                                                               
*  LAYOUT FOR INVENTORY MASTER                                                  
IMREC    DSECT                                                                  
IMID     DS    CL2                 ID (I1)                                      
IMSTA    DS    CL5                 STATION                                      
IMINVN   DS    CL4                 INVENTORY NUMBER                             
IMEFFS   DS    CL6                 EFFECTIVE START DATE                         
IMEFFE   DS    CL6                 EFFECTIVE END DATE                           
IMDAY1   DS    CL11                DAY 1                                        
IMSTIM1  DS    CL11                TIME 1                                       
IMDAY2   DS    CL11                DAY 2                                        
IMSTIM2  DS    CL11                TIME 2                                       
IMDAY3   DS    CL11                DAY 3                                        
IMSTIM3  DS    CL11                TIME 3                                       
IMDAY4   DS    CL11                DAY 4                                        
IMSTIM4  DS    CL11                TIME 4                                       
IMDAY5   DS    CL11                DAY 5                                        
IMSTIM5  DS    CL11                TIME 5                                       
IMDAY6   DS    CL11                DAY 6                                        
IMSTIM6  DS    CL11                TIME 6                                       
IMDAY7   DS    CL11                DAY 7                                        
IMSTIM7  DS    CL11                TIME 7                                       
IMDAY8   DS    CL11                DAY 8                                        
IMSTIM8  DS    CL11                TIME 8                                       
IMPROG1  DS    CL27                PROGRAM TITLE 1                              
IMPROG2  DS    CL27                PROGRAM TITLE 2                              
IMPROG3  DS    CL27                PROGRAM TITLE 3                              
IMPROG4  DS    CL27                PROGRAM TITLE 4                              
IMPROG5  DS    CL27                PROGRAM TITLE 5                              
IMPROG6  DS    CL27                PROGRAM TITLE 6                              
IMPROG7  DS    CL27                PROGRAM TITLE 7                              
IMPROG8  DS    CL27                PROGRAM TITLE 8                              
IMDYPTS  DS    CL6                 DAYPARTS                                     
IMFLTER  DS    CL6                 FILTERS                                      
IMMARKET DS    CL20                MARKET                                       
IMPOWER  DS    CL2                 AGENCY POWER CODE                            
IMCOMPST DS    CL1                 COMPETITIVE STATION                          
IMTIMCHG DS    CL3                 TIME CHANGE                                  
*                                                                               
*  LAYOUT FOR SURVEY RECORD                                                     
SRVREC   DSECT                                                                  
SRVID    DS    CL2                 ID (S1)                                      
SRVSTA   DS    CL5                 STATION                                      
SRVINV   DS    CL4                 INVENTORY NUMBER                             
SRVSEFF  DS    CL6                 EFFECTIVE START DATE                         
SRVSRC   DS    CL1                 SOURCE                                       
SRVBOOK  DS    CL2                 BOOK                                         
SRVLOOK  DS    CL1                 LOOKUP TYPE (T,P,A,S,E)                      
SRVDAY1  DS    CL11                DAY 1                                        
SRVSTM1  DS    CL11                TIME 1                                       
SRVDAY2  DS    CL11                DAY 2                                        
SRVSTM2  DS    CL11                TIME 2                                       
SRVDAY3  DS    CL11                DAY 3                                        
SRVSTM3  DS    CL11                TIME 3                                       
SRVDAY4  DS    CL11                DAY 4                                        
SRVSTM4  DS    CL11                TIME 4                                       
SRVDAY5  DS    CL11                DAY 5                                        
SRVSTM5  DS    CL11                TIME 5                                       
SRVDAY6  DS    CL11                DAY 6                                        
SRVSTM6  DS    CL11                TIME 6                                       
SRVDAY7  DS    CL11                DAY 7                                        
SRVSTM7  DS    CL11                TIME 7                                       
SRVDAY8  DS    CL11                DAY 8                                        
SRVSTM8  DS    CL11                TIME 8                                       
*  STATION LEVEL DEMOS                                                          
STPRAT   DS    CL66                PEOPLE RATINGS 33X2                          
STPIMP   DS    CL132               PEOPLE IMPS 33X4                             
STTHMS   DS    CL4                 TOTAL HOMES 000'S                            
STHLDRTG DS    CL2                 HOUSEHOLD RATINGS                            
STHLDSHR DS    CL2                 HOUSEHOLD SHARES                             
STHLDHUT DS    CL2                 HOUSEHOLD HUTS                               
STMTARTG DS    CL2                 METRO-A RATINGS                              
STMTASHR DS    CL2                 METRO-A SHARE                                
STMTAHUT DS    CL2                 METRO-A HUTS                                 
STMTBRTG DS    CL2                 METRO-B RATINGS                              
STMTBSHR DS    CL2                 METRO-B SHARE                                
STMTBHUT DS    CL2                 METRO-B HUTS                                 
*  MARKET LEVEL DEMOS                                                           
MKPRAT   DS    CL66                PEOPLE RATINGS 33X2                          
MKPIMP   DS    CL132               PEOPLE IMPS 33X4                             
MKTHMS   DS    CL4                 TOTAL HOMES 000'S                            
MKHLDRTG DS    CL2                 HOUSEHOLD RATINGS                            
MKHLDSHR DS    CL2                 HOUSEHOLD SHARES                             
MKHLDHUT DS    CL2                 HOUSEHOLD HUTS                               
MKMTARTG DS    CL2                 METRO-A RATINGS                              
MKMTASHR DS    CL2                 METRO-A SHARE                                
MKMTAHUT DS    CL2                 METRO-A HUTS                                 
MKMTBRTG DS    CL2                 METRO-B RATINGS                              
MKMTBSHR DS    CL2                 METRO-B SHARE                                
MKMTBHUT DS    CL2                 METRO-B HUTS                                 
*                                                                               
SRVPROG  DS    CL27                PROGRAM INFO                                 
SRVBRCD  DS    CL2                 PROGRAM/BREAK CODE                           
SRVFRBK  DS    CL2                 FROM BOOK                                    
SRVFRTP  DS    CL1                 FROM TYPE                                    
SRVFRFN  DS    CL1                 FROM FUNCTION                                
SRVFRST  DS    CL5                 FROM STATION                                 
SRVUPGF  DS    CL70                UPGRADE FORMULA                              
SRVBTYP  DS    CL1                 BOOK TYPE                                    
*                                                                               
*  LAYOUT FOR OVERRIDE RECORD                                                   
OVRREC   DSECT                                                                  
OVRID    DS    CL2                 ID (O1)                                      
OVRSTA   DS    CL5                 STATION                                      
OVRINV   DS    CL4                 INVENTORY NUMBER                             
OVRSEFF  DS    CL6                 EFFECTIVE START DATE                         
OVRCAT   DS    CL7                 CATEGORY NAME                                
OVRVLTYP DS    CL1                 VALUE TYPE R,I                               
OVRDEMO  DS    CL4                 DEMO AMOUNT                                  
*                                                                               
*  LAYOUT FOR COMMENT RECORD                                                    
CMREC    DSECT                                                                  
CMID     DS    CL2                 RECORD ID (C1)                               
CMSTA    DS    CL5                 STATION                                      
CMINVN   DS    CL4                 INVENTORY NUMBER                             
CMEFFS   DS    CL6                 EFFECTIVE START DATE                         
CMTXTN   DS    CL3                 TEXT NUMBER                                  
CMBOOK   DS    CL2                 BOOK                                         
CMBKTYP  DS    CL1                 BOOK TYPE                                    
CMTXT1   DS    CL60                COMMENT TEXT (UP TO 40 LINES)                
*                                                                               
*  LAYOUT FOR TRAILER RECORD                                                    
TRREC    DSECT                                                                  
TRID     DS    CL2                 RECORD ID (T1)                               
TRSTA    DS    CL5                 STATION                                      
TRRECCNT DS    CL6                 RECORD COUNT                                 
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPECD                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE REGENRDP                                                       
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
*                                                                               
TAPECNT  DS    F                   NUMBER OF RECORDS                            
ATAPEDCB DS    F                   ADDRESS OF DCB                               
ASAVE    DS    F                   BYTE SAVE AREA                               
ASTATAB  DS    F                   4000 BYTE COVAIL START ADDRESS               
ASTATABX DS    F                   4000 BYTE COVAIL END ADDRESS                 
*                                                                               
STRTOPT  DS    CL3                 START DATE                                   
STRTOPTC DS    CL2                 START DATE COMPRESSED                        
ENDOPT   DS    CL3                 END DATE                                     
ENDOPTC  DS    CL2                                                              
SAVPROG  DS    CL27                PROGRAM NAME                                 
COMPSTA  DS    CL1                 COMPETITIVE STATION (Y,N)                    
*                                                                               
DBEXTRA1 DS    CL128               DBLOCK EXTRA LENGTH                          
RECLEN   DS    H                   RECORD LENGTH                                
*                                                                               
BLANKS   DS    CL132               SPACE FILLED FIELD                           
*                                                                               
DAYTMHD  DS    XL40                HEADER DAY TIME HOLE                         
*                                                                               
UPGRADST DS    XL132               UPGRADE EXPRESSION HOLD AREA                 
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'187RERMP1C   01/11/11'                                      
         END                                                                    
