*          DATA SET SPSFM13    AT LEVEL 037 AS OF 05/01/02                      
*PHASE T21713A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T21713 - MARKET TRANSLATION RECORD MAINTENANCE        *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST, *         
*               AND REPORT                                            *         
*                                                                     *         
*  INPUTS       SCREEN T217B3 (MAINTENANCE)                           *         
*               SCREEN T217A3 (LIST)                                  *         
*               SCREEN T217C3 (REPORT)                                *         
*                                                                     *         
*  OUTPUTS      UPDATED MTR RECORDS                                   *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- MTRRECD                                         *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- SPOOL                                           *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - MTR RECORD                                      *         
*               IO2 - OTHER RECORDS                                   *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21713 - MARKET TRANSLATION RECORDS'                            
T21713   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21713                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         USING MTRKEY,R4                                                        
         ST    RD,SAVERD                                                        
*                                                                               
         USING T21713,RB,R7                                                     
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       XC    KEY,KEY             BUILD MTR KEY                                
         XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY                                                         
         MVC   MTRKTYPE(2),=X'0D46'                                             
*                                                                               
         LA    R2,SFMMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 VALIMED                                                          
         MVC   MTRKAM,BAGYMD                                                    
*                                                                               
         LA    R2,SFMSRCH          RATING SOURCE                                
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTEQU,ACTLIST      OPTIONAL FOR LIST                            
         BE    VKX                                                              
         CLI   ACTEQU,ACTREP       OPTIONAL FOR REPORT                          
         BE    VKX                                                              
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK10     ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'ARB'     TEST ARBITRON                                
         BNE   *+12                                                             
         MVI   MTRKRTG,C'A'                                                     
         B     VK20                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'NSI'     TEST NIELSON                                 
         BE    *+12                YES                                          
         MVI   ERROR,INVSRC        NO - INVALID                                 
         B     TRAPERR                                                          
         MVI   MTRKRTG,C'N'                                                     
*                                                                               
VK20     CLI   ACTEQU,ACTLIST      NO MORE FIELDS ON LIST/REPORT                
         BE    VKX                                                              
         CLI   ACTEQU,ACTREP                                                    
         BE    VKX                                                              
*                                                                               
         LA    R2,SFMRSMH          RATING SERVICE MARKET NO.                    
         XC    SFMRSMN,SFMRSMN     CLEAR RATING SERVICE MARKET NAME             
         OI    SFMRSMNH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0             REQUIRED FIELD                               
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   R1,5(R2)            FIGURE OUT BINARY CODE                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SFMRSM(0)                                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,MTRKRSMK                                                    
         MVC   SOURCE,SFMSRC                                                    
         MVC   BRSMNUM,MTRKRSMK                                                 
         BAS   RE,VRSM                                                          
*                                                                               
         CLC   =C'**',WORK         TEST VALID MARKET NAME                       
         BNE   *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
         MVC   SFMRSMN,WORK                                                     
*                                                                               
VKX      MVC   KEY(13),SVKEY       MTR KEY                                      
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,X'05'        TRANSLATION ELEMENTS                         
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SFMMKTH          FIRST AGENCY MARKET FIELD                    
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         BAS   RE,VMKT             VALIDATE MARKET FIELD                        
*                                                                               
         LA    R6,ELEM             BUILD TRANSLATION ELEMENT IN ELEM            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'05'                                                       
         MVI   ELEM+1,6                                                         
         MVC   ELEM+4(2),BMKT      AGENCY MARKET NO.                            
         GOTO1 ADDELEM             'ALL' CLIENT MKTNO                           
         B     VR30                                                             
*                                                                               
VR10     CLI   5(R2),0             TEST CLIENT GIVEN                            
         BNE   VR20                YES                                          
         ZIC   R0,0(R2)            BUMP TO MARKET NO. FIELD                     
         AR    R2,R0                                                            
         B     VR30                                                             
*                                                                               
VR20     BAS   RE,VCLT             VALIDATE CLIENT CODE                         
         ZIC   R0,0(R2)            BUMP TO MARKET NO. FIELD                     
         AR    R2,R0                                                            
         BAS   RE,VMKT             VALIDATE MARKET FIELD                        
*                                                                               
         LA    R6,ELEM             BUILD TRANSLATION ELEMENT IN ELEM            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'05'                                                       
         MVI   ELEM+1,6                                                         
         MVC   ELEM+2(2),BCLT      CLIENT CODE                                  
         MVC   ELEM+4(2),BMKT      AGENCY MARKET NO.                            
         GOTO1 ADDELEM             'ALL' CLIENT MKTNO                           
*                                                                               
VR30     ZIC   R0,0(R2)            MARKET NAME FIELD                            
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            SUPPOSEDLY, THE NEXT CLIENT FIELD            
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST END OF SCREEN                           
         BNE   VR10                NO                                           
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,SFMMKTH          CLEAR SCREEN                                 
*                                                                               
DR10     ZIC   RF,0(R2)            RF HAS LENGTH OF FIELD                       
         SH    RF,=H'9'            SUBTRACT HEADER LENGTH +1                    
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
*                                                                               
         EX    RF,*+8              PAD WITH BLANKS                              
         B     *+10                                                             
         OC    8(0,R2),=80X'40'                                                 
         EX    RF,*+8              TEST FIELD EMPTY                             
         B     *+10                                                             
         CLC   8(0,R2),=80X'40'                                                 
         BE    DR20                YES                                          
         EX    RF,*+8              NO - CLEAR FIELD                             
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
DR20     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   DR10                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        MARKET TRANSLATION ELEMENT                   
         BAS   RE,GETEL            FIRST ONE (FOR 'ALL' CLIENT)                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SFMMKTH          MARKET NO. FIELD                             
         EDIT  (B2,4(R6)),(4,SFMMKT),FILL=0                                     
         OI    SFMMKTH+6,X'80'     XMIT                                         
         MVC   QMKT,SFMMKT                                                      
         BAS   RE,VMKT2            GET MARKET NAME                              
         MVC   SFMMKTN,MKTNM                                                    
         OI    SFMMKTNH+6,X'80'    XMIT                                         
         LA    R2,SFMCLTH                                                       
*                                                                               
DR30     BAS   RE,NEXTEL           LOOK FOR MORE TRANSLATION ELEMENTS           
         BNE   DRX                                                              
*                                                                               
         GOTO1 CLUNPK,DMCB,2(R6),8(R2)                                          
         OI    6(R2),X'80'         XMIT CLIENT                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         EDIT  (B2,4(R6)),(4,8(R2)),FILL=0                                      
         MVC   QMKT,8(R2)                                                       
         OI    6(R2),X'80'         XMIT MARKET NO.                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         BAS   RE,VMKT2            GET MARKET NAME                              
         MVC   8(24,R2),MKTNM                                                   
         OI    6(R2),X'80'         XMIT MKT NAME                                
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR30                                                             
*                                                                               
DRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING MTRKEY,R6                                                        
*                                                                               
         OI    SFMMEDH+6,X'80'     XMIT MEDIA                                   
         MVC   SFMMED,QMED                                                      
*                                                                               
         OI    SFMSRCH+6,X'80'     XMIT RATING SOURCE                           
         CLI   MTRKRTG,C'A'        TEST ARBITRON                                
         BNE   *+14                                                             
         MVC   SFMSRC,=C'ARB'                                                   
         B     DK10                                                             
*                                                                               
         CLI   MTRKRTG,C'N'        TEST NIELSON                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SFMSRC,=C'NSI'                                                   
*                                                                               
DK10     OI    SFMRSMH+6,X'80'     XMIT RATING SERVICE MARKET                   
         EDIT  (B2,MTRKRSMK),(3,SFMRSM),ALIGN=LEFT                              
*                                                                               
         OI    SFMRSMNH+6,X'80'    XMIT RATING SERVICE MARKET NAME              
         MVC   SOURCE,SFMSRC                                                    
         MVC   BRSMNUM,MTRKRSMK                                                 
         BAS   RE,VRSM                                                          
*                                                                               
         CLC   =C'**',WORK         TEST VALID MARKET NAME                       
         BNE   *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
         MVC   SFMRSMN,WORK                                                     
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
         USING MTRKEY,R6                                                        
LR       LA    R6,KEY                                                           
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                NO                                           
         MVC   MTRKTYPE,=X'0D46'                                                
         MVC   MTRKAM,BAGYMD                                                    
         MVC   MTRKRTG,SFMSRC                                                   
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R6,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   MTRKTYPE,=X'0D46'   TEST SAME TYPE                               
         BNE   LRX                                                              
         CLC   MTRKAM,BAGYMD       TEST SAME A/M                                
         BNE   LRX                                                              
*                                                                               
         CLI   SFMSRCH+5,0         TEST SOURCE WAS GIVEN                        
         BE    *+14                                                             
         CLC   MTRKRTG,SFMSRC      TEST MATCH ON SOURCE                         
         BNE   LRX                                                              
*                                                                               
         CLI   MTRKRTG,C'A'        TEST ARBITRON                                
         BNE   *+14                                                             
         MVC   LSTSRC,=C'ARB'                                                   
         B     LR40                                                             
*                                                                               
         CLI   MTRKRTG,C'N'        TEST NIELSON                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTSRC,=C'NSI'                                                   
*                                                                               
LR40     EDIT  (B2,MTRKRSMK),(3,LSTRSM)                                         
         MVC   SOURCE,LSTSRC                                                    
         MVC   BRSMNUM,MTRKRSMK                                                 
         BAS   RE,VRSM                                                          
*                                                                               
         CLC   =C'**',WORK         TEST VALID MARKET NAME                       
         BNE   *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
         MVC   LSTRSMN,WORK                                                     
*                                                                               
         GOTO1 GETREC              GET THE MTR RECORD                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        TRANSLATION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                'ALL' CLIENT MUST BE THERE                   
*                                                                               
         EDIT  (B2,4(R6)),(4,LSTMKT),FILL=0                                     
         MVC   QMKT,LSTMKT                                                      
         BAS   RE,VMKT2            GET MARKET NAME                              
         MVC   LSTMKTN,MKTNM                                                    
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   RECFOUND,C'N'                                                    
         MVI   LASTSRC,C' '                                                     
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   MTRKTYPE,=X'0D46'                                                
         MVC   MTRKAM,BAGYMD                                                    
         MVC   MTRKRTG,SFMSRC                                                   
*                                                                               
PR10     GOTO1 HIGH                FIRST RECORD                                 
         B     PR30                                                             
*                                                                               
PR20     LA    R6,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   MTRKTYPE,=X'0D46'   TEST SAME TYPE                               
         BNE   PRX                                                              
         CLC   MTRKAM,BAGYMD       TEST SAME A/M                                
         BNE   PRX                                                              
*                                                                               
         CLI   SFMSRCH+5,0         TEST SOURCE WAS GIVEN                        
         BE    *+18                                                             
         CLC   MTRKRTG,SFMSRC      TEST MATCH ON SOURCE                         
         BE    PR40                                                             
         B     PRX                                                              
*                                                                               
         CLC   LASTSRC,MTRKRTG     TEST CHANGE ON SOURCE                        
         BE    PR40                                                             
*                                                                               
         MVC   LASTSRC,MTRKRTG                                                  
         MVI   LINE,99             EJECT PAGE ON CHANGE OF SOURCE               
*                                                                               
PR40     EDIT  (B2,MTRKRSMK),(3,PRTRSM)                                         
         MVC   SOURCE,MTRKRTG                                                   
         MVC   BRSMNUM,MTRKRSMK                                                 
         BAS   RE,VRSM                                                          
*                                                                               
         CLC   =C'**',WORK         TEST VALID MARKET NAME                       
         BNE   *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
         MVC   PRTRSMN,WORK                                                     
*                                                                               
         GOTO1 GETREC              GET THE MTR RECORD                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        TRANSLATION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                'ALL' CLIENT MUST BE THERE                   
         MVI   RECFOUND,C'Y'                                                    
         MVC   PRTCLT,=C'ALL'                                                   
*                                                                               
PR50     EDIT  (B2,4(R6)),(4,PRTMKT),FILL=0                                     
         MVC   QMKT,PRTMKT                                                      
         BAS   RE,VMKT2            GET MARKET NAME                              
         MVC   PRTMKTN,MKTNM                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PR20                                                             
*                                                                               
         GOTO1 CLUNPK,DMCB,2(R6),PRTCLT                                         
         B     PR50                                                             
*                                                                               
PRX      CLI   RECFOUND,C'Y'                                                    
         BE    XIT                                                              
*                                                                               
         MVC   P+2(16),=C'NO RECORDS FOUND'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
HDHK     NTR1                                                                   
*                                                                               
         MVC   H1+9(1),QMED        MEDIA                                        
*                                                                               
         CLI   SOURCE,C'A'         TEST ARBITRON                                
         BNE   *+14                                                             
         MVC   H2+9(3),=C'ARB'                                                  
         B     HDHKX                                                            
*                                                                               
         CLI   SOURCE,C'N'         TEST NIELSON                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   H2+9(3),=C'NSI'                                                  
*                                                                               
HDHKX    B     XIT                                                              
         SPACE 5                                                                
HEADING  SSPEC H1,3,C'MEDIA'                                                    
         SSPEC H1,37,C'MARKET TRANSLATION RECORD REPORT'                        
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'SOURCE'                                                   
         SSPEC H2,37,C'--------------------------------'                        
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,3,REPORT                                                      
         SSPEC H3,15,RUN                                                        
         SSPEC H3,73,REQUESTOR                                                  
         SSPEC H3,99,PAGE                                                       
         SSPEC H5,3,C'RATING SERVICE MARKET'                                    
         SSPEC H5,35,C'CLT   AGENCY MARKET'                                     
         SSPEC H6,3,C'---------------------'                                    
         SSPEC H6,35,C'---   -------------'                                     
         DC    X'00'                                                            
         EJECT                                                                  
* VALIDATE MARKET                                                               
*                                                                               
VMKT     NTR1                                                                   
*                                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   R1,5(R2)            FIGURE OUT BMKT AND QMKT                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKT                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         B     VMKT10                                                           
*                                                                               
VMKT2    NTR1                                                                   
*                                                                               
VMKT10   L     R3,AIO2             BUILD MARKET KEY                             
         USING MKTRECD,R3                                                       
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,QMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
         MVC   MKTKFILL,=C'000000000'                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',MKTKEY,AIO2                  
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST MARKET RECORD FOUND                     
         BZ    *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
         MVC   MKTNM,MKTNAME       MARKET NAME                                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* VALIDATE CLIENT                                                               
*                                                                               
VCLT     NTR1                                                                   
*                                                                               
         MVC   QCLT,8(R2)                                                       
         OC    QCLT,=C'   '        PAD CLIENT FIELD WITH BLANKS                 
         CLI   5(R2),2                                                          
         BNL   *+12                                                             
         MVI   ERROR,INVCLI                                                     
         BL    TRAPERR                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
         MVI   ERROR,INVCLI                                                     
         BL    TRAPERR                                                          
*                                                                               
         XC    SVKEY,SVKEY         BUILD CLIENT HEADER KEY                      
         MVC   SVKEY+1(1),BAGYMD                                                
         MVC   SVKEY+2(2),BCLT                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'SPTDIR',SVKEY,SVKEY                   
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST CLIENT RECORD FOUND                     
         BZ    *+12                                                             
         MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RATING SERVICE MARKET NO.                                            
*                                                                               
VRSM     NTR1                                                                   
*                                                                               
         XC    BLOCK(256),BLOCK    VALIDATE RATING SVC MARKET CODE              
         LA    R5,BLOCK                                                         
         USING DBLOCK,R5                                                        
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBAREC,AIO2                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELMED,QMED                                                    
         MVC   DBSELSRC,SOURCE                                                  
         MVC   DBSELRMK,BRSMNUM                                                 
*                                                                               
         L     RE,ACOMFACS                                                      
         ST    RE,DBCOMFCS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CDEMAND                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,BLOCK,DHOOK                                            
         CLI   DBERROR,0                                                        
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
         SPACE 5                                                                
DHOOK    NTR1                                                                   
*                                                                               
         L     R6,AIO2             A(DEMO RECORD)                               
         USING DMKEY,R6                                                         
         LA    R6,DMFRSTEL         A(FIRST ELEMENT)                             
         MVI   ELCODE,DMECODEQ     MARKET NAME ELEMENT                          
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING DMELEM,R6                                                        
         XC    WORK,WORK           GET MARKET NAME                              
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),DMMNAME                                                  
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
TRAPERR  L     RD,SAVERD                                                        
         GOTO1 ERREX                                                            
         SPACE 3                                                                
         LTORG                                                                  
         PRINT OFF                                                              
         DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB3D                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 4                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SAVERD   DS    A                                                                
SOURCE   DS    C                                                                
LASTSRC  DS    C                                                                
BRSMNUM  DS    XL2                                                              
RECFOUND DS    C                                                                
         EJECT                                                                  
MTRRECD  DSECT                                                                  
       ++INCLUDE SPGENMTR                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSRC   DS    CL3                                                              
         DS    CL2                                                              
LSTRSM   DS    CL3                                                              
         DS    CL2                                                              
LSTRSMN  DS    CL24                                                             
         DS    CL2                                                              
LSTMKT   DS    CL4                                                              
         DS    CL2                                                              
LSTMKTN  DS    CL24                                                             
         SPACE 3                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PRTRSM   DS    CL3                                                              
         DS    CL2                                                              
PRTRSMN  DS    CL24                                                             
         DS    CL3                                                              
PRTCLT   DS    CL3                                                              
         DS    CL3                                                              
PRTMKT   DS    CL4                                                              
         DS    CL2                                                              
PRTMKTN  DS    CL24                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPSFM13   05/01/02'                                      
         END                                                                    
