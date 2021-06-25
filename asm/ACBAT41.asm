*          DATA SET ACBAT41    AT LEVEL 073 AS OF 04/16/13                      
*PHASE T61B41A                                                                  
         TITLE 'OVERLAY FOR GST/PST TYPES AND AMOUNTS'                          
T61B41   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,T61B41,R8,CLEAR=YES                                    
         USING LWSD,RC                                                          
*                                                                               
         MVC   BATTYPE,0(R1)       GET BATCH TYPE FROM PARAMETER                
         MVI   0(R1),0                                                          
         MVC   AAMNTS,12(R1)       ADDRESS OF AMOUNTS                           
         MVC   AAMNTS2,16(R1)      ADDRESS OF AMOUNTS                           
*                                                                               
         L     R9,4(R1)                                                         
         USING GWS,R9              R9=GLOBAL W/S                                
         L     RA,ATWA0                                                         
         USING TWAD,RA             RA=TWA                                       
         L     RE,8(R1)            ADDRESS OF WORKCODE TABLE                    
         MVC   AMTBLKT,0(RE)       SAVE IT HERE                                 
         ST    RE,ADWCTAB                                                       
         L     RE,0(R1)            A(CANADA TAX BLOCK)                          
         ST    RE,ADTAXB                                                        
*                                                                               
         GOTO1 VDICTAT,DMCB,C'L   ',DICI,DICO                                   
*                                                                               
         L     RE,ADTAXB           READDRESS RE                                 
*                                                                               
MAIN     CLI   0(RE),C'B'                                                       
         BE    INIT10              FIRST TIME - MUST INITIALIZE                 
         CLI   0(RE),C'G'          GO THROUGH ALL IN 1 PASS                     
         BE    INIT10                                                           
         CLI   PFKEY,15            PF15, NO MORE INPUT - RETURN                 
         BE    TAX10                 BUT VALIDATE AGAIN                         
         CLI   PFKEY,10                                                         
         BE    NEXT10              PF=10 REFRESH SCREEN FOR NEXT                
         CLI   PFKEY,0                                                          
         BE    TAX10               PF=0 'ENTER' - OK TO EDIT                    
BADPFK   LA    R2,CONACTH                                                       
         MVI   ERRNUM,251          ERROR INVALID PF KEY                         
         B     EXIT                                                             
*                                                                               
PEXIT    CLI   BATTYPE,21          TYPE 21 AND 26 ONLY                          
         BE    PEXIT2                                                           
         CLI   BATTYPE,26                                                       
         BNE   EXIT                                                             
PEXIT2   CLI   PFKEY,15                                                         
         BE    END10                                                            
         B     EXIT                                                             
         EJECT                                                                  
*              SAVE THE CURRENT SCREEN TWA0 IN TWA3                             
*                                                                               
INIT10   LA    RF,CTXDATA                                                       
         LA    R1,CTXLNQ                                                        
         MOVE  ((RF),(R1)),(RE)       SAVE CANADA TAX BLOCK                     
         GOTO1 ANTRSES,0                                                        
*                                                                               
*              GET CANADIAN TAX SCREEN                                          
*                                                                               
         XC    DMCB(20),DMCB                                                    
         MVC   DMCB+4(4),=X'D9061BBF'                                           
         GOTO1 CALLOV,DMCB,(0,CONTABH)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                    CANT READ SCREEN                         
*                                                                               
         LA    RE,CONTABH              TRANSMIT SCREEN                          
         SR    R1,R1                                                            
INIT20   OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   INIT20                                                           
         XC    1(2,RE),1(RE)                                                    
*                                                                               
         ZAP   LASTBASE,=P'0'                                                   
         OI    CSINDSL2,CSIDPFQN   DISABLE RETURN/NEXT PFKEYS                   
*                                                                               
* FILL IN ANY INFO PASSED FROM CALLER                                           
*                                                                               
         MVC   COFFICE,CTXOFF      OFFICE CODE                                  
         MVC   COFFCLST,CTAX46OF   TYPE 46'S OFFICE LIST                        
         MVC   PWOSDATE,CTXDATE    DATE                                         
         MVC   CPROV,CTXPROV       PROVINCE CODE                                
         MVC   PDATDATE,CTXPDATE   PRIMARY DATE FOR TYPE 26                     
         MVC   AMTBLK,AMTBLKT                                                   
         MVC   CNTRACC,CTXCNTRA                                                 
         MVC   CNTRACCN,CTXCNTRN                                                
         MVC   GSTVENDT,CTXVENGT                                                
         MVC   PSTVENDT,CTXVENPT                                                
         MVC   USERPST,CTXUPSTT                                                 
         MVC   USERPROV,CTXUPROV                                                
         MVC   GBASIS,CTXGBASE                                                  
         MVC   PBASIS,CTXPBASE                                                  
         MVC   GINPUT,CTXGINPT                                                  
*                                                                               
         MVC   GSTCGON,CTXGORN     MOVE OVER DATA FROM PREV SCREEN              
         MVI   GSTCGONH+5,1        FAKE LENGTH FOR FIRST TIME IN                
         OI    GSTCGONH+6,X'80'    TRANSMIT THEM                                
         MVC   GSTGSTT,CTXGSTT                                                  
         MVI   GSTGSTTH+5,1                                                     
         OI    GSTGSTTH+6,X'81'                                                 
         MVC   GSTGSTN,CTXGSTTN                                                 
         OI    GSTGSTNH+6,X'80'                                                 
         CLI   CTXLGSTA,0          NO OVERRIDES?                                
         BZ    INIT50              NOTHING TO SHOW                              
         ZIC   R1,CTXLGSTA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GSTGSTA(0),CTXGSTA                                               
         MVC   GSTGSTAH+5(1),CTXLGSTA                                           
         OI    GSTGSTAH+6,X'80'                                                 
*                                                                               
INIT50   OC    CTXPROV,CTXPROV                                                  
         BZ    INIT60                                                           
         CLC   CTXPROV,SPACES                                                   
         BE    INIT60                                                           
         MVC   GSTPSTP,CTXPROV                                                  
         MVI   GSTPSTPH+5,1                                                     
         OI    GSTPSTPH+6,X'80'                                                 
*                                                                               
INIT60   BAS   RE,SHOWXTRA         SHOW WHAT WAS SAVED                          
INIT70   LA    R2,GSTCGONH         FIRST CURSOR                                 
         CLI   8(R2),C' '                                                       
         BNH   INIT72                                                           
         LA    R2,GSTGSTTH         NEXT INPUT                                   
INIT72   MVI   ERRNUM,SPECIAL                                                   
         MVI   MSG,C' '                                                         
         MVC   MSG+1(L'MSG-1),MSG                                               
         MVC   MSG(25),=CL25'INPUT REQUIRED FIELDS'                             
         L     RE,ADTAXB          SCREEN LOADED- OK TO EDIT(NEXT TIME)          
         CLI   0(RE),C'G'                                                       
         BE    *+8                                                              
         MVI   0(RE),C'E'                                                       
         B     TAX10                                                            
         EJECT                                                                  
*                                                                               
SHOWXTRA NTR1                                                                   
         LA    RF,CTXXTELM         CHECK SCREEN ELEMENT SAVED                   
         USING SFSELD,RF                                                        
SHOWLP   CLI   SFSEL,0             EOT?                                         
         BZ    EXIT                                                             
         CLI   SFSEL,SFSELQ        MAKE SURE IT IS A SAVED ELEMENT              
         BNE   SHOW50                                                           
*                                                                               
         LA    R2,GSTPSTTH                                                      
         LA    R3,GSTPSTTX                                                      
SHOW10   TM    1(R2),FVAXTND       FIELD HAS EXTENDED HEADER                    
         BZ    SHOW20                                                           
         CLC   SFSFLDN,0(R3)       CORRECT FIELD NUMBER                         
         BE    SHOW40                                                           
*                                                                               
SHOW20   SR    R1,R1                                                            
         ICM   R1,1,0(R2)          LENGTH OF TWA FIELD                          
         BNZ   *+6                                                              
         DC    H'0'                HAVE TO FIND FIELD                           
*                                                                               
         AR    R2,R1               BUMP TO NEXT FIELD                           
         LR    R3,R2                                                            
         IC    R1,0(R2)                                                         
         AR    R3,R1                                                            
         SH    R3,=H'8'            NEXT EXTENDED FIELD                          
         B     SHOW10                                                           
*                                                                               
SHOW40   ZIC   R1,SFSLN                                                         
         SH    R1,=H'3'            3 FOR ELEMENT + 1 FOR EX                     
         STC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SFSFIELD    COPY FIELD TO SCREEN                         
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
SHOW50   ZIC   R1,SFSLN                                                         
         AR    RF,R1               NEXT SCREEN FIELD                            
         B     SHOWLP                                                           
         DROP  RF                                                               
         EJECT                                                                  
*              RESTORE SAVED SCREEN                                             
*                                                                               
END10    DS    0H                                                               
         L     R2,ADTAXB                                                        
         CLI   0(R2),C'Z'          ERROR OCCURED FOR 1-PASS                     
         BE    *+8                                                              
         CLI   0(R2),C'G'          LEAVE 1-PASS ALONE                           
         BE    *+8                                                              
         MVI   0(R2),C'X'          CTXMODE TO END                               
         USING CTXDATA,R2                                                       
*                                                                               
         MVC   CTXNET,TOTNET       MOVE OVER TOTALS                             
         MVC   CTXGST,TOTGST                                                    
         MVC   CTXPST,TOTPST                                                    
         MVC   CTXGRS,TOTGRS                                                    
         MVC   CTXAPPL,GPSTAPPL                                                 
         MVC   CTXGORN,GSTCGON     COPY INPUT FROM SCREEN                       
         MVC   CTXPROV,GSTPSTP                                                  
         MVC   CTXLGSTA,GSTGSTAH+5                                              
         MVC   CTXGSTA,GSTGSTA                                                  
         MVC   CTXGSTTN,GSTGSTN                                                 
         MVC   CTXGSTT,GSTGSTT                                                  
         MVC   CTXERR,RETERR                                                    
         MVC   CTXMSGNO,MSGNO                                                   
         MVC   CTXUPSTT,USERPST                                                 
         MVC   CTXUPROV,USERPROV                                                
         BAS   RE,PUTOVER                                                       
*                                                                               
         GOTO1 AXITSES                                                          
         LA    R1,8                                                             
         L     RE,ADWCTAB          NEW AMOUNTS                                  
         LA    RF,CTAMTTAB                                                      
         L     R2,AAMNTS                                                        
         USING AMTD,RF                                                          
END20    CLC   AMTWC,SPACES        NO MORE WORKCODES                            
         BE    END35               QUIT                                         
         CLC   AMTWC,0(RE)                                                      
         BNE   END30                                                            
         ZAP   2(6,RE),AMTNET                                                   
         CLI   BATTYPE,46                                                       
         BNE   *+10                                                             
         MVC   0(AMTLNQ-2,R2),2(RF)      COPY AMOUNTS                           
         LA    R2,AMTLNQ-2(R2)                                                  
END25    LA    RF,AMTLNQ(RF)                                                    
END30    LA    RE,8(RE)                                                         
         CLI   BATTYPE,6           TYPE 6 AND 26 ONLY 1 AMOUNT                  
         BE    END35                                                            
         CLI   BATTYPE,26                                                       
         BE    END35                                                            
         BCT   R1,END20                                                         
*                                                                               
END35    CLI   BATTYPE,6                                                        
         BNE   END38                                                            
         L     R2,AAMNTS2                                                       
         MVC   0(VTCLNQ,R2),PSTVATBL                                            
         B     *+12                NEED GST VAT BLOCK TOO                       
END38    CLI   BATTYPE,26                                                       
         BNE   END40                                                            
         L     R2,AAMNTS                                                        
         MVC   0(VTCLNQ,R2),GSTVATBL                                            
*                                                                               
END40    LA    R2,CONACTH          CURSOR HAS TO BE SOMEWHERE                   
         MVI   CSSPROG,0           RESET THE PF KEYS                            
         B     EXIT                                                             
         DROP  RF                                                               
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* PUTOVER - GRABS OVERRIDES AND PUTS THEM BACK TO CTAX BLOCK                    
*---------------------------------------------------------------------          
         USING CTXDATA,R2                                                       
PUTOVER  NTR1                                                                   
         XC    CTXXTELM,CTXXTELM                                                
         LA    RF,CTXXTELM                                                      
         USING SFSELD,RF                                                        
*                                                                               
         CLI   GSTPSTPH+5,0        NO PST AVAILABLE                             
         BZ    PUTO100                                                          
         MVI   SFSEL,SFSELQ                                                     
         MVI   SFSLN,4                                                          
         MVC   SFSFLDN,GSTPSTTX    FIELD #                                      
         MVC   SFSFIELD,GSTPSTT                                                 
         LA    RF,4(RF)                                                         
*                                                                               
         CLI   NPST,0              ANY OVERRIDES?                               
         BZ    PUTO100             NO, DON'T PUT THEM OUT                       
         ZIC   RE,GSTPSTAH+5       GET LENGTH                                   
         LA    RE,3(RE)            FOR ELEM #, LENGTH, & FIELD ID               
         MVI   SFSEL,SFSELQ                                                     
         STC   RE,SFSLN                                                         
         MVC   SFSFLDN,GSTPSTAX    FIELD ID #                                   
         SH    RE,=H'4'            FOR EX MVC                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SFSFIELD(0),GSTPSTA    SAVE THE FIELD ENTRY                      
         LA    RF,4(RF)                                                         
         AR    RF,RE               ADD THE FIELD LENGTH                         
*                                                                               
PUTO100  MVI   0(RF),0                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  RF                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*              CLEAR SCREEN FOR NEXT INPUT                                      
*                                                                               
NEXT10   DS    0H                                                               
         TWAXC GSTCGONH,GSTPSTAH,PROT=Y    CLEAR THE SCREEN                     
         EDIT  LASTBASE,(11,WORK1),2,ALIGN=LEFT,FLOAT=-                         
         B     INIT70                       SET-UP FOR NEXT                     
         EJECT                                                                  
*              EDIT THE INPUT                                                   
*                                                                               
TAX10    DS    0H                                                               
         TM    GSTPSTTH+4,X'80'    USER INPUT                                   
         BZ    *+8                                                              
         MVI   USERPST,C'Y'        USER PST TYPE                                
         TM    GSTPSTPH+4,X'80'    USER INPUT                                   
         BZ    *+8                                                              
         MVI   USERPROV,C'Y'       USER PROVINCE                                
         BAS   RE,DEFPST                                                        
         MVI   RETERR,0            RETURN ERROR CODE                            
         CLI   GSTGSTT,C'*'                                                     
         BNE   TAX10A                                                           
         CLI   GSTPSTPH+5,0        NO PROVINCE?                                 
         BZ    TAX15               SKIP PST CHECK                               
         CLI   GSTPSTT,C'*'                                                     
         BE    TAX15                                                            
                                                                                
TAX10A   CLI   BATTYPE,6           BATCH 6 USES NET ONLY                        
         BE    *+8                                                              
         CLI   BATTYPE,26          BATCH 26 ALSO                                
         BNE   TAX11                                                            
         MVI   GSTCGON,C'N'        FORCE NET                                    
         MVI   GSTCGONH+5,1                                                     
         OI    GSTCGONH+6,X'80'    TRANSMIT IT                                  
TAX11    LA    R2,GSTCGONH         GROSS / NET                                  
*        BAS   RE,ANY                                                           
*                                                                               
         CLI   5(R2),0             HAVE TO INPUT SOMETHING                      
         BE    TAX14J              TRANSFER AMOUNTS                             
*                                                                               
TAX13    CLI   GSTCGON,C'G'        HAS TO BE G/N                                
         BE    TAX15                                                            
         CLI   GSTCGON,C'N'                                                     
         BE    TAX15                                                            
*                                                                               
TAX14    L     RE,ADTAXB                                                        
         CLI   0(RE),C'G'          1-PASS?                                      
         BNE   TAX14J                                                           
         MVI   RETERR,4            HAVE PROG POINT TO GROSS/NET                 
         B     TAX80A                                                           
*                                                                               
TAX14J   LA    R2,GSTCGONH                                                      
         MVC   MSGNO,=Y(AE$GRNET)                                               
         B     TAX80A              LET USER RETURN EVEN W/ ERROR                
*                                                                               
* TRANSFER AMOUNTS FROM AMTBLK                                                  
*                                                                               
TAX15    DS    0H                                                               
         ZAP   TOTGST,=P'0'        INITIALIZE TOTALS                            
         ZAP   TOTPST,=P'0'                                                     
         ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTGRS,=P'0'                                                     
         ZAP   CALCTOT,=P'0'       CALCULATED TOTAL                             
*                                                                               
         SR    R2,R2               COUNTER FOR # OF WORKCODES                   
         LA    R3,CTAMTTAB                                                      
         USING AMTD,R3                                                          
         LA    R0,8                MAX                                          
         LA    R4,AMTBLK           R4=A(WORK CODE TABLE)                        
*                                                                               
TAX15A   DS    0H                                                               
         MVC   AMTWC,0(R4)         BUILD ENTRY                                  
         ZAP   AMTGST,=P'0'        INITIALIZE AMOUNTS                           
         ZAP   AMTPST,=P'0'                                                     
         ZAP   AMTNET,=P'0'                                                     
         ZAP   AMTGRS,=P'0'                                                     
         ZAP   AMT,=P'0'                                                        
         ZAP   COM1,=P'0'                                                       
         ZAP   COM2,=P'0'                                                       
         CLC   0(2,R4),SPACES      NEED A WORK CODE                             
         BE    TAX15B                                                           
         LA    R1,AMTGRS           PUT AMOUNT IN APPR PLACE                     
         CLI   GSTCGON,C'G'                                                     
         BE    *+8                                                              
         LA    R1,AMTNET                                                        
         AP    0(L'AMTNET,R1),2(6,R4)                                           
         AP    CALCTOT,2(6,R4)                                                  
         LA    R2,1(R2)                                                         
         LA    R3,AMTLNQ(R3)                                                    
         DROP  R3                                                               
         CLI   BATTYPE,26                                                       
         BNE   TAX15B                                                           
         LA    RF,AMTBLK                                                        
         USING AMTD,RF                                                          
         ZAP   AMT,AMTAMT                                                       
         ZAP   COM1,AMTCOM                                                      
         ZAP   COM2,AMTCOM2                                                     
         B     TAX15C                                                           
*                                                                               
TAX15B   LA    R4,8(R4)                                                         
         BCT   R0,TAX15A                                                        
TAX15C   STC   R2,NAMTS                                                         
         LTR   R2,R2                                                            
         BNZ   TAX20                                                            
         MVC   MSGNO,=Y(AE$INVAM)                                               
         B     TAX80A                                                           
         DROP  RF                                                               
         USING AMTD,R3                                                          
*                                                                               
TAX20    LA    R1,GSTVATBL                                                      
         LA    R3,CTAMTTAB                                                      
         ZIC   R0,NAMTS                                                         
         LA    RF,COFFCLST                                                      
         ST    RF,AOFFC                                                         
TAX20A   MVI   OVERGPST,C'G'                                                    
         USING VTCD,R1                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVC   FLD,SPACES                                                       
         CLI   GSTGSTT,C'*'        NO GST                                       
         BE    TAX59               SKIP VATICAN                                 
*                                                                               
         MVI   VTCACTN,VTCAIVAL    VALIDATE INPUT TAX TYPE                      
         MVC   VTCCPY,COMPANY                                                   
         MVC   VTCOFFC,COFFICE                                                  
         CLI   BATTYPE,46                                                       
         BNE   TAX20B                                                           
         L     RF,AOFFC            GET CURRENT TYPE 46'S OFFICE                 
         MVC   VTCOFFC,0(RF)                                                    
*                                                                               
TAX20B   MVC   VTCCOMF,ACOMFACS                                                 
*                                                                               
         CLI   BATTYPE,6           BATCH TYPE 6 USES OUTPUT GST/PST             
         BE    TAX21                                                            
         CLI   BATTYPE,26          BATCH TYPE 26 USES BOTH                      
         BNE   TAX22                                                            
         CLI   GINPUT,C'Y'         INPUT GST/PST?                               
         BE    TAX22                                                            
TAX21    MVI   VTCACTN,VTCALOOK    VALIDATE OUTPUT TYPE                         
         MVC   VTCTYPE,GSTGSTT                                                  
         CLI   VTCTYPE,0                                                        
         BNE   *+10                                                             
         MVC   VTCTYPE,GSTVENDT                                                 
         B     TAX23A                                                           
*                                                                               
TAX22    MVC   TMPHEADR,GSTGSTTH   COPY INTO TEMP                               
         NI    TMPHEADR+1,X'FD'    EXTRACT EXTENDED FLD HDR                     
         LA    R2,TMPHEADR                                                      
         CLI   5(R2),0             NO TYPE GIVEN?                               
         BE    TAX23                                                            
         CLI   8(R2),0             NO TYPE GIVEN?                               
         BNE   *+10                                                             
TAX23    MVC   8(1,R2),GSTVENDT    USE VENDOR TYPE AS DEFAULT                   
         ST    R2,VTCAFLDH                                                      
TAX23A   MVC   VTCINVD,PWOSDATE                                                 
         GOTO1 VATICAN                                                          
         LA    R2,GSTGSTTH         POINT TO TYPE                                
         BE    TAX30                                                            
*                                                                               
TAX25    MVC   MSGNO,=Y(AE$INVGT)                                               
         MVI   RETERR,1            GST TYPE ERROR                               
         CLI   OVERGPST,C'P'                                                    
         BNE   *+14                                                             
         MVC   MSGNO,=Y(AE$INVPT)                                               
         MVI   RETERR,3            PST TYPE ERROR                               
         B     TAX80A                                                           
*                                                                               
TAX25X   MVI   ERRNUM,NOTFOUND                                                  
         B     EXIT                                                             
*                                                                               
TAX30    TM    VTCINDS,VTCINA      IS GST AVAILABLE?                            
         BNO   TAX50                 YES, USE IT                                
         MVC   MSGNO,=Y(AE$GSTNA)                                               
         MVI   RETERR,1            GST TYPE ERROR                               
         B     TAX80A                                                           
*                                                                               
TAX50    CLI   BATTYPE,46                                                       
         BNE   TAX53                                                            
         LA    R3,AMTLNQ(R3)                                                    
         LA    R1,VTCLNQ(R1)       NEXT VATICAN BLOCK                           
         L     RF,AOFFC                                                         
         LA    RF,2(RF)                                                         
         ST    RF,AOFFC                                                         
         BCT   R0,TAX20A                                                        
TAX53    LA    R2,GSTGSTNH         SHOW GST INFO ON SCREEN                      
         LA    R1,GSTVATBL                                                      
         MVC   FLD,SPACES                                                       
         MVC   FLD(1),VTCTYPE                                                   
         MVC   FLD+2(L'VTCTTYPE),VTCTTYPE                                       
         MVC   FLD+14(5),=C'RATE='                                              
         SR    R0,R0                                                            
         ICM   R0,3,VTCRATE                                                     
*                                                                               
         TM    VTCINDS,VTCIDC3                                                  
         BZ    TAX55                                                            
         EDIT  (R0),(5,FLD+18),3,ALIGN=LEFT                                     
         B     TAX57                                                            
*                                                                               
TAX55    EDIT  (R0),(5,FLD+19),2,ALIGN=LEFT                                     
*                                                                               
TAX57    GOTO1 SQUASHER,DMCB,FLD,L'FLD                                          
*                                                                               
TAX59    MVC   GSTGSTN(20),FLD                                                  
         OI    GSTGSTNH+6,X'80'                                                 
*                                                                               
* NOW CHECK FOR PROVINCIAL TAX                                                  
*                                                                               
TAX60    CLI   GSTPSTPH+5,0                                                     
         BNZ   *+14                                                             
         MVC   FLD,SPACES                                                       
         B     TAX79                                                            
*                                                                               
* VALIDATE PROVINCE                                                             
*                                                                               
         LA    RF,PRVTAB                                                        
TAX60LP  CLI   0(RF),X'FF'         EOT?                                         
         BE    TAX60ERR                                                         
         CLC   GSTPSTP,0(RF)       FOUND PROVINCE?                              
         BE    TAX69                                                            
         LA    RF,L'PRVTAB(RF)                                                  
         B     TAX60LP                                                          
*                                                                               
TAX60ERR LA    R2,GSTPSTPH                                                      
         MVC   MSGNO,=Y(AE$PROVX)                                               
         MVI   RETERR,3            PST PROV ERROR                               
         B     TAX80A                                                           
*                                                                               
TAX69    LA    R1,PSTVATBL                                                      
         LA    R3,CTAMTTAB                                                      
         ZIC   R0,NAMTS                                                         
         LA    RF,COFFCLST                                                      
         ST    RF,AOFFC                                                         
TAX69A   MVI   OVERGPST,C'P'                                                    
         XC    VTCD(VTCLNQ),VTCD                                                
         MVC   FLD,SPACES                                                       
         CLI   GSTPSTT,C'*'        NO PST, FOR * TOO                            
         BE    TAX79               SKIP VATICAN                                 
*                                                                               
         MVI   VTCACTN,VTCAIVAL    VALIDATE INPUT TAX TYPE                      
         MVC   VTCCPY,COMPANY                                                   
         MVC   VTCPRV,GSTPSTP      PROVINCIAL CODE                              
         MVC   VTCOFFC,COFFICE                                                  
         CLI   BATTYPE,46                                                       
         BNE   TAX69B                                                           
         L     RF,AOFFC            GET CURRENT TYPE 46'S OFFICE                 
         MVC   VTCOFFC,0(RF)                                                    
*                                                                               
TAX69B   MVC   VTCCOMF,ACOMFACS                                                 
         CLI   BATTYPE,6           BATCH TYPE 6 USES OUTPUT                     
         BE    TAX69C                                                           
         CLI   BATTYPE,26          BATCH TYPE 26 TOO                            
         BNE   TAX69D                                                           
         CLI   GINPUT,C'Y'         UNLESS INPUT=Y                               
         BE    TAX69D                                                           
TAX69C   MVI   VTCACTN,VTCALOOK    VALIDATE OUTPUT TYPE                         
         MVC   VTCTYPE,GSTPSTT                                                  
         CLI   VTCTYPE,0                                                        
         BNE   *+10                                                             
         MVC   VTCTYPE,PSTVENDT                                                 
         B     TAX69X2                                                          
*                                                                               
TAX69D   MVC   TMPHEADR,GSTPSTTH                                                
         LA    R2,TMPHEADR                                                      
         CLI   5(R2),0             PST TYPE GIVEN?                              
         BZ    TAX69X                                                           
         CLI   8(R2),0                                                          
         BNE   *+10                                                             
TAX69X   MVC   8(1,R2),PSTVENDT    NO, USE VENDOR TYPE                          
         ST    R2,VTCAFLDH                                                      
TAX69X2  MVC   VTCINVD,PWOSDATE                                                 
         GOTO1 VATICAN                                                          
         LA    R2,GSTPSTTH         POINT TO TYPE                                
         BNE   TAX25                                                            
*                                                                               
         TM    VTCINDS,VTCINA                                                   
         BNO   TAX70                                                            
         MVC   MSGNO,=Y(AE$PSTNA)                                               
         MVI   RETERR,3            PST ERROR                                    
         B     TAX80A                                                           
*                                                                               
TAX70    CLI   BATTYPE,46                                                       
         BNE   TAX73                                                            
         LA    R3,AMTLNQ(R3)                                                    
         LA    R1,VTCLNQ(R1)                                                    
         L     RF,AOFFC                                                         
         LA    RF,2(RF)                                                         
         ST    RF,AOFFC                                                         
         BCT   R0,TAX69A                                                        
*                                                                               
TAX73    LA    R2,GSTPSTNH                                                      
         LA    R1,PSTVATBL                                                      
         MVC   FLD,SPACES                                                       
         MVC   FLD(1),VTCTYPE                                                   
         MVC   FLD+2(L'VTCTTYPE),VTCTTYPE                                       
         MVC   FLD+14(5),=C'RATE='                                              
         SR    R0,R0                                                            
         ICM   R0,3,VTCRATE                                                     
*                                                                               
         TM    VTCINDS,VTCIDC3     3 DECIMALS?                                  
         BZ    TAX75               NO                                           
         EDIT  (R0),(5,FLD+18),3,ALIGN=LEFT                                     
         B      TAX77                                                           
*                                                                               
TAX75    EDIT  (R0),(5,FLD+19),2,ALIGN=LEFT                                     
*                                                                               
TAX77    GOTO1 SQUASHER,DMCB,FLD,L'FLD                                          
*                                                                               
TAX79    MVC   GSTPSTN(20),FLD                                                  
         OI    GSTPSTNH+6,X'80'                                                 
*                                                                               
* OVERRIDING AMOUNTS                                                            
*                                                                               
TAX80    CLI   GSTCGON,C'G'                                                     
         BE    TAX81                                                            
         MVI   SVGSTCGN,C'N'                                                    
         LA    R2,GSTGSTAH         EXTRACT GST OVERRIDE AMOUNTS                 
         MVI   OVERGPST,C'G'                                                    
         BAS   RE,OVERAMTS                                                      
         BNE   TAX80A                                                           
         LA    R2,GSTPSTAH         EXTRACT PST OVERRIDE AMOUNTS                 
         MVI   OVERGPST,C'P'                                                    
         BAS   RE,OVERAMTS                                                      
         BNE   TAX80A                                                           
         B     TAX85                                                            
*                                                                               
TAX80A   L     RE,ADTAXB                                                        
         CLI   0(RE),C'G'          1 PASS?                                      
         BNE   PEXIT                                                            
         MVI   0(RE),C'Z'          TELL, CALLER OVERLAY ERROR                   
         B     END10                                                            
*                                                                               
* GROSS GOES BACKWARD                                                           
*                                                                               
TAX81    LA    R2,GSTPSTAH         EXTRACT GST OVERRIDE AMOUNTS                 
         MVI   SVGSTCGN,C'G'                                                    
         CLC   CPROV,=C'PQ'        SPECIAL CALCS FOR AFTER JAN/13               
         BNE   *+14                                                             
         CLC   PWOSDATE,=X'B30101'                                              
         BNL   TAX82                                                            
*                                                                               
         MVI   OVERGPST,C'P'       DO NORMAL CALCS                              
         BAS   RE,OVERAMTS                                                      
         BNE   TAX80A                                                           
         LA    R2,GSTGSTAH         EXTRACT PST OVERRIDE AMOUNTS                 
         MVI   OVERGPST,C'G'                                                    
         BAS   RE,OVERAMTS                                                      
         BNE   TAX80A                                                           
         B     TAX85                                                            
*                                                                               
TAX82    MVI   OVERGPST,C'P'                                                    
         BAS   RE,OVERAMTS         FIGURE OUT NET                               
         BNE   TAX80A                                                           
         MVI   SVGSTCGN,C'N'       RUN AS NET TO FIGURE OUT REST                
         LA    R2,GSTGSTAH         EXTRACT GST OVERRIDE AMOUNTS                 
         MVI   OVERGPST,C'G'                                                    
         BAS   RE,OVERAMTS                                                      
         BNE   TAX80A                                                           
         LA    R2,GSTPSTAH         EXTRACT PST OVERRIDE AMOUNTS                 
         MVI   OVERGPST,C'P'                                                    
         BAS   RE,OVERAMTS                                                      
         BNE   TAX80A                                                           
*                                                                               
TAX85    ZIC   R0,NAMTS            LOOP TOTALLING                               
         ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTGRS,=P'0'                                                     
         ZAP   TOTGST,=P'0'                                                     
         ZAP   TOTPST,=P'0'                                                     
         LA    R3,CTAMTTAB         TABLE OF AMOUNTS                             
*                                                                               
TAX85A   DS    0H                                                               
         AP    TOTNET,AMTNET                                                    
         AP    TOTGST,AMTGST                                                    
         AP    TOTPST,AMTPST                                                    
         AP    TOTGRS,AMTGRS                                                    
         LA    R3,AMTLNQ(R3)                                                    
         BCT   R0,TAX85A                                                        
*                                                                               
         MVC   FLD,SPACES                                                       
         CLI   GSTGSTT,C'*'                                                     
         BE    TAX85B                                                           
         MVC   FLD(4),=C'GST='                                                  
         SR    R0,R0                                                            
         EDIT  TOTGST,(8,FLD+4),2,ALIGN=LEFT,MINUS=YES                          
TAX85B   MVC   GSTGSTX(20),FLD                                                  
         OI    GSTGSTXH+6,X'80'                                                 
*                                                                               
         MVC   FLD,SPACES                                                       
         CLI   GSTPSTT,C'*'                                                     
         BE    TAX89                                                            
         CLI   GSTPSTPH+5,0                                                     
         BE    TAX89                                                            
         MVC   FLD(4),=C'PST='                                                  
         SR    R0,R0                                                            
         EDIT  TOTPST,(8,FLD+4),2,ALIGN=LEFT,MINUS=YES                          
TAX89    MVC   GSTPSTX(20),FLD                                                  
         OI    GSTPSTXH+6,X'80'                                                 
*                                                                               
         MVC   FLD,SPACES                                                       
         CLI   BATTYPE,26                                                       
         BE    TAX90                                                            
         MVC   FLD(4),=C'NET='                                                  
         SR    R0,R0                                                            
         EDIT  TOTNET,(10,FLD+4),2,ALIGN=LEFT,MINUS=YES                         
         MVC   FLD+24(4),=C'GRS='                                               
         SR    R0,R0                                                            
         EDIT  TOTGRS,(10,FLD+28),2,ALIGN=LEFT,MINUS=YES                        
         GOTO1 SQUASHER,DMCB,FLD,L'FLD                                          
TAX90    MVC   GSTBLAH,FLD                                                      
         OI    GSTBLAHH+6,X'80'                                                 
*                                                                               
TAX100   BAS   RE,POSTIT           DO POSTINGS                                  
         L     RE,ADTAXB           SCREEN LOADED- OK TO EDIT(NEXT TIME)         
         MVC   ERRNUM,0(RE)                                                     
         L     RF,AIOA                                                          
         ST    RF,0(RE)                                                         
         MVC   0(1,RE),ERRNUM                                                   
TAXIT    MVI   ERRNUM,SPECIAL                                                   
         MVC   MSG,SPACES                                                       
         MVC   MSG(L'LEAVE),LEAVE                                               
         LA    R2,GSTCGONH         POINT BACK TO START                          
         CLI   PFKEY,15            ONLY PF15 WILL RETURN                        
         BE    END10                                                            
         CLI   0(RE),C'G'                                                       
         BNE   EXIT                                                             
         B     END10                                                            
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
* DECIDE IF WE NEED TO CHANGE PST TYPE, DEFAULT PST                             
*                                                                               
DEFPST   NTR1                                                                   
         CLI   GSTPSTPH+5,0        NO PROVINCE                                  
         BE    EXIT                FORGET ABOUT PST                             
         CLI   USERPST,C'Y'        DID USER ENTER THEIR OWN PST?                
         BE    EXIT                FORGET ABOUT CHANGING IT                     
         CLI   GSTGSTT,C'*'        DISABLE GST TAX?                             
         BE    DEFPST10                                                         
*                                                                               
         MVI   GSTPSTT,0                                                        
         OI    GSTPSTTH+6,X'80'    TRANSMIT                                     
         MVI   GSTPSTTH+5,0                                                     
         B     EXIT                                                             
*                                                                               
DEFPST10 MVI   GSTPSTT,C'*'                                                     
         OI    GSTPSTTH+6,X'80'    TRANSMIT                                     
         MVI   GSTPSTTH+5,1                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* GET OVERRIDE AMOUNTS / RATE CALCULATION                                       
*                                                                               
         USING VTCD,R6                                                          
OVERAMTS NTR1                                                                   
         CLI   OVERGPST,C'G'                                                    
         BNE   *+12                                                             
         LA    R6,GSTVATBL         GST VATICAN BLOCK                            
         B     *+8                                                              
         LA    R6,PSTVATBL         PST VATICAN BLOCK                            
*                                                                               
         MVI   NOVER,0                                                          
         CLI   OVERGPST,C'G'                                                    
         BNE   OVER02                                                           
         CLI   GSTGSTT,C'*'                                                     
         BE    OVER03                                                           
         B     OVER05                                                           
OVER02   CLI   GSTPSTT,C'*'                                                     
         BE    OVER03                                                           
         CLI   GSTPSTPH+5,0        ANY PROVINCE?                                
         BNZ   OVER05                                                           
*                                                                               
OVER03   CLI   5(R2),0             TAX NOT APPLICABLE & OVERRIDES?              
         BZ    OVER60                                                           
         MVC   MSGNO,=Y(AE$GSTNA)                                               
         MVI   RETERR,2                                                         
         CLI   OVERGPST,C'P'                                                    
         BNE   OVERBAD                                                          
         MVC   MSGNO,=Y(AE$PSTNA)                                               
         MVI   RETERR,3                                                         
         B     OVERBAD                                                          
*                                                                               
OVER05   CLI   5(R2),0             DO WE HAVE OVERRIDING AMOUNTS?               
         BNZ   OVER08                                                           
         CLI   BATTYPE,6           OVERRIDE AMOUNT REQUIRED                     
         BE    *+8                                                              
         CLI   BATTYPE,26          OVERRIDE AMOUNT REQUIRED FOR 26 TOO          
         BNE   OVER60              NO, DO RATE CALCULATIONS                     
         OC    VTCRATE,VTCRATE     ZERO RATE?                                   
         BZ    OVER60                                                           
OVER06   MVC   MSGNO,=Y(AE$GSTAR)                                               
         LA    RE,1                                                             
         MVI   RETERR,2                                                         
         CLI   OVERGPST,C'P'                                                    
         BNE   OVERBAD                                                          
         MVC   MSGNO,=Y(AE$PSTAR)                                               
         MVI   RETERR,3                                                         
         B     OVERBAD                                                          
*                                                                               
OVER08   GOTO1 SCANNER,DMCB,(0,(R2)),(X'80',IOAREA),0                           
         MVI   ERRNUM,2                                                         
         CLI   4(R1),0             NO LINES?                                    
         BZ    OVER60              DO RATE CALCULATIONS                         
         MVC   NOVER,4(R1)         SAVE # OF OVERRIDES                          
         CLC   NOVER,NAMTS         # OF OVERRIDES HAS TO = # OF WC              
         BE    OVER10                                                           
         CLI   NOVER,1             1 LUMP GST/PST SUM                           
         BE    OVER50                                                           
*                                                                               
         MVC   MSGNO,=Y(AE$GAMTW)                                               
         MVI   RETERR,2            GST AMOUNT ERROR                             
         CLI   OVERGPST,C'P'                                                    
         BNE   *+14                                                             
         MVC   MSGNO,=Y(AE$PAMTW)                                               
         MVI   RETERR,3            PST ERROR                                    
         LA    RE,1                                                             
         B     OVERBAD                                                          
*                                                                               
OVER10   LA    R4,IOAREA                                                        
         USING SCANBLKD,R4                                                      
         LA    R3,CTAMTTAB                                                      
         USING AMTD,R3                                                          
         ZIC   R5,NOVER            # OF OVERRIDES, COUNTER                      
OVER20   MVC   ERRNDX,SC2NDNUM                                                  
         CLI   SC2NDLEN,0                                                       
         BNE   OVEROK                                                           
         MVC   ERRNDX,SC1STNUM                                                  
         CLI   SC1STLEN,0                                                       
         BE    OVEROK                                                           
         ZIC   R0,SC1STLEN                                                      
         GOTO1 CASHVAL,DMCB,(X'82',SCONEFLD),(R0)                               
         CLI   0(R1),X'FF'                                                      
         BNE   OVER25                                                           
OVER23   MVC   MSGNO,=Y(AE$GSTAI)                                               
         MVI   RETERR,2            GST AMOUNT ERROR                             
         CLI   OVERGPST,C'P'                                                    
         BNE   OVERBAD                                                          
         MVC   MSGNO,=Y(AE$PSTAI)                                               
         MVI   RETERR,3            PST ERROR                                    
         B     OVERBAD                                                          
OVER25   OC    VTCRATE,VTCRATE     ZERO RATE?                                   
         BNZ   OVER25A                                                          
         CLI   BATTYPE,6           OVERRIDE AMOUNT REQUIRED                     
         BE    *+8                                                              
         CLI   BATTYPE,26          OVERRIDE AMOUNT REQUIRED FOR 26 TOO          
         BNE   OVER25A             NO, DO RATE CALCULATIONS                     
         CP    4(8,R1),=P'0'       ZERO AMOUNT?                                 
         BNE   OVER23              NO, ERROR                                    
*                                                                               
OVER25A  MVI   ERRNDX,0                                                         
         CLI   OVERGPST,C'G'                                                    
         BNE   OVER30                                                           
         ZAP   AMTGST,4(8,R1)      SET UP GST AMOUNT                            
         B     OVER40                                                           
*                                                                               
OVER30   ZAP   AMTPST,4(8,R1)      SET UP PST AMOUNT                            
OVER40   LA    R4,SCBLKLQ(R4)      NEXT AMOUNT                                  
         LA    R3,AMTLNQ(R3)                                                    
         BCT   R5,OVER20                                                        
         B     OVER60                                                           
*                                                                               
OVER50   DS    0H                  SPLIT GST/PST IF NEEDED                      
         ZAP   DUB1,=P'0'                                                       
         LA    R4,IOAREA                                                        
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   0(R1),X'FF'                                                      
         BE    OVER23              SET ERRORS IF BAD CASHVAL                    
         ZAP   ONETAX,4(8,R1)      SAVE TAX AMOUNT                              
         CP    CALCTOT,=P'0'                                                    
         BNE   OVER52                                                           
         CP    ONETAX,=P'0'                                                     
         BE    OVER52                                                           
         MVI   ERRNUM,INVALID                                                   
         B     OVERBAD                                                          
*                                                                               
OVER52   ZIC   R0,NAMTS            SPLIT GST/PST IF NEEDED                      
         LA    R3,CTAMTTAB         TABLE OF AMOUNTS                             
OVER55   ZICM  RF,VTCRATE,2                                                     
         CVD   RF,DUB                                                           
         ZAP   WORK(16),AMTNET     CALCULATE THE PROPORTIONAL TAX               
         CLI   SVGSTCGN,C'G'       GROSS INPUT?                                 
         BNE   *+10                                                             
         ZAP   WORK(16),AMTGRS                                                  
         LA    R5,AMTGST                                                        
         CLI   OVERGPST,C'G'                                                    
         BE    *+8                                                              
         LA    R5,AMTPST                                                        
*                                                                               
         CP    CALCTOT,=P'0'       TOTAL = 0?                                   
         BE    OVER58                                                           
         MP    WORK(16),ONETAX     * TAX AMOUNT                                 
*                                                                               
         SRP   WORK(16),1,0        * 10                                         
         DP    WORK(16),CALCTOT    / TOTAL AMOUNT                               
         SRP   WORK(16-L'CALCTOT),64-1,5          ROUND THE AMOUNT              
*                                                                               
         ZAP   0(L'AMTGST,R5),WORK(16-L'CALCTOT)                                
         AP    DUB1,WORK(16-L'CALCTOT)                                          
         B     OVER59                                                           
*                                                                               
OVER58   MP    WORK(16),DUB                                                     
         TM    VTCINDS,VTCIDC3     FOR 3 DCP, DO NOT X10                        
         BO    *+10                                                             
         SRP   WORK(16),1,0                                                     
         SRP   WORK(16),64-5,5                                                  
         ZAP   0(L'AMTGST,R5),WORK(16)                                          
         AP    DUB1,WORK(16)                                                    
*                                                                               
OVER59   LA    R3,AMTLNQ(R3)                                                    
         CLI   BATTYPE,46                                                       
         BNE   *+8                                                              
         LA    R6,VTCLNQ(R6)                                                    
         BCT   R0,OVER55                                                        
         ZAP   DUB2,ONETAX                                                      
         SP    DUB2,DUB1                                                        
         BZ    OVER60                                                           
         BAS   RE,REMAIN           TAKE DIFFERENCE OFF HIGHEST                  
*                                                                               
* CALCULATE GST/PST IF NO OVERRIDES                                             
*                                                                               
OVER60   CLI   SVGSTCGN,C'G'       GROSS INPUT?                                 
         BE    OVER80              GROSS INPUT, DIFFERENT CALCULATION           
*                                                                               
         CLI   OVERGPST,C'G'                                                    
         BNE   *+12                                                             
         LA    R6,GSTVATBL         GST VATICAN BLOCK                            
         B     *+8                                                              
         LA    R6,PSTVATBL         PST VATICAN BLOCK                            
         LA    R3,CTAMTTAB                                                      
         ZIC   R0,NAMTS            USE CALCULATION THROUGHOUT AMOUNTS           
*                                                                               
OVER65   ZICM  RF,VTCRATE,2        CONVERT RATE TO PACKED DECI                  
         CVD   RF,DUB                                                           
*                                                                               
         CP    DUB,=P'0'           IF RATE IS ZERO, JUST ADD                    
         BNE   OVER66                                                           
         ZAP   WORK(16),=P'0'                                                   
         B     OVER67A                                                          
*                                                                               
OVER66   CLI   NOVER,0             WORKING WITH OVERRIDES?                      
         BNZ   OVER67A                                                          
         ZAP   WORK(16),AMTNET     GST= (NET) * RATE                            
         CLI   OVERGPST,C'P'                                                    
         BNE   OVER67                                                           
*                                                                               
         CLC   CPROV,=C'PQ'                                                     
         BNE   OVER66A                                                          
         CLC   PWOSDATE,=X'B30101'                                              
         BNL   OVER67                                                           
                                                                                
OVER66A  AP    WORK(16),AMTGST                                                  
*                                                                               
OVER67   MP    WORK(16),DUB        PST= (NET + GST) * RATE                      
*                                                                               
         TM    VTCINDS,VTCIDC3     FOR 3 DCP, DO NOT X10                        
         BO    *+10                                                             
         SRP   WORK(16),1,0                                                     
         SRP   WORK(16),64-5,5     ROUND BACK TO 2 DP                           
*                                                                               
OVER67A  CLI   OVERGPST,C'G'                                                    
         BNE   OVER68                                                           
         CLI   NOVER,0                                                          
         BNZ   *+10                                                             
         ZAP   AMTGST,WORK(16)                                                  
         ZAP   AMTGRS,AMTGST                                                    
         AP    AMTGRS,AMTNET                                                    
         B     OVER69                                                           
OVER68   CLI   NOVER,0                                                          
         BNZ   *+10                                                             
         ZAP   AMTPST,WORK(16)                                                  
         ZAP   AMTGRS,AMTPST                                                    
         AP    AMTGRS,AMTGST                                                    
         AP    AMTGRS,AMTNET                                                    
OVER69   LA    R3,AMTLNQ(R3)                                                    
         CLI   BATTYPE,46                                                       
         BNE   *+8                                                              
         LA    R6,VTCLNQ(R6)                                                    
         BCT   R0,OVER65                                                        
*                                                                               
OVER70   B     OVER100                                                          
*                                                                               
* GROSS INPUT                                                                   
*                                                                               
OVER80   DS    0H                                                               
         LA    R3,CTAMTTAB                                                      
         ZIC   R0,NAMTS                                                         
*                                                                               
         CLI   OVERGPST,C'G'                                                    
         BNE   *+12                                                             
         LA    R6,GSTVATBL         GST VATICAN BLOCK                            
         B     *+8                                                              
         LA    R6,PSTVATBL         PST VATICAN BLOCK                            
         OC    VTCRATE,VTCRATE     ZERO RATE?                                   
         BNZ   OVER84                                                           
         CLI   NOVER,0             AND NO OVERRIDES                             
         BNZ   OVER84                                                           
         CLI   OVERGPST,C'P'       HAVE TO MOVE GROSS AMOUNT TO NET             
         BNE   OVER100               WHEN PST RATE = 0 ONLY                     
OVER83   MVC   AMTNET,AMTGRS                                                    
         LA    R3,AMTLNQ(R3)                                                    
         BCT   R0,OVER83                                                        
         B     OVER100                                                          
*                                                                               
OVER84   LA    R3,CTAMTTAB                                                      
OVER84A  MVC   HALF,VTCRATE                                                     
         CLC   CPROV,=C'PQ'                                                     
         BNE   OVER85                                                           
         CLC   PWOSDATE,=X'B30101'                                              
         BL    OVER85                                                           
         CLI   OVERGPST,C'P'                                                    
         BNE   OVER85                                                           
         LA    RE,GSTVATBL         GST VATICAN BLOCK                            
         SR    RF,RF                                                            
         ICM   RF,3,VTCRATE-VTCD(RE)                                            
         AH    RF,HALF                                                          
         STH   RF,HALF                                                          
*                                                                               
OVER85   CLI   OVERGPST,C'P'       NET= GROSS / (RATE+100%)                     
         BNE   *+14                                                             
         ZAP   DUB,AMTGRS                                                       
         B     *+10                                                             
         ZAP   DUB,AMTNET          USE AMTNET FOR GST TMP GROSS                 
*                                                                               
         CLI   NOVER,0                                                          
         BNZ   OVER86O                                                          
*                                                                               
         SR    R4,R4                                                            
         CVB   R5,DUB                                                           
         LH    RF,HALF                                                          
         TM    VTCINDS,VTCIDC3     FOR 3 DCP, DO NOT X10                        
         BO    OVER85B                                                          
         M     R4,=F'20000'        *10000 FOR SCALING, *2 FOR ROUNDING          
         A     RF,=F'10000'        ADD 100% FOR 2 DCP                           
         B     OVER85C                                                          
OVER85B  M     R4,=F'200000'       *100000 FOR SCALING, *2 FOR ROUNDING         
         A     RF,=F'100000'       ADD 100% FOR 3 DCP                           
OVER85C  DR    R4,RF                                                            
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
*        TM    VTCINDS,VTCIDC3     FOR 3 DCP, DO NOT X10                        
*        BO    *+8                                                              
         SRA   R5,1                ROUNDED DIVIDE FOR THE NET                   
         CVD   R5,DUB                                                           
         ZAP   WORK(16),AMTNET     TMP STORAGE                                  
         ZAP   AMTNET,DUB                                                       
         CLI   OVERGPST,C'P'                                                    
         BNE   OVER86                                                           
         ZAP   AMTPST,AMTGRS       PST=GROSS-NET                                
         SP    AMTPST,AMTNET                                                    
         B     OVER87                                                           
*                                                                               
OVER86   ZAP   AMTGST,WORK(16)                                                  
         SP    AMTGST,AMTNET       GST=GROSS-NET                                
         CLC   CPROV,=C'PQ'                                                     
         BNE   OVER87                                                           
         CLC   PWOSDATE,=X'B30101'                                              
         BL    OVER87                                                           
         SP    AMTPST,AMTGST      PQ PST BASIS =PST-GST                         
         B     OVER87                                                           
*                                                                               
OVER86O  CLI   OVERGPST,C'G'       OVERRIDES HAVE DIFF CALC                     
         BE    OVER86O1                                                         
         ZAP   AMTNET,AMTGRS                                                    
         SP    AMTNET,AMTPST                                                    
         B     OVER87                                                           
*                                                                               
OVER86O1 SP    AMTNET,AMTGST                                                    
*                                                                               
OVER87   LA    R3,AMTLNQ(R3)       NEXT AMOUNT                                  
         CLI   BATTYPE,46                                                       
         BNE   *+8                                                              
         LA    R6,VTCLNQ(R6)       NEXT VATICAN AMOUNT                          
         BCT   R0,OVER84A                                                       
         B     OVER100                                                          
*                                                                               
OVER100  DS    0H                                                               
*                                                                               
OVEROK   MVI   ERRNUM,OK                                                        
         CLI   OVERGPST,C'G'                                                    
         BNE   *+14                                                             
         MVC   NGST,NOVER                                                       
         B     *+10                                                             
         MVC   NPST,NOVER                                                       
         SR    RE,RE                                                            
OVERBAD  LTR   RE,RE                                                            
         B     CURSIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ALLOCATE A REMAINDER TO THE AMOUNT TABLE                       
*                                                                               
* AT ENTRY, DUB2=REMAINDER AMOUNT                                               
*                                                                               
* ROUTINE ALLOCATES THE REMAINDER TO THE FIRST NON-ZERO                         
* AMOUNT OR THE AMOUNT WITH THE LARGEST ABSOLUTE VALUE                          
* OF THE NET OR GST DEPENDING UPON THE PARAMETER                                
*                                                                               
REMAIN   NTR1  ,                                                                
         XC    ALARGEST,ALARGEST                                                
         ZIC   R0,NAMTS                                                         
         LA    R3,CTAMTTAB                                                      
*                                                                               
REM2     LA    R4,AMTNET                                                        
         CLI   SVGSTCGN,C'N'                                                    
         BE    *+8                                                              
         LA    R4,AMTGRS                                                        
         ZAP   DUB,0(L'AMTNET,R4)                                               
         BZ    REM4                ZERO AMOUNT                                  
         OC    ALARGEST,ALARGEST   TEST FOR FIRST NON-ZERO WC                   
         BNZ   REM3                NO                                           
*                                                                               
         ST    R3,ALARGEST                                                      
         ZAP   DUB1,0(L'AMTNET,R4)                                              
         CP    DUB1,=P'0'          TEST IF NEGATIVE                             
         BNM   *+10                NO                                           
         MP    DUB1,=P'-1'         MAKE SURE ITS THE ABSOLUTE VALUE             
         B     REM4                                                             
*                                                                               
REM3     CP    DUB,=P'0'           TEST FOR NEGATIVE WORKCODE                   
         BNM   *+10                                                             
         MP    DUB,=P'-1'          FORM ABSOLUTE VALUE                          
         CP    DUB,DUB1            TEST IF ITS THE LARGEST                      
         BNH   REM4                NO                                           
*                                                                               
         ST    R3,ALARGEST         YES                                          
         ZAP   DUB1,DUB            RESET THE LARGEST VALUE                      
*                                                                               
REM4     LA    R3,AMTLNQ(R3)                                                    
         BCT   R0,REM2                                                          
*                                                                               
REM6     ICM   R3,15,ALARGEST                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R4,AMTGST                                                        
         CLI   OVERGPST,C'G'                                                    
         BE    *+8                                                              
         LA    R4,AMTPST                                                        
         AP    0(L'AMTNET,R4),DUB2 APPLY THE REMAINDER                          
*                                                                               
REMX     XIT1                                                                   
*                                                                               
*              BUILD POSTING RECORDS                                            
*                                                                               
         USING PSTD,R5                                                          
         USING VTCD,R1                                                          
POSTIT   NTR1                      DO THE GST/PST POSTINGS HERE                 
         L     R2,AIOA                                                          
         LR    RF,R2                                                            
         LA    R3,CTAMTTAB                                                      
         LA    R1,GSTVATBL                                                      
         ST    R1,CURGSTVT                                                      
         LA    R1,PSTVATBL                                                      
         ST    R1,CURPSTVT                                                      
         LA    R1,COFFCLST                                                      
         ST    R1,AOFFC                                                         
*                                                                               
         XC    0(2,R2),0(R2)       CLEAR LENGTH                                 
         MVI   GPSTAPPL,C'N'                                                    
         CLI   GSTGSTT,C'*'        NO GST?                                      
         BNE   POST010             NO, DO POSTING                               
         CLI   GSTPSTT,C'*'        NO PST?                                      
         BE    POSTX                                                            
*                                                                               
POST010  MVI   GPSTAPPL,C'Y'                                                    
         MVI   OVERGPST,C'G'                                                    
         CLI   BATTYPE,46          SPECIAL BATCH TYPE 46?                       
         BNE   POST015                                                          
         OC    AMTWC,AMTWC         ARE WE DONE?                                 
         BZ    POSTX               YES, LEAVE                                   
POST015  LA    R2,2(R2)            TWO BYTES FOR LENGTH                         
         CLI   GSTGSTT,C'*'                                                     
         BE    POST150                                                          
*                                                                               
*                                                                               
         USING TRCASHD,R2                                                       
POST050  XC    TRCSEL(TRCSLNQ1),TRCSEL                                          
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1    BUIL A NET ELEMENT W/ THE BASIS              
         MVI   TRCSTYPE,C'N'                                                    
         ZAP   TRCSAMNT,TOTNET                                                  
         CLI   BATTYPE,26          BAT26 NET ELEMENT DIFF                       
         BNE   POST053                                                          
         ZAP   TRCSAMNT,COM1                                                    
         AP    TRCSAMNT,COM2       GST NET=COMM FOR ZERO BILLS                  
         CP    AMT,=P'0'                                                        
         BE    *+10                                                             
         AP    TRCSAMNT,TOTNET     BASIS=NET+COM FOR ALL OTHERS                 
*                                                                               
POST053  CLI   BATTYPE,6           BAT06 POSTS USER'S BASIS                     
         BE    *+8                                                              
         CLI   BATTYPE,26          BAT26 TOO                                    
         BNE   POST055                                                          
         OC    GBASIS,GBASIS                                                    
         BZ    POST055                                                          
         ZAP   TRCSAMNT,GBASIS                                                  
*                                                                               
POST055  CLI   BATTYPE,46          BAT46?                                       
         BNE   *+10                                                             
         ZAP   TRCSAMNT,AMTNET     USE WORKCODE'S AMOUNT                        
         CLI   OVERGPST,C'P'                                                    
         BNE   POST059                                                          
         CLC   CPROV,=C'PQ'                                                     
         BNE   POST057                                                          
         CLC   PWOSDATE,=X'B30101'                                              
         BNL   POST057                                                          
*                                                                               
         CLI   BATTYPE,46                                                       
         BNE   *+14                                                             
         AP    TRCSAMNT,AMTGST     USE WORKCODE'S GST                           
         B     POST059                                                          
         AP    TRCSAMNT,TOTGST                                                  
*                                                                               
POST057  CLI   BATTYPE,6           BAT06 POSTS USER'S BASIS                     
         BE    *+8                                                              
         CLI   BATTYPE,26          BAT26 TOO                                    
         BNE   POST059                                                          
         OC    PBASIS,PBASIS                                                    
         BZ    POST059                                                          
         ZAP   TRCSAMNT,PBASIS                                                  
*                                                                               
POST059  CLI   GINPUT,C'Y'         TEST FOR INPUT TAX TRANS                     
         BNE   *+10                                                             
         MP    TRCSAMNT,=P'-1'                                                  
         LA    R2,TRCSLNQ1(R2)                                                  
*                                                                               
POST060  CLI   BATTYPE,26                                                       
         BNE   POST100                                                          
         CLC   PDATDATE,PWOSDATE   TEST FOR TAX POINT OVERRIDE                  
         BE    POST100                                                          
         USING GDAELD,R2                                                        
         MVI   GDAEL,GDAELQ        ADD A GENERAL DATE ELEMENT                   
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATTAXP    TYPE=TAX TYPE                                
         MVC   GDADATE,PWOSDATE                                                 
         LA    R2,GDALNQ(R2)                                                    
*                                                                               
         USING DLPOSTD,R2                                                       
POST100  XC    DLPSEL(DLPSLNQ),DLPSEL                                           
         MVI   DLPSEL,DLPSEDRQ     DEBIT TO GST/PST                             
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         L     R1,CURGSTVT                                                      
         CLI   OVERGPST,C'G'                                                    
         BNE   POST103                                                          
         ZIC   RE,VTCTYPE          SAVE FOR LATER                               
         B     *+8                                                              
POST103  L     R1,CURPSTVT                                                      
         CLI   BATTYPE,6           OUTPUT TYPE POSTINGS                         
         BE    POST105                                                          
         CLI   BATTYPE,26          CHECK IF 26 IS INPUT/OUTPUT?                 
         BNE   POST104                                                          
         CLI   GINPUT,C'Y'                                                      
         BNE   POST105                                                          
*                                                                               
POST104  MVC   DLPSDBAC,VTCACT     EXTRACT ACCOUNT FROM VATICAN                 
         MVC   DLPSDBNM,VTCACTNM                                                
*                                                                               
         MVC   DLPSCRAC,CNTRACC    MOVE CONTRA ACC                              
         MVC   DLPSCRNM,CNTRACCN                                                
         B     POST107                                                          
*                                                                               
*                                                                               
POST105  MVI   DLPSEL,DLPSECRQ                                                  
         MVC   DLPSCRAC,VTCACT                                                  
         MVC   DLPSCRNM,VTCACTNM                                                
         MVC   DLPSDBAC,CNTRACC                                                 
         MVC   DLPSDBNM,CNTRACCN                                                
*                                                                               
POST107  MVC   DLPSANAL,COFFICE    OFFICE=FINANCIAL OFFICE                      
         CLI   BATTYPE,46                                                       
         BNE   POST108                                                          
         L     R6,AOFFC                                                         
         MVC   DLPSANAL,0(R6)      EXPENSE VENDOR, OFFICE                       
*                                                                               
POST108  ZAP   DLPSAMNT,TOTGST                                                  
         CLI   BATTYPE,46                                                       
         BNE   *+10                                                             
         ZAP   DLPSAMNT,AMTGST     WORKCODE'S GST                               
         CLI   OVERGPST,C'G'                                                    
         BE    POST109                                                          
         ZAP   DLPSAMNT,TOTPST                                                  
         CLI   BATTYPE,46                                                       
         BNE   *+10                                                             
         ZAP   DLPSAMNT,AMTPST     WORKCODE'S PST                               
POST109  CLI   GINPUT,C'Y'                                                      
         BNE   *+10                                                             
         MP    DLPSAMNT,=P'-1'                                                  
         LA    R2,DLPSLNQ(R2)                                                   
         B     POST200                                                          
*                                                                               
POST150  MVI   OVERGPST,C'P'                                                    
         CLI   GSTPSTT,C'*'        NO PST                                       
         BE    POST250                                                          
         CLI   GSTPSTPH+5,0        ANY PROVINCE CODE?                           
         BZ    POST250             NO PST AVAILABLE                             
         B     POST050                                                          
*                                                                               
         USING SCIELD,R2                                                        
POST200  CLI   GSTGSTT,C'*'                                                     
         BE    POST210                                                          
* SAYS   CLI   GINPUT,C'N'         TYPE 26, GST INPUT?                          
* VLAPN  BE    POST207             SKIP TAX MEMO POSTING                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q       ADD TAX PAID MEMO TO                         
         MVI   SCITYPE,SCITTAXP    GST TAX MEMO                                 
         ZAP   SCIAMNT,TOTGST                                                   
         OC    GBASIS,GBASIS       USE BASIS IF GIVEN                           
         BZ    *+14                                                             
         ZAP   SCIBASE,GBASIS                                                   
         B     POST202                                                          
*                                                                               
         ZAP   SCIBASE,TOTNET                                                   
         CLI   BATTYPE,26                                                       
         BNE   POST203                                                          
         ZAP   SCIBASE,COM1                                                     
         AP    SCIBASE,COM2                                                     
         CP    AMT,=P'0'                                                        
         BE    *+10                                                             
         AP    SCIBASE,TOTNET                                                   
POST202  CLI   GINPUT,C'Y'                                                      
         BNE   *+16                                                             
         MP    SCIAMNT,=P'-1'                                                   
         MP    SCIBASE,=P'-1'                                                   
         B     POST205                                                          
*                                                                               
POST203  CLI   BATTYPE,46                                                       
         BNE   POST205                                                          
         ZAP   SCIAMNT,AMTGST      WORKCODE'S GST                               
         ZAP   SCIBASE,AMTNET                                                   
POST205  MVC   SCISUBPR,SPACES     SPACE FILLED FOR GST                         
         STC   RE,SCISUBPT         SAVE GST TYPE BEFORE                         
         LA    R2,SCILN3Q(R2)                                                   
POST207  CLI   OVERGPST,C'G'                                                    
         BE    POST150                                                          
*                                                                               
POST210  DS    0H                                                               
* SAYS   CLI   GINPUT,C'N'         TYPE 26, GST INPUT?                          
* VLAPN  BE    POST250             SKIP TAX MEMO POSTING                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q       ADD TAX PAID MEMO TO                         
         MVI   SCITYPE,SCITTQST    SUPPLIER CREDIT                              
         ZAP   SCIAMNT,TOTPST                                                   
         OC    PBASIS,PBASIS                                                    
         BZ    *+14                                                             
         ZAP   SCIBASE,PBASIS                                                   
         B     POST212                                                          
*                                                                               
         ZAP   SCIBASE,TOTNET      QST BASIS                                    
         AP    SCIBASE,TOTGST                                                   
         CLI   BATTYPE,26                                                       
         BNE   POST213                                                          
         ZAP   SCIBASE,COM1                                                     
         AP    SCIBASE,COM2                                                     
         AP    SCIBASE,TOTGST                                                   
         CP    AMT,=P'0'                                                        
         BE    *+10                                                             
         AP    SCIBASE,TOTNET                                                   
POST212  CLI   GINPUT,C'Y'                                                      
         BNE   *+16                                                             
         MP    SCIAMNT,=P'-1'                                                   
         MP    SCIBASE,=P'-1'                                                   
         B     POST220                                                          
*                                                                               
POST213  CLI   BATTYPE,46                                                       
         BNE   POST220                                                          
         ZAP   SCIAMNT,AMTPST                                                   
         ZAP   SCIBASE,AMTNET      QST BASIS                                    
         AP    SCIBASE,AMTGST                                                   
POST220  MVC   SCISUBPR,GSTPSTP    COPY PROVINCE CODE                           
         MVC   SCISUBPT,VTCTYPE                                                 
         LA    R2,SCILN3Q(R2)                                                   
*                                                                               
POST250  LR    R4,R2               CALCULATE LENGTH                             
         SR    R4,RF                 OF POSTING SECTIONS                        
         STCM  R4,3,0(RF)          SAVE LENGTH                                  
         CLI   BATTYPE,46          BATCH 46, LOOP THRU WORKCODES                
         BNE   POSTX                                                            
         LR    RF,R2               NEW POSITION                                 
         XC    0(2,R2),0(R2)       CLEAR LENGTH                                 
         LA    R3,AMTLNQ(R3)       NEXT WORKCODE FOR BAT46                      
         L     R1,CURGSTVT                                                      
         LA    R1,VTCLNQ(R1)                                                    
         ST    R1,CURGSTVT                                                      
         L     R1,CURPSTVT                                                      
         LA    R1,VTCLNQ(R1)                                                    
         ST    R1,CURPSTVT                                                      
         L     R1,AOFFC                                                         
         LA    R1,2(R1)                                                         
         ST    R1,AOFFC                                                         
*                                                                               
         B     POST010                                                          
POSTX    B     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*              CONSTANTS                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
LEAVE    DC    C'INPUT COMPLETE, PLEASE RETURN TO THE INVOICE.'                 
*                                                                               
DICI     DS    0X                                                               
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#ONT,3                                                         
         DCDDL AC#HST,3                                                         
         DC    AL1(EOT)                                                         
*                                                                               
       ++INCLUDE ACPRVTAB                                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR LOCAL W/S                                              
*                                                                               
LWSD     DSECT                                                                  
ADTAXB   DS    A                   A(USERS INPUT BLOCK)                         
ADWCTAB  DS    A                   A(WORKCODE TABLE)                            
AAREA    DS    A                                                                
AAMNTS   DS    A                   A(AMOUNTS CALCULATED)                        
AAMNTS2  DS    A                   A(AMOUNTS CALCULATED PST)                    
BATTYPE  DS    CL1                 BATCH INPUT TYPE                             
ELCODE   DS    CL1                                                              
ACTIVITY DS    CL1                                                              
SCRTOT   DS    PL6                                                              
*L13     DS    PL13                                                             
SAVRE    DS    F                                                                
*                                                                               
OFFICE   DS    CL2                                                              
DOCNO    DS    CL6                                                              
PDATE    DS    CL3                                                              
ORDERNO  DS    CL6                                                              
*                                                                               
JOB      DS    CL15                JOB     CODE                                 
JBNME    DS    CL36                        NAME                                 
PRD      DS    CL15                PRODUCT CODE                                 
PRNME    DS    CL36                        NAME                                 
CLI      DS    CL15                CLIENT  CODE                                 
CLNME    DS    CL36                        NAME                                 
KEY      DS    CL49                                                             
GSTVATBL DS    4CL(VTCLNQ)         GST VATICAN BLOCK                            
PSTVATBL DS    4CL(VTCLNQ)         PST VATICAN BLOCK                            
CTAMTTAB DS    8XL(AMTLNQ)         TEMP WORKCODE AMOUNT TABLE                   
NAMTS    DS    XL1                 NUMBER OF WORKCODES                          
NOVER    DS    XL1                 NUMBER OF WORKCODES                          
NGST     DS    XL1                 NUMBER OF GST OVERRIDES                      
NPST     DS    XL1                 NUMBER OF PST OVERRIDES                      
TOTNET   DS    PL6                                                              
TOTGST   DS    PL6                                                              
TOTPST   DS    PL6                                                              
TOTGRS   DS    PL6                                                              
CALCTOT  DS    PL6                 CALCULATED TOTAL                             
ONETAX   DS    PL6                 ONE TAX AMOUNT                               
ALARGEST DS    A                                                                
GPSTAPPL DS    CL1                                                              
AMTBLKT  DS    CL64                                                             
OVERGPST DS    CL1                 GST/PST FLAG FOR OVERAMT PROC                
TMPHEADR DS    CL9                 TEMPORARY HEADER FOR VATICAN GST/PST         
RETERR   DS    XL1                 RETURN ERROR CODE                            
GBASIS   DS    PL8                 GST BASIS OVERRIDE (TYPE 6)                  
PBASIS   DS    PL8                 PST BASIS OVERRIDE (TYPE 6)                  
GINPUT   DS    CL1                 GST INPUT, TYPE 46 ONLY                      
CURGSTVT DS    A                   CURRENT GST VATICAN BLOCK                    
CURPSTVT DS    A                   CURRENT PST VATICAN BLOCK                    
*                                                                               
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
         DS    XL1                                                              
*                                                                               
* ACBATCTAX                                                                     
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
* ACGOBLOCK                                                                     
* GOBLOCKC DS    CL(GOBLOCKX-GOBLOCK)                                           
*                                                                               
*                                                                               
LWKEY    DS    CL42                                                             
LWIO     DS    1000C                                                            
MXLNES   EQU   10                  MAXIMUM NUMBER OF LINES                      
         DS    0F                                                               
IOAREA   DS    2000C                                                            
PSTABLE  DS    (MXLNES*PSTLINE)C  TABLE OF POSTING ACCOUNTS (14 LINES)          
LWSX     DS    0C                                                               
         EJECT                                                                  
*              DSECT TO COVER WORK CODE AMOUNTS                                 
AMTD     DSECT                                                                  
AMTWC    DS    CL2                 WORKCODE                                     
AMTNET   DS    PL6                 NET                                          
AMTGRS   DS    PL6                 GROSS                                        
AMTGST   DS    PL6                 GST                                          
AMTPST   DS    PL6                 PST                                          
AMTLNQ   EQU   *-AMTD                                                           
         ORG   AMTGRS              FOR TYPE 26 ONLY                             
AMTAMT   DS    PL6                 AMOUNT FROM MAIN                             
AMTCOM   DS    PL6                 COMMISION                                    
AMTCOM2  DS    PL6                 COMMISION 2                                  
*              DSECT TO COVER INPUT LINE                                        
TXD      DSECT                                                                  
TXGONH   DS    CL(L'GSTCGONH)      HEADER FOR GROSS/NET                         
TXGON    DS    CL(L'GSTCGON)       GROSS/NET                                    
TXGSTTH  DS    CL(L'GSTGSTTH)      GST TYPE HEADER                              
TXGSTT   DS    CL(L'GSTGSTT)       GST TYPE                                     
TXGSTNH  DS    CL(L'GSTGSTNH)      GST TYPE NAME HEADER                         
TXGSTN   DS    CL(L'GSTGSTN)       GST TYPE NAME                                
TXGSTXH  DS    CL(L'GSTGSTXH)      GST TYPE EXTENDED HEADER                     
TXGSTX   DS    CL(L'GSTGSTX)       GST TYPE EXTENDED                            
TXGSTAH  DS    CL(L'GSTGSTAH)      GST AMOUNT HEADER                            
TXGSTA   DS    CL(L'GSTGSTA)       GST AMOUNT                                   
TXPSTPH  DS    CL(L'GSTPSTPH)      PST PROVINCE HEADER                          
TXPSTP   DS    CL(L'GSTPSTP)       PST PROVINCE                                 
TXPSTTH  DS    CL(L'GSTPSTTH)      PST TYPE HEADER                              
TXPSTT   DS    CL(L'GSTPSTT)       PST TYPE                                     
TXPSTNH  DS    CL(L'GSTPSTNH)      PST TYPE NAME HEADER                         
TXPSTN   DS    CL(L'GSTPSTN)       PST TYPE NAME                                
TXPSTXH  DS    CL(L'GSTPSTXH)      PST TYPE EXTENDED HEADER                     
TXPSTX   DS    CL(L'GSTPSTX)       PST TYPE EXTENDED                            
TXPSTAH  DS    CL(L'GSTPSTAH)      PST AMOUNT HEADER                            
TXPSTA   DS    CL(L'GSTPSTA)       PST AMOUNT                                   
TXLNQ    EQU   *-TXD               LENGTH OF INPUT LINE                         
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER POSTING DATA FOR A LINE                           
*                                                                               
PSTD     DSECT                                                                  
PSTACC   DS    CL15                CREDIT ACCOUNT                               
PSTNME   DS    CL36                ACCOUNT NAME                                 
PSTLOC   DS    CL14                LOCALITY                                     
PSTLCNM  DS    CL36                LOCALITY NAME                                
PSTWKC   DS    CL2                 WORKCODE                                     
PSTEFF   DS    CL3                 EFFECTIVE DATE                               
PSTPCT   DS    PL4                 PERCENT                                      
PSTBAS   DS    PL6                 BASIS                                        
PSTAMT   DS    PL6                 POSTING AMOUNT                               
PSTLNQ   EQU   *-PSTD                                                           
PSTLINE  EQU   PSTLNQ*4            4 POSSIBLE ENTRIES PER LINE                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         ORG   CONTABH                                                          
       ++INCLUDE ACBATBFD                                                       
*              OSVALS+550                                                       
         ORG   TWAHOLE                                                          
* CALLING OVERLAYS (ACBAT01, ACBAT03) USE FIRST 200 BYTES                       
*                                                                               
USERL    EQU   OSVALSL-550                                                      
LASTBASE DS    PL6                 BASIS                                        
PWOSDATE DS    PL3                 PWOS START DATE                              
COFFICE  DS    CL2                 CREDIT OFFICE                                
AOFFC    DS    A                   TYPE 46'S CURRENT OFFICE                     
COFFCLST DS    CL8                 TYPE 46'S OFFICE LIST                        
AMTBLK   DS    CL64                AMOUNT BLOCK                                 
CNTRACC  DS    CL15                                                             
CNTRACCN DS    CL36                                                             
GSTVENDT DS    CL1                 GST VENDOR TYPE, IF TYPE N/A                 
PSTVENDT DS    CL1                 PST VENDOR TYPE, IF TYPE N/A                 
USERPST  DS    CL1                 USER ENTERED PST                             
USERPROV DS    CL1                 USER ENTERED PROVINCE                        
AMT      DS    PL6                 AMOUNT                                       
COM1     DS    PL6                 COMMISION 1                                  
COM2     DS    PL6                 COMMISION 2                                  
PDATDATE DS    PL3                 PWOS START DATE, TYPE 26                     
SVGSTCGN DS    CL1                 GSTCGON FOR OVERAMTS RTE                     
CPROV    DS    CL2                 PROVINCE CODE                                
*              CL(USERL-(*-LASTBASE))   SPARE                                   
         EJECT                                                                  
* ACGENBOTH                                                                     
* ACGENDAY                                                                      
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073ACBAT41   04/16/13'                                      
         END                                                                    
