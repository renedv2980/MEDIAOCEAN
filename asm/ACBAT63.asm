*          DATA SET ACBAT63    AT LEVEL 012 AS OF 10/20/03                      
*PHASE T61B63A,*                                                                
BAT63    TITLE '- BATCH PROGRAM MOS-LOCK CONTROL HANDLING'                      
BAT63    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BA63**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         USING BSVALS,R5                                                        
         LH    R8,=Y(BSDICTU-TWAD)                                              
         LA    R8,TWAD(R8)                                                      
         USING BSDICTU,R8          R8=A(UPPER CASE DICTIONARY WORDS)            
         L     RC,AOVERWRK                                                      
         USING OVERWRK,RC                                                       
         ST    RE,BORELO                                                        
         SPACE 1                                                                
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3          R3=A(RECORD/ACTION ENTRY)                    
         SPACE 1                                                                
         LA    R2,IOKEY            READ COMPANY RECORD                          
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         LA    R1,IOREAD+IOACCMST+IO1                                           
         CLI   MIXACTB,ACTCHA                                                   
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)       IF CHANGE, READ FOR UPDATE                   
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         SPACE 1                                                                
         CLI   MIXROUT,CONROUTM                                                 
         BL    *+6                                                              
         DC    H'0'                                                             
         XR    RF,RF                                                            
         IC    RF,MIXROUT                                                       
         SLL   RF,2                                                             
         LA    RE,XIT                                                           
         B     CONROUTS(RF)                                                     
         SPACE 1                                                                
CONROUTS DS    0XL4                                                             
         B     DISCON                                                           
         B     VALCON                                                           
CONROUTM EQU   (*-CONROUTS)/L'CONROUTS                                          
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE CONTROLS                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALCON   NTR1  ,                                                                
         SPACE 1                                                                
         CLI   TWASCRN,CTRLSCRN    TEST CONTROLS SCREEN DISPLAYED               
         BE    VCON2                                                            
         BAS   RE,DISCON           IF NOT, DISPLAY IT                           
         MVC   FVMSGNO,=AL2(AI$ENCHA)                                           
         LA    R0,CONOMLH                                                       
         ST    R0,BOCURSOR                                                      
         B     VALCONX                                                          
         SPACE 1                                                                
VCON2    GOTO1 FINDEL,CPYELQ                                                    
         USING CPYELD,R3           R3=A(COMPANY ELEMENT)                        
         SPACE 1                                                                
         MVI   FVMINL,1            VALIDATE MOS-OVERRIDE                        
         GOTO1 AFVAL,CONOMLH                                                    
         BNE   VALCONX                                                          
         OI    CPYSTAT3,CPYSOPBM                                                
*        OI    CPYSTAT6,CPYSBANY   'ANY' NOT VALID FOR NEW FORMAT               
*        GOTO1 COMPWORD,BOPARM,(FVILEN,FVIFLD),(L'UC@ANY,UC@ANY)                
*        BE    VCON4                                                            
         NI    CPYSTAT6,FF-CPYSBANY                                             
         GOTO1 COMPWORD,BOPARM,(FVILEN,FVIFLD),(L'UC@YES,UC@YES)                
         BE    VCON4                                                            
         NI    CPYSTAT3,FF-CPYSOPBM                                             
         GOTO1 (RF),(R1),,(L'UC@NO,UC@NO)                                       
         BE    VCON4                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALCONX                                                          
         SPACE 1                                                                
VCON4    MVI   FVMINL,1            VALIDATE DATES OVERRIDE                      
         GOTO1 AFVAL,CONODTH                                                    
         BNE   VALCONX                                                          
         OI    CPYSTAT4,CPYSOV12                                                
         GOTO1 COMPWORD,BOPARM,(FVILEN,FVIFLD),(L'UC@YES,UC@YES)                
         BE    VCON6                                                            
         NI    CPYSTAT4,FF-CPYSOV12                                             
         GOTO1 (RF),(R1),,(L'UC@NO,UC@NO)                                       
         BE    VCON6                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALCONX                                                          
         SPACE 1                                                                
VCON6    MVI   FVMINL,1            COMPANY MOS-LOCK                             
         GOTO1 VALMOS,BOPARM,(0,CPYMOSX),CONCMOSH                               
         BNE   VALCONX                                                          
         SPACE 1                                                                
*&&US                                                                           
         MVI   FVMINL,1            COMPANY SECURITY LEVEL                       
         GOTO1 VALSEC,BOPARM,CPYBSEC,CONCSECH                                   
         BNE   VALCONX                                                          
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*   - BUILD MOS-LOCK ELEMENT                                          *         
***********************************************************************         
         SPACE 1                                                                
         XC    BOELEM,BOELEM       BUILD MOS-LOCK ELEMENT                       
         LA    R3,BOELEM                                                        
         USING MSLELD,R3           R3=A(MOS LOCK ELEMENT)                       
         MVI   MSLEL,MSLELQ                                                     
         LA    R2,MSLVALS2                                                      
         USING MSLVALS2,R2         R2=A(LOCK VALUES SUB-ELEMENT)                
         SPACE 1                                                                
         MVI   MSLVTYP,0           NEW FORMAT CARRIES OVERRIDE MTH/YR           
         GOTO1 VALMOS,BOPARM,(1,MSLVMON),CONCMOSH                               
         BNE   VALCONX                                                          
         MVC   OVSDEFMM,MSLVMON    SAVE DEFAULT MONTH AND YEAR                  
         MVC   OVSDEFYY,MSLVYEAR                                                
*&&US*&& GOTO1 VALSEC,BOPARM,MSLVSEC,CONSECH                                    
*&&US*&& BNE   VALCONX                                                          
         LA    R2,L'MSLVALS2(R2)                                                
         SPACE 1                                                                
         LA    R7,1                R7=SUB-EL.COUNTER                            
         LA    R0,CONLINEN         R0=NO. OF LINES                              
         LA    R4,CONBTYH                                                       
         USING CONBTYH,R4          R4=A(INPUT LINE)                             
         SPACE 1                                                                
VCON12   CLI   CONBTYH+FHILD,0                                                  
         BE    VCON38                                                           
         GOTO1 VALBTYP,BOPARM,MSLVTYP,CONBTYH                                   
         BNE   VALCONX                                                          
         SPACE 1                                                                
         GOTO1 VALMOS,BOPARM,(1,MSLVMON),CONMOSH                                
         BNE   VALCONX                                                          
         SPACE 1                                                                
*&&US                                                                           
         GOTO1 VALSEC,BOPARM,MSLVSEC,CONSECH                                    
         BNE   VALCONX                                                          
*&&                                                                             
         SPACE 1                                                                
         CLI   MSLVMON,MSLVSDEF    TEST AT LEAST ONE FIELD NOT DEFAULT          
         BNE   VCON14                                                           
*&&US                                                                           
         TM    MSLVSEC,MSLVSDEF                                                 
         BZ    VCON14                                                           
         LA    R0,CONMOSH                                                       
         ST    R0,BOCURSOR                                                      
*&&                                                                             
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     VALCONX                                                          
         SPACE 1                                                                
VCON14   LTR   RF,R7               RF=NO. OF PREVIOUS SUB-ELS.                  
         BZ    VCON30                                                           
         DROP  R2                                                               
         LR    R1,R2               R1=A(CURRENT SUB-ELEMENT)                    
         USING MSLVALS2,R1                                                      
         LA    RE,L'MSLVALS2       RE=L'SUB-ELEMENT                             
         SPACE 1                                                                
VCON22   SR    R1,RE               BUMP R1 BACK 1 SUB-ELEMENT                   
         CLC   MSLVTYP,MSLVTYP+L'MSLVALS2                                       
         BL    VCON30              (SUB ELEMENT IN CORRECT PLACE)               
         BH    VCON28                                                           
         ST    R4,BOCURSOR         DUPLICATE BATCH TYPE FOUND                   
         MVC   FVMSGNO,=AL2(AE$DUPIF)                                           
         B     VALCONX                                                          
         SPACE 1                                                                
VCON28   XC    MSLVALS2,MSLVALS2+L'MSLVALS2  BUMP SUB-EL. UP LIST               
         XC    MSLVALS2+L'MSLVALS2(L'MSLVALS2),MSLVALS                          
         XC    MSLVALS2,MSLVALS2+L'MSLVALS2                                     
         BCT   RF,VCON22                                                        
         SPACE 1                                                                
         DROP  R1                                                               
         USING MSLVALS2,R2                                                      
         SPACE 1                                                                
VCON30   LA    R7,1(R7)            UPDATE NUMBER OF SUB-ELS.                    
         LA    R2,L'MSLVALS2(R2)   BUMP R2 TO NEXT SUB EL.                      
         SPACE 1                                                                
VCON38   LA    R4,CONLINEL(R4)     BUMP R4 TO NEXT INPUT LINE                   
         BCT   R0,VCON12                                                        
         SPACE 1                                                                
         SR    R2,R3               SET ELEMENT LENGTH                           
         STC   R2,MSLLN                                                         
         STC   R7,MSLNUM                                                        
         DROP  R2,R4                                                            
         SPACE 1                                                                
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('MSLELQ',AIO1),0,0                  
         CLI   MSLNUM,0            UPDATE MOS-LOCK ELEMENT                      
         BE    VCON50                                                           
         GOTO1 (RF),(R1),(C'P',ACCMST),AIO1,MSLELD                              
         SPACE 3                                                                
VCON50   GOTO1 AIO,IOWRITE+IOACCMST+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 FINDEL,CPYELQ       COPY CHANGED COMPANY ELEMENT DETAILS         
         MVC   BCCPYEL(CPYLN1Q),0(R3)   INTO BCCPYEL                            
         SPACE 1                                                                
         BAS   RE,DISCON           RE-DISPLAY CONTROLS                          
         SPACE 1                                                                
VALCONX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY CONTROLS                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISCON   NTR1  ,                                                                
         SPACE 1                                                                
         GOTO1 AOVRSCR,BOPARM,('CTRLSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         GOTO1 FINDEL,CPYELQ                                                    
         USING CPYELD,R3           R3=A(COMPANY ELEMENT)                        
         SPACE 1                                                                
         MVC   CONOML,UC@NO        DISPLAY MOS-OVERRIDE                         
         TM    CPYSTAT3,CPYSOPBM                                                
         BZ    *+10                                                             
         MVC   CONOML,UC@YES                                                    
         TM    CPYSTAT6,CPYSBANY                                                
         BZ    *+10                                                             
         MVC   CONOML,UC@ANY                                                    
         OI    CONOMLH+FHOID,FHOITR                                             
         SPACE 1                                                                
         MVC   CONODT,UC@NO        DISPLAY DATES OVERRIDE                       
         TM    CPYSTAT4,CPYSOV12                                                
         BZ    *+10                                                             
         MVC   CONODT,UC@YES                                                    
         OI    CONODTH+FHOID,FHOITR                                             
         SPACE 1                                                                
         GOTO1 DISMOS,BOPARM,CPYMOSX,CONCMOSH                                   
         SPACE 1                                                                
*&&US*&& GOTO1 DISSEC,BOPARM,CPYBSEC,CONCSECH                                   
         SPACE 1                                                                
         GOTO1 FINDEL,MSLELQ                                                    
         BNE   DCON10                                                           
         USING MSLELD,R3           R3=A(MOS LOCK ELEMENT)                       
         SPACE 1                                                                
         XR    R7,R7                                                            
         ICM   R7,1,MSLNUM         R7=NO. OF SUB-ELS.                           
         BZ    DCON10                                                           
         LA    R2,MSLVALS                                                       
         USING MSLVALS,R2          R2=A(VALUES ENTRY)                           
         SPACE 1                                                                
         CLI   MSLVTYP,0           NEW FORMAT CARRIES OVERRIDE MTH/YR           
         BNE   DCON1                                                            
         GOTO1 DISMOS,BOPARM,MSLVMON,CONCMOSH                                   
         LA    R2,L'MSLVALS2(R2)                                                
         SH    R7,=H'1'                                                         
         BZ    DCON10                                                           
         SPACE 1                                                                
DCON1    LA    R4,CONBTYH                                                       
         USING CONBTYH,R4          R4=A(DISPLAY LINE)                           
         SPACE 1                                                                
DCON2    OI    CONBTYH+FHOID,FHOITR                                             
         CURED MSLVTYP,(L'CONBTY,CONBTY),0,ALIGN=LEFT,DMCB=BOPARM               
         SPACE 1                                                                
         GOTO1 DISMOS,BOPARM,MSLVMON,CONMOSH                                    
         SPACE 1                                                                
*&&US*&& GOTO1 DISSEC,BOPARM,MSLVSEC,CONSECH                                    
         SPACE 1                                                                
DCON8    TM    MSLVSEC,MSLVSXTN                                                 
         BO    *+12                                                             
         LA    R2,L'MSLVALS(R2)                                                 
         B     *+8                                                              
         LA    R2,L'MSLVALS2(R2)                                                
         LA    R4,CONLINEL(R4)                                                  
         BCT   R7,DCON2                                                         
         DROP  R2,R3,R4                                                         
         SPACE 1                                                                
DCON10   MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         LA    R0,BASRECH                                                       
         ST    R0,BOCURSOR                                                      
         SPACE 1                                                                
DISCONX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH TYPE                                                 *         
*                                                                     *         
* NTRY: P1=A(BATCH TYPE)                                              *         
*       P2=A(INPUT FIELD)                                             *         
* EXIT: BATCH TYPE IS SET                                             *         
***********************************************************************         
         SPACE 1                                                                
VALBTYP  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING MSLVTYP,R2          R2=A(BATCH TYPE)                             
         USING FHD,R3              R3=A(INPUT FIELD)                            
         SPACE 1                                                                
         GOTO1 AFVAL,FHD                                                        
         BNE   VALBTYPN                                                         
         TM    FVIIND,FVINUM       TEST INPUT IS NUMERIC                        
         BZ    VALBTYPN                                                         
         OC    BCFULL(3),BCFULL    AND ONE BYTE LONG                            
         BNZ   VALBTYPN                                                         
         MVC   MSLVTYP,BCFULL+3                                                 
         SPACE 1                                                                
         CLI   MSLVTYP,30                                                       
         BE    VALBTYPY                                                         
         CLI   MSLVTYP,37                                                       
         BE    VALBTYPY                                                         
*&&US*&& CLI   MSLVTYP,58                                                       
*&&US*&& BE    VALBTYPY                                                         
         SPACE 1                                                                
         L     R4,ATYPTAB                                                       
         USING TYPTABD,R4          R4=A(BATCH TYPE TABLE)                       
         SPACE 1                                                                
VALBTYP2 CLI   TYPNUM,EOT                                                       
         BE    VALBTYPN                                                         
         CLC   TYPNUM,MSLVTYP                                                   
         BNE   VALBTYP4                                                         
         CLI   TYPCTRY,CTRYALL     TEST ALL COUNTRIES                           
         BE    VALBTYP6                                                         
         CLC   TYPCTRY,CUCTRY      TEST FOR SINGLE VALID COUNTRY                
         BE    VALBTYP6                                                         
         MVC   BCWORK(L'TYPCTRY),TYPCTRY                                        
         NI    BCWORK,FF-CTRYNOT                                                
         TM    TYPCTRY,CTRYNOT     TEST TYPE EXCLUDED FOR A COUNTRY             
         BNO   VALBTYP4                                                         
         CLC   CUCTRY,BCWORK       TEST THIS IS THE EXCLUDED COUNTRY            
         BNE   VALBTYP6                                                         
         SPACE 1                                                                
VALBTYP4 LA    R4,TYPTABL(R4)                                                   
         B     VALBTYP2                                                         
         SPACE 1                                                                
VALBTYP6 TM    TYPIND2,TYPIDDS     TEST DDS ONLY BATCH TYPE                     
         BNO   VALBTYPY                                                         
         TM    CUSTAT,CUSDDS       TEST CONNECTED USER IS DDS                   
         BNO   VALBTYPN                                                         
         SPACE 1                                                                
VALBTYPY CR    RB,RB                                                            
         B     XIT                                                              
VALBTYPN MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOS-LOCK                                                   *         
*                                                                     *         
* NTRY: P1=A(MOS),HOB:0=UPDATING CPYEL (MONTH);1=MSLEL (MONTH/YEAR)   *         
*       P2=A(INPUT FIELD)                                             *         
* EXIT: MOS IS SET                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALMOS   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         MVC   BOBYTE1,0(R1)                                                    
         USING MSLVMON,R2          R2=MOS                                       
         USING FHD,R3              R3=A(INPUT FIELD)                            
         SPACE 1                                                                
         GOTO1 AVALBMO,FHD                                                      
         BE    VMOS2                                                            
         BNL   VALMOSN                                                          
*        MVI   MSLVMON,MSLVSDEF    NO INPUT/USE DEFAULTS - OLD FORMAT           
         CLI   BOBYTE1,1                                 - NEW FORMAT           
         BNE   VALMOSY                                                          
         MVC   MSLVMON,OVSDEFMM    CARRY DEFAULTS IF NEW FORMAT                 
         MVC   MSLVYEAR,OVSDEFYY                                                
         OI    MSLVSEC,MSLVSXTN+MSLVSDUM  EXTENDED & DUMB ELEMENT               
         B     VALMOSY                                                          
         SPACE 1                                                                
VMOS2    LA    R4,BCWORK                                                        
         USING PERVALD,R4          R4=A(PERVAL BLOCK)                           
         SPACE 1                                                                
         TM    PVALASSM,PVALASY    TEST YEAR NOT ENTERED                        
*        BO    VMOS6                                                            
         CLC   BCTMONP,PVALPSTA    INPUT MOS MUST NOT BE AFTER TODAY'S          
         BNL   VMOS6               MONTH                                        
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     VALMOSN                                                          
         SPACE 1                                                                
VMOS6    CLI   BOBYTE1,1           UPDATING CPYEL OR MSLEL                      
         BNE   *+14                                                             
         MVC   MSLVYEAR,PVALPSTA   NEW FORMAT CARRIES YEAR                      
         OI    MSLVSEC,MSLVSXTN    EXTENDED ELEMENT                             
         MVC   MSLVMON,PVALPSTA+1  CONVERT MONTH FROM PACKED FORMAT             
         TR    MSLVMON,MONTABLE      TO CHARACTER FORMAT                        
         SPACE 1                                                                
VALMOSY  CLI   BOBYTE1,1                                                        
         BNE   VALMOSY2                                                         
         GOTO1 DISMOS,BOPARM,MSLVMON,FHD                                        
VALMOSY2 CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
VALMOSN  LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY MOS-LOCK                                                    *         
*                                                                     *         
* NTRY: P1=A(MOS)                                                     *         
*       P2=A(OUTPUT FIELD)                                            *         
***********************************************************************         
         SPACE 1                                                                
DISMOS   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING MSLVMON,R2          R2=A(MOS)                                    
         USING FHD,R3              R3=A(OUTPUT FIELD)                           
         XC    FHDA(L'CONMOS),FHDA                                              
         OI    FHOI,FHOITR                                                      
         SPACE 1                                                                
         CLI   MSLVMON,MSLVSDEF    TEST FOR USE DEFAULT - OLD FORMAT            
         BE    DISMOSX                                                          
         TM    MSLVSEC,MSLVSDUM                         - NEW FORMAT            
         BO    DISMOSX                                                          
         CLI   MSLVMON,0                                                        
         BE    DISMOSX                                                          
         SPACE 1                                                                
         LA    RE,L'MONTABLE-1     CONVERT CHARATER MONTH                       
         LA    RF,MONTABLE+L'MONTABLE-1  TO PACKED MONTH                        
DMOS02   CLC   MSLVMON,0(RF)                                                    
         BE    DMOS04                                                           
         BCTR  RF,0                                                             
         BCT   RE,DMOS02                                                        
         B     DISMOSX             INVALID CHARACTER                            
         SPACE 1                                                                
DMOS04   MVC   OVPDY,BCTMONP       SET OVPDYMD TO MOS-LOCK                      
         TM    MSLVSEC,MSLVSXTN                                                 
         BZ    *+10                                                             
         MVC   OVPDY,MSLVYEAR                                                   
         STC   RE,OVPDM                                                         
         MVI   OVPDD,X'01'         SET FIRST OF THE MONTH                       
         CLC   OVPDYM,BCTMONP      ENSURE MOS-LOCK IS NOT AFTER THIS            
         BNH   DMOS06              MONTH                                        
BOW      USING OVPDY,BOWORK1                                                    
         MVC   BOW.OVPDYMD,OVPDY   SET JANUARY 1ST, CURRENT YEAR                
         MVI   BOW.OVPDM,X'01'                                                  
         GOTO1 VDATCON,BOPARM,(X'31',BOW.OVPDY),(1,BOW.OVPDY),(4,0)             
         MVC   OVPDY,BOW.OVPDY     SET PREVIOUS YEAR                            
         DROP  BOW                                                              
DMOS06   GOTO1 VDATCON,BOPARM,(1,OVPDYM),(9,FHDA)                               
         SPACE 1                                                                
DISMOSX  B     XIT                                                              
         SPACE 1                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* VALIDATE SECURITY LEVEL                                             *         
*                                                                     *         
* NTRY: P1=A(SECURITY LEVEL)                                          *         
*       P2=A(INPUT FIELD)                                             *         
* EXIT: SECURITY LEVEL IS SET                                         *         
***********************************************************************         
         SPACE 1                                                                
VALSEC   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING MSLVSEC,R2          R2=A(SECURITY LEVEL)                         
         USING FHD,R3              R3=A(INPUT FIELD)                            
         SPACE 1                                                                
         GOTO1 SCAN,FHD            SCAN INPUT                                   
         BNE   VALSECN                                                          
         SPACE 1                                                                
         XR    R0,R0                                                            
         ICM   R0,1,OVSCANN        R0=NO. SCAN ENTRIES                          
         BNZ   VSEC2                                                            
         OI    MSLVSEC,MSLVSDEF    SET OUTPUT=DEFAULT                           
         B     VALSECY                                                          
         SPACE 1                                                                
VSEC2    NI    MSLVSEC,X'FF'-(MSLVSDEF+MSLVSSEC+MSLVSOFF)  CHECK NONE           
         GOTO1 COMPWORD,BOPARM,(L'OVSCAN,OVSCAN),(L'UC@NONE,UC@NONE)            
         BE    VALSECY                                                          
         LA    R4,OVSCAN                                                        
         SPACE 1                                                                
VSEC4    GOTO1 COMPWORD,BOPARM,(L'OVSCAN,(R4)),(L'UC2LGR,UC2LGR)                
         BNE   *+12                                                             
         OI    MSLVSEC,MSLVSSEC                                                 
         B     VSEC8                                                            
         GOTO1 (RF),(R1),,(L'UC2OFF,UC2OFF)                                     
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSECN                                                          
         OI    MSLVSEC,MSLVSOFF                                                 
         SPACE 1                                                                
VSEC8    LA    R4,L'OVSCAN(R4)     GET NEXT SCAN ENTRY                          
         BCT   R0,VSEC4                                                         
         SPACE 1                                                                
VALSECY  GOTO1 DISSEC,BOPARM,MSLVSEC,FHD                                        
         CR    RB,RB                                                            
         B     XIT                                                              
VALSECN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY SECURITY LEVEL                                              *         
*                                                                     *         
* NTRY: P1=A(SECURITY LEVEL)                                          *         
*       P2=A(OUTPUT FIELD)                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEC   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING MSLVSEC,R2          R2=A(SECURITY LEVEL)                         
         USING FHD,R3              R3=A(OUTPUT FIELD)                           
         XC    FHDA(L'CONCSEC),FHDA                                             
         OI    FHOI,FHOITR                                                      
         SPACE 1                                                                
         TM    MSLVSEC,MSLVSDEF    TEST USE DEFAULT VALUE                       
         BO    DISSECX                                                          
         SPACE 1                                                                
         TM    MSLVSEC,MSLVSDEF+MSLVSSEC+MSLVSOFF  TEST NONE                    
         BNZ   *+14                                                             
         MVC   FHDA(L'UC@NONE),UC@NONE                                          
         B     DISSECX                                                          
         SPACE 1                                                                
         TM    MSLVSEC,MSLVSSEC+MSLVSOFF  LEDGER/ACCOUNT &  OFFICE              
         BNO   DISSEC02                                                         
         MVC   FHDA(L'UC2LGR),UC2LGR                                            
         MVC   FHDA+L'UC2LGR(L'BCCOMMA),BCCOMMA                                 
         MVC   FHDA+L'UC2LGR+L'BCCOMMA(L'UC2OFF),UC2OFF                         
         B     DISSECX                                                          
         SPACE 1                                                                
DISSEC02 TM    MSLVSEC,MSLVSSEC    LEDGER/ACCOUNT LEVEL                         
         BZ    *+14                                                             
         MVC   FHDA(L'UC2LGR),UC2LGR                                            
         B     DISSECX                                                          
         SPACE 1                                                                
         TM    MSLVSEC,MSLVSOFF    OFFICE LEVEL                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FHDA(L'UC2OFF),UC2OFF                                            
DISSECX  B     XIT                                                              
         DROP  R2,R3                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* SCANNER ROUTINE                                                     *         
*                                                                     *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: OVSCANN=NO. OF ENTRIES,  OVSCAN=ENTRIES                       *         
***********************************************************************         
         SPACE 1                                                                
SCAN     NTR1  ,                                                                
         MVC   OVSCAN,BCSPACES     INITIALIZE SCAN BLOCK                        
         MVI   OVSCANN,0                                                        
         GOTO1 AFVAL                                                            
         BH    SCANN                                                            
         SPACE 1                                                                
         LA    R2,FVIFLD           R2=A(START)                                  
         LA    R0,FVIFLD+L'FVIFLD  R0=A(END)+1                                  
         SPACE 1                                                                
         LA    R4,OVSCAN           R4=A(BLOCK)                                  
         SPACE 1                                                                
SCAN2    CR    R2,R0               TEST START AT END OF FIELD                   
         BNL   SCANY                                                            
         CLI   0(R2),C' '          GET RID OF PRECEDING SPACES                  
         BH    *+12                                                             
         LA    R2,1(R2)                                                         
         B     SCAN2                                                            
         LR    R3,R2                                                            
         SPACE 1                                                                
SCAN4    CLC   BCCOMMA,0(R3)       BUMP R3 TO NEXT COMMA                        
         BE    SCAN6                 OR TO END OF FIELD                         
         LA    R3,1(R3)                                                         
         CR    R3,R0                                                            
         BL    SCAN4                                                            
         SPACE 1                                                                
SCAN6    LR    RE,R3               RE=LENGTH OF ENTRY                           
         SR    RE,R2                                                            
         BZ    SCAN8                                                            
         LA    RF,L'OVSCAN                                                      
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4              COPY ENTRY                                   
         MVC   0(0,R4),0(R2)                                                    
         LA    R4,L'OVSCAN(R4)                                                  
         IC    RE,OVSCANN                                                       
         LA    RE,1(RE)                                                         
         STC   RE,OVSCANN                                                       
         SPACE 1                                                                
SCAN8    LA    R2,1(R3)                                                         
         B     SCAN2                                                            
         SPACE 1                                                                
SCANY    CR    RB,RB                                                            
         B     XIT                                                              
SCANN    LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COMPARE INPUT TO DICTIONARY ENTRY                        *         
*                                                                     *         
* NTRY: P1=(L'INPUT, A(INPUT))                                        *         
*       P2=A(DICTIONARY WORD)                                         *         
* EXIT: CC=EQUAL IF MATCH                                             *         
***********************************************************************         
         SPACE 1                                                                
COMPWORD NTR1  ,                                                                
         LM    R2,R3,0(R1)         R2=A(START OF INPUT)                         
         XR    RE,RE               RE=L(INPUT)                                  
         IC    RE,0(R1)                                                         
         CLC   4(1,R1),0(R1)                                                    
         BNL   *+8                                                              
         IC    RE,4(R1)                                                         
         LA    RF,0(R2,RE)         RF=A(END OF INPUT)                           
         SPACE 1                                                                
CWRD2    BCTR  RF,0                DELETE TRAILING ZEROS                        
         CLI   0(RF),C' '                                                       
         BH    CWRD4                                                            
         BCT   RE,CWRD2                                                         
         LTR   RB,RB               EXIT CC=NOT EQUAL                            
         B     XIT                                                              
         SPACE 1                                                                
CWRD4    BCTR  RE,0                COMPARE WORDS                                
         EX    RE,*+8                                                           
         B     XIT                                                              
         CLC   0(0,R2),0(R3)                                                    
         SPACE 3                                                                
***********************************************************************         
* FIND ELEMENT IN IO1                                                 *         
*                                                                     *         
* NTRY: R1=ELEMENT CODE                                               *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT), CC=NOT EQUAL IF NOT FOUND       *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   L     R3,AIO1                                                          
         LA    R3,CPYRFST-CPYRECD(R3)                                           
         XR    RF,RF                                                            
         SPACE 1                                                                
FEL2     CLI   0(R3),0             TEST E-O-R                                   
         BE    FINDELN                                                          
         SPACE 1                                                                
         IC    RF,1(R3)            RF=LENGTH                                    
         SPACE 1                                                                
         CLM   R1,1,0(R3)          MATCH ON ELEMENT CODE                        
         BER   RE                                                               
         SPACE 1                                                                
         BXH   R3,RF,FEL2          BUMP R2 TO NEXT ELEMENT                      
         SPACE 1                                                                
FINDELN  LTR   RB,RB               SET CC=NOT EQUAL                             
         BR    RE                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST  '                                                      
         SPACE 1                                                                
MONTABLE DC    C'.123456789......ABC' MONTHS FOR PACKED TO CHAR CONV.           
         EJECT                                                                  
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   BASOLY1H                                                         
       ++INCLUDE ACBATA8D                                                       
CONLINEL EQU   CONBTY2H-CONBTYH    LENGTH OF 1 LINE                             
CONLINES EQU   CONPFKH-CONBTYH                                                  
CONLINEN EQU   CONLINES/CONLINEL   NUMBER OF LINES                              
         ORG   OSVALS                                                           
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         EJECT                                                                  
WORKD    DSECT                     OVERLAY W/S                                  
         ORG   OVERWRK                                                          
         SPACE 1                                                                
OVPDYMD  DS    0PL3                * DUMMY PACKED DATE *                        
OVPDYM   DS    0PL2                YYMM                                         
OVPDY    DS    PL1                 YY = YEAR                                    
OVPDM    DS    PL1                 MM = MONTH                                   
OVPDD    DS    PL1                 DD = MONTH                                   
         SPACE 1                                                                
OVSCANN  DS    XL1                 NO. OF SCAN ENTRIES                          
OVSCAN   DS    10XL8               SCAN ENTRIES                                 
         SPACE 1                                                                
OVSDEFMM DS    XL1                 DEFAULT MONTH (NEW FORMAT)                   
OVSDEFYY DS    XL1                 DEFAULT YEAR (NEW FORMAT)                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACBAT63   10/20/03'                                      
         END                                                                    
