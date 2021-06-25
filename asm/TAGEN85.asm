*          DATA SET TAGEN85    AT LEVEL 011 AS OF 08/20/15                      
*PHASE T70285C,*                                                                
         TITLE 'T70285 - OPTIONS DISPLAY'                                       
T70285   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70285                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
                                                                                
         CLI   MODE,VALKEY                                                      
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,DISPREC                                                     
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
PFTAB    DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'HISTORY ',CL8'DISPLAY'                               
PF13     DC    AL1(KEYTYTWA,L'OPTAGY-1),AL2(OPTAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'OPTINV-1),AL2(OPTINV-T702FFD)                     
PF13X    DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'40',OPTAGYH),(X'80',OPTINVH)                      
         JE    XIT                                                              
                                                                                
         GOTO1 (RF),(R1),(1,OPTCIDH),(X'80',OPTOPTH)                            
         GOTO1 (RF),(R1),(1,OPTLIDH),OPTMEDNH                                   
                                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',OPTAGYH),OPTAGYNH                     
                                                                                
         LA    R2,OPTINVH                                                       
         CLI   5(R2),0         IF NO INVOICE INPUT                              
         JNE   VK10                                                             
         OC    TGINV,TGINV     AND GLOBAL IS NOT SET                            
         JZ    VKMISS          RETURN MISSING ERROR                             
                                                                                
         MVC   SVINV,TGINV                                                      
         XC    SVINV,VKHEXFFS                                                   
         GOTO1 TINVCON,DMCB,SVINV,OPTINV,DATCON                                 
         MVI   5(R2),6                                                          
                                                                                
VK10     GOTO1 TINVCON,DMCB,OPTINV,SVINV,DATCON                                 
         CLI   0(R1),X'FF'                                                      
         JE    VKINV                                                            
         XC    SVINV,VKHEXFFS                                                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',SVINV)                                
         JNE   VKEND                                                            
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TAINSTA2,TAINSADJ   IF NOT AN ADJUSTMENT                         
         JO    VK20                                                             
         TM    TAINSTAT,TAINSPAY   OK IF PAID ALREADY                           
         JO    VK20                                                             
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL            GET PAYMENT DETAILS ELEMENT                  
         JNE   VKNPD                                                            
         TM    TAPDOPT3,TAPDOGRY   IF NOT GREY PAYMENT                          
         JZ    VKNPD               INVOICE MUST BE PAID ALREADY                 
         DROP  R4                                                               
                                                                                
VK20     GOTO1 FLDVAL,DMCB,(X'20',OPTAGYH),(X'80',OPTINVH)                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR VALKEY ROUTINE                            *         
***********************************************************************         
                                                                                
VKINV    MVI   ERROR,INVALID                                                    
         J     VKEND                                                            
                                                                                
VKMISS   MVI   ERROR,MISSING                                                    
         J     VKEND                                                            
                                                                                
VKNPD    MVI   ERROR,ERNOTPD                                                    
         J     VKEND                                                            
                                                                                
VKEND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VKEY ROUTINES                     *         
***********************************************************************         
                                                                                
VKHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         BAS   RE,VALDISP                                                       
                                                                                
         USING TAVRD,R4                                                         
         MVI   ELCODE,TAVRELQ      IF PAYMENT WAS TO A VERSION                  
         BRAS  RE,GETEL            DISPLAY VERSION CODE AND ID                  
         JNE   DR10                                                             
         EDIT  TAVRVERS,OPTLFT,ALIGN=LEFT                                       
         MVC   OPTLID,TAVRCID                                                   
         J     DR20                                                             
                                                                                
         USING TALFD,R4                                                         
DR10     L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      IF PAYMENT WAS TO A LIFT                     
         BRAS  RE,GETEL            DISPLAY LIFT INDICATOR AND ID                
         JNE   DR20                                                             
         MVI   OPTLFT,C'Y'                                                      
         MVC   OPTLID,TALFLID                                                   
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
DR20     L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JNE   DR30                                                             
                                                                                
         MVC   OPTCID,TACOCID      DISPLAY COMMERCIAL ID                        
         GOTO1 MEDVAL,DMCB,TACOMED MEDIA                                        
         MVC   OPTMEDN,TGMENAME    AND MEDIA                                    
                                                                                
         CLI   TACOSEC,0           IF COMMERCIAL LENGTH IS SAVED                
         BE    DR30                DISPLAY IT                                   
         EDIT  TACOSEC,(3,OPTSEC+1),ALIGN=LEFT                                  
         MVI   OPTSEC,C':'                                                      
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
DR30     L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVPDPST1,TAPDPST1   SAVE FIRST PAYMENT STATUS                    
                                                                                
         TM    TAPDSTA2,TAPDSLFA   IF PAYMENT TO ALL ON COMM'L                  
         JZ    *+8                                                              
         MVI   OPTLFT,C'A'         SET LFT TO A                                 
                                                                                
         MVC   AIO,AIO2                                                         
         GOTOR RECVAL,DMCB,TLCLCDQ,(X'88',TAPDCLI),OPTCLINH                     
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'88',TAPDCOM),OPTCOMNH                    
         MVC   AIO,AIO1                                                         
                                                                                
         MVC   OPTCLPR(L'TGCLI),TAPDCLI                                         
                                                                                
         OC    TAPDPRD,TAPDPRD                                                  
         JZ    DR60                                                             
         LA    R1,OPTCLPR+L'TAPDCLI-1                                           
DR40     CLI   0(R1),X'40'                                                      
         JH    DR50                                                             
         BCT   R1,DR40                                                          
DR50     MVI   1(R1),C'/'                                                       
         MVC   2(L'TAPDPRD,R1),TAPDPRD                                          
                                                                                
         USING OPTD,R3                                                          
DR60     LA    R3,OPTTAB           R3=A(ENTRY IN OPTTAB)                        
         LA    R5,BLOCK            R5=A(CURRENT BLOCK ENTRY)                    
         XR    R0,R0               R0=NUMBER OF ENTRIES IN BLOCK                
         XR    R1,R1                                                            
                                                                                
DR70     CLI   0(R3),X'FF'         CHECK FOR END OF OPTTAB                      
         JE    DR100                                                            
         ZIC   RE,OPTSTAT          DISPL. OF STATUS BYTE + A(TAPD EL.)          
         AR    RE,R4               = RE = A(STATUS BYTE)                        
         ZIC   R1,OPTBIT                                                        
         EX    R1,*+8                                                           
         J     *+8                                                              
         TM    0(RE),0             TEST FOR BIT MASK                            
         JO    DR80                BRANCH IF MATCH                              
         LA    R3,OPTNEXT          BUMP TO NEXT OPTTAB ENTRY                    
         J     DR70                                                             
                                                                                
DR80     LH    RF,OPTDIS           RF=DISP. TO DISPLAY ROUTINE                  
         AR    RF,RB                                                            
         LA    RE,DR90             SET RE FOR COMMON NTR1                       
         MVI   EDITSTAT,0                                                       
         NTR1                                                                   
         BR    RF                  ** OFF TO DISPLAY ROUTINE **                 
                                                                                
DR90     AHI   R0,1                INCREMENT COUNT OF ENTRIES IN BLOCK          
         LA    R5,20(R5)           BUMP TO NEXT ENTRY IN BLOCK                  
         LA    R3,OPTNEXT          BUMP TO NEXT ENTRY IN OPTTAB                 
         J     DR70                                                             
                                                                                
DR100    CLI   TAPDLEN,TAPDLNQ                                                  
         JL    DR109                                                            
                                                                                
         TM    TAPDOPT5,TAPDORG2                                                
         JZ    DR105                                                            
         MVC   0(10,R5),=CL10'RG2' PROCESS IN SECOND REGRESSION RUN             
         MVC   10(10,R5),=CL10'Y'                                               
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
                                                                                
DR105    TM    TAPDOPT5,TAPDONHW                                                
         JZ    DR109                                                            
         MVC   0(10,R5),=CL10'NHW' DO NOT PRINT H&W COLUMN                      
         MVC   10(10,R5),=CL10'Y'                                               
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
         DROP  R4                                                               
                                                                                
         USING TAPAD,R4                                                         
DR109    L     R4,AIO                                                           
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATFEE))                                     
         JNE   DR110                                                            
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         MVC   0(10,R5),=CL10'FEE'                                              
         NI    EDITSTAT,X'FF'-NODEC                                             
         BAS   RE,EDITBLK                                                       
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
DR110    L     R4,AIO                                                           
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTASC))                                     
         JNE   DR111                                                            
         L     R4,TGELEM                                                        
         MVC   0(10,R5),=CL10'ASC' AGENCY STUDIO CODE                           
         MVC   10(6,R5),TAFNNAME                                                
         MVC   16(4,R5),SPACES                                                  
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
         DROP  R4                                                               
                                                                                
DR111    LTR   R0,R0               TEST IF ANY OPTIONS TO DISPLAY               
         JZ    DR120                                                            
         XC    0(20,R5),0(R5)      CLEAR END OF BLOCK                           
         GOTO1 UNSCAN,DMCB,((R0),BLOCK),OPTOPTH                                 
                                                                                
DR120    GOTO1 FLDVAL,DMCB,(4,OPTCIDH),OPTOPTH                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE DISPLAY FIELD                            *         
*        ON ENTRY ... R4=A(INVOICE RECORD)                            *         
***********************************************************************         
                                                                                
VALDISP  NTR1                                                                   
         MVI   DISPSTAT,0                                                       
         XC    ECVTRATE,ECVTRATE                                                
                                                                                
         CLI   OPTDISPH+5,0                                                     
         JE    XIT                                                              
                                                                                
         ZIC   RE,OPTDISPH+5       IF INPUT IS EUROS ...                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   =C'EUROS'(0),OPTDISP                                             
         BNE   DISPINV                                                          
                                                                                
         LR    R2,R4                                                            
                                                                                
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   DISPINV                                                          
         TM    TAPDPST2,TAPDPEUR   INVOICE MUST BE IN EUROS                     
         JZ    DISPINV                                                          
         OI    DISPSTAT,DSEUROS    SET TO DISPLAY IN EUROS                      
                                                                                
         LR    R4,R2                                                            
         MVI   ELCODE,TAEUELQ      EXIT IF WE DON'T HAVE EURO-BASED             
         BRAS  RE,GETEL            PAYMENT DETAILS ELEMENT                      
         JNE   XIT                                                              
                                                                                
         USING TABDD,R4                                                         
         LR    R4,R2               GET BILLING DETAILS ELEMENT                  
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         ZAP   DUB,TABDCCVT        AND SAVE EURO CONVERSION RATE                
         CVB   R1,DUB              IN BINARY                                    
         ST    R1,ECVTRATE                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
DISPINV  LA    R2,OPTDISPH                                                      
         MVI   ERROR,INVALID                                                    
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        SIMPLE OPTION DISPLAY ROUTINES                               *         
***********************************************************************         
                                                                                
DISAPH   MVC   0(10,R5),=CL10'A'   APPLY CREDITS-CODE FROM HOLDING FEE          
         MVC   10(10,R5),=CL10'H'                                               
         J     XIT                                                              
                                                                                
DISNAC   MVC   0(10,R5),=CL10'A'   DON'T APPLY CREDITS                          
         MVC   10(10,R5),=CL10'N'                                               
         J     XIT                                                              
                                                                                
DISNGC   MVC   0(10,R5),=CL10'G'   DON'T TAKE GUARANTEE CREDITS                 
         MVC   10(10,R5),=CL10'N'                                               
         J     XIT                                                              
                                                                                
DISCAN   MVC   0(10,R5),=CL10'C'   TAKE CANADIAN TAXES                          
         MVC   10(10,R5),=CL10'Y'                                               
         J     XIT                                                              
                                                                                
DISURG   MVC   0(10,R5),=CL10'U'   URGENT PAYMENT - DUE TODAY                   
         MVC   10(10,R5),=CL10'Y'                                               
         J     XIT                                                              
                                                                                
DISNOI   MVC   0(10,R5),=CL10'NI'  NO INTERFACE                                 
         MVC   10(10,R5),=CL10'Y'                                               
         J     XIT                                                              
                                                                                
DISGRE   MVC   0(10,R5),=CL10'G'   APPLY TO GUARANTEE BASED ON END DATE         
         MVC   10(10,R5),=CL10'E'                                               
         J     XIT                                                              
                                                                                
DISDCL   MVC   0(10,R5),=CL10'R'   DUE COMPANY RECOVER FROM CLIENT ONLY         
         MVC   10(10,R5),=CL10'C'                                               
         J     XIT                                                              
                                                                                
DISDAY   MVC   0(10,R5),=CL10'R'   DUE COMPANY RECOVER FROM AGENCY ONLY         
         MVC   10(10,R5),=CL10'A'                                               
         J     XIT                                                              
                                                                                
DISDAL   MVC   0(10,R5),=CL10'R'   DUE COMPANY RECOVER FROM ALL                 
         MVC   10(10,R5),=CL10'L'                                               
         J     XIT                                                              
                                                                                
DISDUM   MVC   0(10,R5),=CL10'D'   DUMMY PAYMENT                                
         MVC   10(10,R5),=CL10'Y'                                               
         J     XIT                                                              
                                                                                
DISRET   MVC   0(10,R5),=CL10'R'   RETROACTIVE PAYMENT                          
         MVC   10(10,R5),=CL10'Y'                                               
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANURT4I))                                     
         JNE   XIT                                                              
         USING TANUD,RE                                                         
         L     RE,TGELEM                                                        
         GOTO1 TINVCON,DMCB,TANUMBER,10(R5),DATCON                              
         J     XIT                                                              
                                                                                
DISPUR   MVC   0(10,R5),=CL10'PUR' PUR INVOICE                                  
         MVC   10(10,R5),=CL10'Y'                                               
         J     XIT                                                              
                                                                                
DISAPS   MVC   0(10,R5),=CL10'A'   APPLY CREDITS-CODE FROM SESSION              
         MVC   10(10,R5),=CL10'S'                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        OVERRIDE P&H RATE ROUTINE                                    *         
***********************************************************************         
                                                                                
DISPHR   MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATPHR))                                     
         JNE   DPHR10                                                           
                                                                                
         USING TAPAD,RE                                                         
         L     RE,TGELEM                                                        
         ZICM  R1,TAPADATA,2                                                    
         J     DPHR20                                                           
         DROP  RE                                                               
                                                                                
         USING TAPDD,R4                                                         
DPHR10   MVC   10(10,R5),=CL10'?'                                               
         OC    TAPDSPNH,TAPDSPNH                                                
         JZ    DPHR20                                                           
         MVC   10(10,R5),=CL10'0'                                               
         OC    TAPDPNH,TAPDPNH                                                  
         JZ    DPHR20                                                           
         SR    R0,R0                                                            
         L     R1,TAPDPNH                                                       
         M     R0,=F'100000'                                                    
         D     R0,TAPDSPNH                                                      
         XR    R0,R0                                                            
         D     R0,=F'5'                                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
DPHR20   BAS   RE,EDITBLK                                                       
DPHR30   MVC   0(10,R5),=CL10'P'                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OVERRIDE TAX ROUTINE                                         *         
***********************************************************************         
                                                                                
         USING TAPAD,R4                                                         
DISTAX   MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATTAX))                                     
         JNE   DTAX10                                                           
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         J     DTAX20                                                           
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
DTAX10   L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         L     R1,TABDTAX                                                       
DTAX20   MVC   0(10,R5),=CL10'T'                                                
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OVERRIDE HANDLING ROUTINE                                    *         
***********************************************************************         
                                                                                
         USING TAPDD,R4                                                         
DISHND   MVI   BYTE,0                                                           
         TM    TAPDOPT3,TAPDOHNC                                                
         JZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
                                                                                
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATHND))                                     
         BNE   DHND10                                                           
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         B     DHND20                                                           
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
DHND10   L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         L     R1,TABDHND                                                       
         CLI   BYTE,C'Y'                                                        
         JNE   *+8                                                              
         L     R1,TABDHNDC                                                      
DHND20   MVC   0(10,R5),=CL10'H'                                                
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OVERRIDE CHLOE HANDLING AMOUNT ROUTINE                       *         
***********************************************************************         
                                                                                
         USING TAPDD,R4                                                         
DISCH    MVI   BYTE,0                                                           
         TM    TAPDOPT4,TAPDOHDA                                                
         JZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
                                                                                
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATCHA))                                     
         JNE   XIT                                                              
                                                                                
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         MVC   0(10,R5),=CL10'CH'                                               
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OVERRIDE CHLOE HANDLING PERCENT ROUTINE                      *         
***********************************************************************         
                                                                                
         USING TAPDD,R4                                                         
DISCP    MVI   BYTE,0                                                           
         TM    TAPDOPT4,TAPDOHDP                                                
         JZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
                                                                                
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATCHO))                                     
         JNE   XIT                                                              
                                                                                
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         MVC   0(10,R5),=CL10'CP'                                               
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OVERRIDE FICA CREDITS ROUTINE                                *         
***********************************************************************         
                                                                                
         USING TAPAD,R4                                                         
DISFCR   MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATFIC))                                     
         JNE   DFCR10                                                           
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         J     DFCR20                                                           
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
DFCR10   L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         L     R1,TABDFICR                                                      
DFCR20   MVC   0(10,R5),=CL10'F'                                                
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OVERRIDE TOTAL USES OPTION ROUTINE                           *         
***********************************************************************         
                                                                                
         USING TAPDD,R4                                                         
DISPTU   MVC   0(10,R5),=CL10'TU'                                               
         LH    R1,TAPDSTUS                                                      
         BCTR  R1,0                                                             
                                                                                
         OI    EDITSTAT,NODEC                                                   
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OVERRIDE LIFT USES OPTION ROUTINE                            *         
***********************************************************************         
                                                                                
         USING TANDD,R4                                                         
         L     R4,AIO                                                           
DISPLU   MVI   ELCODE,TANDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   0(10,R5),=CL10'LU'                                               
         LH    R1,TANDSTUL                                                      
         OI    EDITSTAT,NODEC                                                   
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OVERRIDE NUMBER OF USES TO PAY OPTION ROUTINE                *         
***********************************************************************         
                                                                                
         USING TAPDD,R4                                                         
DISNUS   MVC   0(10,R5),=CL10'US'                                               
         LH    R1,TAPDUSES                                                      
         OI    EDITSTAT,NODEC                                                   
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OVERRIDE H&W ADJUSTMENT AMOUNT OPTION ROUTINE                *         
***********************************************************************         
                                                                                
         USING TAPAD,R4                                                         
DISHNW   MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATHNW))                                     
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         MVC   0(10,R5),=CL10'HW'                                               
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         OI    EDITSTAT,FLOAT                                                   
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OVERRIDE CANADIAN CONVERSION RATE OPTION ROUTINE             *         
***********************************************************************         
                                                                                
         USING TABDD,R4                                                         
DISCCR   L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   0(10,R5),=CL10'RA'                                               
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OVERRIDE CONTRACT SERVICE FEE OPTION ROUTINE                 *         
***********************************************************************         
                                                                                
DISCSF   MVC   0(10,R5),=CL10'CSF'                                              
         MVC   10(10,R5),=CL10'Y'                                               
                                                                                
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATCSF))                                     
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OVERRIDE GST AMOUNT OPTION ROUTINE                           *         
***********************************************************************         
                                                                                
         USING TAPAD,R4                                                         
DISGST   MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATGST))                                     
         BNE   DGST10                                                           
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BAS   RE,CVTR1EUR                                                      
         B     DGST20                                                           
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
DGST10   L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         L     R1,TABDGST                                                       
DGST20   MVC   0(10,R5),=CL10'GST'                                              
         BAS   RE,EDITBLK                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OVERRIDE CABLE RATE OPTION ROUTINE                           *         
***********************************************************************         
                                                                                
         USING TAPAD,R4                                                         
DISCRR   MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATSOC))                                     
         JNE   DCRR10                                                           
         L     R4,TGELEM                                                        
         XR    R1,R1                                                            
         ICM   R1,3,TAPADATA                                                    
         BAS   RE,EDITBLK                                                       
         J     DCRR20                                                           
                                                                                
DCRR10   MVC   10(10,R5),=CL10'Y'                                               
DCRR20   MVC   0(10,R5),=CL10'RR'                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        EDIT OPTION AMOUNT                                           *         
***********************************************************************         
                                                                                
EDITBLK  NTR1                                                                   
         LA    R4,10(R5)                                                        
         LTR   R1,R1                                                            
         JNZ   *+14                                                             
         MVC   0(10,R4),=CL10'0'                                                
         J     XIT                                                              
                                                                                
         TM    SVPDPST1,TAPDPCRD                                                
         JZ    *+6                                                              
         LCR   R1,R1                                                            
                                                                                
         TM    EDITSTAT,FLOAT                                                   
         JZ    EDITB3                                                           
         EDIT  (R1),(9,1(R4)),2,ALIGN=LEFT                                      
         J     EDITB10                                                          
                                                                                
EDITB3   TM    EDITSTAT,NODEC                                                   
         JZ    EDITB5                                                           
         EDIT  (R1),(10,(R4)),ALIGN=LEFT                                        
         J     XIT                                                              
                                                                                
EDITB5   EDIT  (R1),(10,(R4)),2,ALIGN=LEFT,FLOAT=-                              
                                                                                
EDITB10  CLI   0(R4),C'.'                                                       
         JE    *+12                                                             
         LA    R4,1(R4)                                                         
         J     EDITB10                                                          
                                                                                
         CLC   0(3,R4),=C'.00'                                                  
         JNE   EDITB20                                                          
         MVC   0(3,R4),=3C' '                                                   
                                                                                
         TM    EDITSTAT,FLOAT                                                   
         JO    EDITB25                                                          
         J     XIT                                                              
                                                                                
EDITB20  CLI   2(R4),C'0'                                                       
         JNE   *+8                                                              
         MVI   2(R4),C' '                                                       
                                                                                
         TM    EDITSTAT,FLOAT                                                   
         JZ    XIT                                                              
EDITB25  MVI   10(R5),C'+'                                                      
         LTR   R1,R1                                                            
         JP    XIT                                                              
         MVI   10(R5),C'-'                                                      
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONVERT US$ AMOUNT IN R1 TO EUROS                            *         
*        ON ENTRY ... R1 = AMOUNT                                               
***********************************************************************         
                                                                                
CVTR1EUR NTR1                                                                   
         OC    ECVTRATE,ECVTRATE   IF EURO CONVERSION RATE                      
         JZ    CR1EX               HAS BEEN SAVED                               
                                                                                
         XR    R0,R0               DIVIDE US$ AMOUNT BY                         
         LTR   R1,R1               IS R1 NEGATIVE?                              
         BNM   *+8                                                              
         AHI   R0,-1               MAKE R0 NEGATIVE TOO                         
                                                                                
         M     R0,=F'10000'        EURO CONVERSION RATE                         
         D     R0,ECVTRATE                                                      
                                                                                
         MHI   R0,10                                                            
         L     RE,ECVTRATE         ROUND THE RESULT                             
         AHI   RE,1                                                             
         SRA   RE,1                                                             
         CR    R0,RE                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
                                                                                
CR1EX    XIT1  REGS=(R1)           AND RETURN IT IN R1                          
         EJECT                                                                  
**********************************************************************          
*        TABLE TO COVER OPTION FIELD ENTRIES                         *          
**********************************************************************          
                                                                                
OPTTAB   DC    AL1(TAPDOPT1-TAPDD,TAPDOAPH),AL2(DISAPH-DR)      A=H             
         DC    AL1(TAPDOPT1-TAPDD,TAPDONAC),AL2(DISNAC-DR)      A=N             
         DC    AL1(TAPDOPT1-TAPDD,TAPDONGC),AL2(DISNGC-DR)      G=N             
         DC    AL1(TAPDOPT1-TAPDD,TAPDOCAN),AL2(DISCAN-DR)      C=Y             
         DC    AL1(TAPDOPT1-TAPDD,TAPDOPHR),AL2(DISPHR-DR)      P=RATE          
         DC    AL1(TAPDOPT1-TAPDD,TAPDOTAX),AL2(DISTAX-DR)      T=AMNT          
         DC    AL1(TAPDOPT1-TAPDD,TAPDOHND),AL2(DISHND-DR)      H=AMNT          
         DC    AL1(TAPDOPT2-TAPDD,TAPDOURG),AL2(DISURG-DR)      U=Y             
         DC    AL1(TAPDOPT2-TAPDD,TAPDODCL),AL2(DISDCL-DR)      R=C             
         DC    AL1(TAPDOPT2-TAPDD,TAPDODAY),AL2(DISDAY-DR)      R=A             
         DC    AL1(TAPDOPT2-TAPDD,TAPDODAL),AL2(DISDAL-DR)      R=L             
         DC    AL1(TAPDOPT2-TAPDD,TAPDOFCR),AL2(DISFCR-DR)      F=AMNT          
         DC    AL1(TAPDOPT2-TAPDD,TAPDOAPS),AL2(DISAPS-DR)      A=S             
         DC    AL1(TAPDOPT2-TAPDD,TAPDOPTU),AL2(DISPTU-DR)      TU=#            
         DC    AL1(TAPDOPT2-TAPDD,TAPDOPLU),AL2(DISPLU-DR)      LU=#            
         DC    AL1(TAPDOPT3-TAPDD,TAPDONUS),AL2(DISNUS-DR)      US=#            
         DC    AL1(TAPDOPT3-TAPDD,TAPDOHNW),AL2(DISHNW-DR)     HW=+-AMT         
         DC    AL1(TAPDOPT3-TAPDD,TAPDOCCR),AL2(DISCCR-DR)      RA=RATE         
         DC    AL1(TAPDOPT3-TAPDD,TAPDODUM),AL2(DISDUM-DR)      D=Y             
         DC    AL1(TAPDOPT3-TAPDD,TAPDORET),AL2(DISRET-DR)      R=Y             
         DC    AL1(TAPDOPT3-TAPDD,TAPDOCOD),AL2(DISPUR-DR)      COD=Y           
         DC    AL1(TAPDOPT4-TAPDD,TAPDOCSF),AL2(DISCSF-DR)      CSF=Y           
         DC    AL1(TAPDOPT1-TAPDD,TAPDOGST),AL2(DISGST-DR)      GST=AMT         
         DC    AL1(TAPDOPT4-TAPDD,TAPDOSOC),AL2(DISCRR-DR)      RR=Y            
         DC    AL1(TAPDOPT4-TAPDD,TAPDONOI),AL2(DISNOI-DR)      NI=Y            
         DC    AL1(TAPDOPT4-TAPDD,TAPDGRTE),AL2(DISGRE-DR)      NI=Y            
         DC    AL1(TAPDOPT4-TAPDD,TAPDOHDA),AL2(DISCH-DR)       CH=AMT          
         DC    AL1(TAPDOPT4-TAPDD,TAPDOHDP),AL2(DISCP-DR)       CP=PCT          
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR85D                                                       
         EJECT                                                                  
**********************************************************************          
*        SAVED VARIABLES                                             *          
**********************************************************************          
                                                                                
         DS    D                                                                
                                                                                
SVINV    DS    XL(L'TGINV)                                                      
SVPDPST1 DS    XL(L'TAPDPST1)                                                   
                                                                                
EDITSTAT DS    CL1                 STATUS FOR EDITING OPTIONS                   
NODEC    EQU   X'80'               NO DECIMAL PLACES                            
FLOAT    EQU   X'40'               FLOATING SIGNS (+ AND -)                     
                                                                                
DISPSTAT DS    CL1                 STATUS FOR DISPLAY OPTIONS                   
DSEUROS  EQU   X'80'                                                            
                                                                                
ECVTRATE DS    F                   EURO CONVERSION RATE                         
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
OPTD     DSECT                     DSECT TO COVER OPTTAB                        
OPTSTAT  DS    AL1                 DISPLACEMENT TO STATUS BYTE IN 80 EL         
OPTBIT   DS    AL1                 BIT MASK FOR OPTION                          
OPTDIS   DS    AL2                 DISPLACEMENT TO DISPLAY ROUTINE              
OPTNEXT  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011TAGEN85   08/20/15'                                      
         END                                                                    
