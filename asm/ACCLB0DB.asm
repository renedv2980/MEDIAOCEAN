*          DATA SET ACCLB0DB   AT LEVEL 147 AS OF 12/22/99                      
*PHASE T6210DB                                                                  
CLB0D    TITLE '- BILL PROGRAM - FORMAT CONTROL LIST'                           
CLB0D    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLBD**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         LH    R7,=Y(BSDICT-TWAD)                                               
         LA    R7,TWAD(R7)                                                      
         USING BSDICT,R7                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PBCRECD,IOKEY                                                    
         L     RC,AOVERWRK                                                      
         USING FWORKD,RC                                                        
         USING FBLKD,FFMTBLK                                                    
         ST    RE,BORELO                                                        
         SRL   RF,24                                                            
         SLL   RF,2                                                             
*                                                                               
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     EXITY               LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     VALCLM              VALIDATE COLUMN                              
         B     VALLAST             LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXIT                SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE LIST SELECTION                      
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  MVC   SVFLTS,FLTS                                                      
         XC    FLTS,FLTS                                                        
*                                                                               
         L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDFMT),GOCBDFMT-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   EXITL                                                            
*                                                                               
SFRST30  GOTO1 AFVAL,CLTFMTH       VALIDATE START FORMAT NUMBER                 
         BL    SFRST40                                                          
         TM    FVIIND,FVINUM                                                    
         BZ    SCRFRSTN                                                         
         OC    BCFULL(3),BCFULL                                                 
         BNZ   SCRFRSTN                                                         
         CLI   BCFULL+3,0                                                       
         BE    SCRFRSTN                                                         
         MVC   FLTFMT,BCFULL+3                                                  
*                                                                               
SFRST40  MVI   FLTLNG,FF           VALIDATE LANGUAGE                            
         CLI   CUCTRY,CTRYGER                                                   
         BE    SFRST41                                                          
         XC    CLTLNGW,CLTLNGW                                                  
         XC    CLTLNG,CLTLNG                                                    
         OI    CLTLNGH+FHATD,FHATPR                                             
         B     SFRST50                                                          
SFRST41  GOTO1 AFVAL,CLTLNGH                                                    
         BL    SFRST50                                                          
         L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         USING LANGTABD,R2                                                      
SFRST42  CLI   LANGTABD,FF                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFELANG)                                           
         B     SCRFRSTN                                                         
         GOTO1 CMPWRD,LANGSHR                                                   
         BE    SFRST44                                                          
         GOTO1 CMPWRD,LANGSHRN                                                  
         BE    SFRST44                                                          
         GOTO1 CMPWRD,LANGFUL                                                   
         BE    SFRST44                                                          
         GOTO1 CMPWRD,LANGFULN                                                  
         BE    SFRST44                                                          
         LA    R2,LANGTABL(R2)                                                  
         B     SFRST42                                                          
SFRST44  MVC   FLTLNG,LANGCODE                                                  
         MVC   CLTLNG,LANGFULN                                                  
         OI    CLTLNGH+FHOID,FHOITR                                             
         CLC   FLTLNG,CULANG                                                    
         BNE   *+12                                                             
         MVI   FLTLNG,0                                                         
         B     SFRST50                                                          
         CLI   FLTLNG,LANGENG                                                   
         BNE   SFRST50                                                          
         MVI   FLTLNG,LANGEUK                                                   
         DROP  R2                                                               
*                                                                               
SFRST50  DS    0H                                                               
*FRST50  GOTO1 AFVAL,CLTGRBH       VALIDATE GROUPING BASIS                      
*        BL    SFRST60                                                          
*        TM    FVIIND,FVINUM                                                    
*        BZ    SCRFRSTN                                                         
*        OC    BCFULL(3),BCFULL                                                 
*        BNZ   SCRFRSTN                                                         
*        CLI   BCFULL+3,0                                                       
*        BE    SCRFRSTN                                                         
*        MVC   FLTGRB,BCFULL+3                                                  
*        GOTO1 AGETGRB,BOPARM,FLTGRB,0                                          
*        BNE   SCRFRSTN                                                         
*                                                                               
SFRST60  CLC   FLTS,SVFLTS                                                      
         BE    EXITY                                                            
         B     EXITH                                                            
*                                                                               
SCRFRSTN MVC   FLTS,SVFLTS                                                      
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXITL                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* COMPARE INPUT WITH SOME KIND OF A WORD                              *         
*                                                                     *         
* NTRY: R1=A(WORD)                                                    *         
***********************************************************************         
         SPACE 1                                                                
CMPWRD   LA    RF,2                                                             
         CLI   FVXLEN,2                                                         
         BH    *+8                                                              
         IC    RF,FVXLEN                                                        
         EX    RF,*+6                                                           
         BR    RE                                                               
         CLC   FVIFLD(0),0(R1)                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   LR    R0,R1                                                            
         TM    LSINDS1,LSICLMIN                                                 
         BO    VALCLM02                                                         
         GOTO1 AFMTBLK,BOPARM,('FBGET',FBLKD),AIO1                              
*                                                                               
VALCLM02 GOTO1 AFMTBLK,BOPARM,('FBVAL',FBLKD),(R0)                              
         B     EXIT                                                             
*                                                                               
VALLAST  TM    LSINDS1,LSICLMIN                                                 
         BZ    EXITY                                                            
         GOTO1 AFMTBLK,BOPARM,('FBPUT',FBLKD),AIO1                              
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING PBCRECD,R2                                                       
         STC   R1,BOBYTE1                                                       
         TM    BOBYTE1,X'40'                                                    
         BO    DISCLM02                                                         
         LR    R0,R1                                                            
         GOTO1 AFMTBLK,BOPARM,('FBDIS',FBLKD),(R0)                              
         B     EXIT                                                             
*                                                                               
DISCLM02 SLL   R1,26                                                            
         SRL   R1,24                                                            
         B     *+4(R1)                                                          
         B     DISFMT                                                           
         DC    2H'0'                                                            
         B     DISNAM                                                           
*                                                                               
DISFMT   GOTO1 AFMTBLK,BOPARM,('FBGET',FBLKD),AIO1                              
         XR    R0,R0                                                            
         IC    R0,PBCKFMT                                                       
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  FVIFLD(3),BODUB1                                                 
         CLI   CUCTRY,CTRYGER                                                   
         BNE   EXITY                                                            
         MVC   FVIFLD+3(1),BCSLASH                                              
*                                                                               
DISLNG   L     R3,ALANG                                                         
         LA    R3,6(R3)                                                         
         MVC   BOBYTE1,PBCKLANG                                                 
         CLI   BOBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   BOBYTE1,CULANG                                                   
         B     DLNG02                                                           
         CLI   BOBYTE1,LANGEUK                                                  
         BNE   DLNG02                                                           
         MVI   BOBYTE1,LANGENG                                                  
         USING LANGTABD,R3                                                      
DLNG02   CLI   LANGTABD,FF                                                      
         BE    EXITN                                                            
         CLC   LANGCODE,BOBYTE1                                                 
         BE    DLNG10                                                           
         LA    R3,LANGTABL(R3)                                                  
         B     DLNG02                                                           
*                                                                               
DLNG10   MVC   FVIFLD+4(L'LANGSHRN),LANGSHRN                                    
         B     EXITY                                                            
         DROP  R3                                                               
*                                                                               
*ISGRB   XR    R0,R0                                                            
*        IC    R0,PBCRGRPB                                                      
*        CVD   R0,BODUB1                                                        
*        OI    BODUB1+7,X'0F'                                                   
*        UNPK  FVIFLD(3),BODUB1                                                 
*        GOTO1 AGETGRB,BOPARM,PBCRGRPB,(20,FVIFLD+4)                            
*        B     EXIT                                                             
*                                                                               
DISNAM   TM    PBCRSTAT,PBCSDELT   TEST DELETED RECORD                          
         BZ    *+14                                                             
         MVCDD FVIFLD(36),AC#DELD,F                                             
         B     EXIT                                                             
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('NAMELQ',PBCRECD),0                 
         CLI   12(R1),0                                                         
         BNE   EXIT                                                             
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXIT                                                             
         DROP  RF                                                               
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  XC    PBCKEY,PBCKEY       INITIALIZE THE KEY                           
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,FLTFMT                                                   
         CLI   PBCKFMT,0                                                        
         BNE   *+8                                                              
         MVI   PBCKFMT,1                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT RECORD FOR LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  LA    R1,IOHID+IOACCDIR                                                
         B     *+8                                                              
GETNEXT  LA    R1,IOSQD+IOACCDIR                                                
         GOTO1 AIO                                                              
         BE    *+12                                                             
         CLI   IOERR,IOEDEL                                                     
         BNE   EXITN                                                            
         CLC   PBCKEY(PBCKFMT-PBCRECD),IOKEYSAV                                 
         BNE   EXITN                                                            
         CLI   PBCKSEQ,0           TEST IF CONTINUATION RECORD                  
         BH    GETNEXT                                                          
         CLI   FLTLNG,FF           TEST LANGUAGE FILTER                         
         BE    *+14                                                             
         CLC   PBCKLANG,FLTLNG                                                  
         BNE   GETNEXT                                                          
*        CLI   FLTGRB,0            TEST GROUPING BASIS FILTER                   
*        BE    *+14                                                             
*        CLC   PBCKGRPB,FLTGRB                                                  
*        BNE   GETNEXT                                                          
         MVC   TLCDIR,PBCKEY                                                    
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECTION                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         LA    R2,TLCDIR                                                        
         USING PBCRECD,R2                                                       
         B     *(R1)                                                            
         B     VALCON              SELECT CONTROL                               
         B     VALDEL              DELETE                                       
         B     VALRES              RESTORE                                      
*                                                                               
VALCON   TM    PBCKSTAT,PBCSDELT                                                
         BO    EXITN                                                            
         B     EXITY                                                            
*                                                                               
VALDEL   TM    PBCKSTAT,PBCSDELT                                                
         BO    EXITN                                                            
         OI    PBCKSTAT,PBCSDELT   CHANGE TSAR RECORD                           
         L     R2,AIO1                                                          
         OI    PBCRSTAT,PBCSDELT   CHANGE FILE RECORD                           
         OI    LSINDS1,LSIUPREC                                                 
         B     EXITY                                                            
*                                                                               
VALRES   TM    PBCKSTAT,PBCSDELT                                                
         BZ    EXITN                                                            
         NI    PBCKSTAT,FF-PBCSDELT   CHANGE TSAR RECORD                        
         L     R2,AIO1                                                          
         NI    PBCRSTAT,FF-PBCSDELT   CHANGE FILE RECORD                        
         OI    LSINDS1,LSIUPREC                                                 
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
DEFCLM   DS    0XL1                ** DEFAULT COLUMN LIST **                    
         DC    AL1(LFT#GRB)                                                     
         DC    AL1(LFT#NAM)                                                     
         DC    AL1(LFT#01)                                                      
         DC    AL1(LFT#02)                                                      
         DC    AL1(LFT#03)                                                      
         DC    AL1(LFT#04)                                                      
         DC    AL1(LFT#05)                                                      
         DC    AL1(LFT#06)                                                      
         DC    AL1(LFT#07)                                                      
         DC    AL1(LFT#23)                                                      
         DC    AL1(LFT#08)                                                      
         DC    AL1(LFT#09)                                                      
         DC    AL1(LFT#10)                                                      
         DC    AL1(LFT#11)                                                      
         DC    AL1(LFT#12)                                                      
         DC    AL1(LFT#18)                                                      
         DC    AL1(LFT#19)                                                      
         DC    AL1(LFT#22)                                                      
         DC    AL1(LFT#28)                                                      
         DC    AL1(LFT#13)                                                      
         DC    AL1(LFT#24)                                                      
         DC    AL1(LFT#25)                                                      
         DC    AL1(LFT#26)                                                      
         DC    AL1(LFT#27)                                                      
         DC    AL1(LFT#14)                                                      
         DC    AL1(LFT#15)                                                      
         DC    AL1(LFT#16)                                                      
         DC    AL1(LFT#17)                                                      
         DC    AL1(LFT#20)                                                      
         DC    AL1(LFT#21)                                                      
DEFCLML  EQU   *-DEFCLM                                                         
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
FWORKD   DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    XL64                                                             
FFMTBLK  DS    XL(L'FBLK)                                                       
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLB0D    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,L'LSDISLST,L'LSDISLST)                           
         DC    AL1(0)                                                           
         DC    AL2(OPTDISQ,LSDIS-LSVALSD)                                       
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R8,RB                                                            
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF2BD                                                      
         SPACE 1                                                                
         ORG   OSVALS                                                           
FLTS     DS    0XL2                * FILTERS *                                  
FLTFMT   DS    XL1                 FORMAT NUMBER                                
FLTLNG   DS    XL1                 LANGUAGE                                     
SVFLTS   DS    XL(L'FLTS)          SAVED FILTERS                                
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'147ACCLB0DB  12/22/99'                                      
         END                                                                    
