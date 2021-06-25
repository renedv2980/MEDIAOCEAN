*          DATA SET NESFM19    AT LEVEL 114 AS OF 09/29/10                      
*PHASE T31C19A,+0                                                               
         TITLE 'NESFM19 - NETFILE MAINT - NTWK UNIVERSE REC NAD DEMOS'          
         PRINT NOGEN                                                            
T31C19   CSECT                                                                  
         NMOD1 0,T31C19                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASUBSYSD                                                      
         USING SYSD,R9                                                          
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T31C19,RB,R7                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVC   AIO,AIO1                                                         
         MVI   GOAGAIN,C'N'        RESET SWAP SWITCH                            
         SPACE 3                                                                
         OI    CONRECH+6,X'01'     REVALIDATE EVERY TIME (PF KEYS)              
         CLI   ACTNUM,ACTADD       ADD ACTION NOT ALLOWED                       
         BE    INVADD                                                           
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   VKCHK                                                            
         MVC   SVDMWORK,DMWORK                                                  
         BAS   RE,PROCPF                                                        
         BAS   RE,VR                                                            
         MVC   DMWORK(96),SVDMWORK                                              
         GOTO1 VSETSPT                                                          
         MVC   KEY,SVKEY                                                        
         B     EXIT1                                                            
VKCHK    CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   DKCHK                                                            
         BAS   RE,PROCPF                                                        
         BAS   RE,VK                                                            
         B     RESET                                                            
DKCHK    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   DRCHK                                                            
         BAS   RE,DK                                                            
         B     RESET                                                            
DRCHK    CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   LRCHK                                                            
         BAS   RE,PROCPF                                                        
         BAS   RE,DR                                                            
         B     RESET                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PRCHK                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
PRCHK    CLI   MODE,PRINTREP       PRINT RECORDS                                
         BNE   PRPFK                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
PRPFK    CLI   MODE,PROCPFK        PRINT RECORDS                                
         BNE   EXIT1                                                            
         BAS   RE,PF                                                            
*                                                                               
RESET    MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
*                                                                               
EXIT     GOTO1 VSETSPT                                                          
         PRINT GEN                                                              
EXIT1    XIT1                                                                   
         PRINT NOGEN                                                            
         EJECT                                                                  
VK       NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NUNRECD,R6                                                       
         MVC   NUNKAGY,AGENCY                                                   
         MVC   NUNKTYP,=2X'0D22'                                                
         LA    R2,NDUCODH                                                       
         GOTO1 VALIFLD,DMCB                                                     
         BZ    NOINPUT                                                          
         LTR   R0,R0               IS INPUT NUMERIC                             
         BNZ   VKCODE              YES/CODE                                     
*                                                                               
         CLC   NFLD(4),=C'DDS='    IS IT DDS DISPLAY                            
         BNE   VKDATE                                                           
         CLI   ACTNUM,ACTDIS       DDS UNIV ONLY ALLOWS DISPLAY                 
         BE    *+12                                                             
         MVI   ERROR,INVACT                                                     
         B     LFMERR                                                           
         XC    NUNKAGY,NUNKAGY     AGY=00 FOR DDS                               
         OI    WHENOK,X'01'        DDS UNIV DOES OWN IO                         
         MVI   QDPT,X'FF'                                                       
         MVC   NFLD(8),NFLD+4                                                   
         SH    R1,=H'4'                                                         
*                                                                               
VKDATE   C     R1,=F'5'                                                         
         BL    VKCODE                                                           
         GOTO1 DATVAL,DMCB,(0,NFLD),WORK                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VKINVAL                                                          
         GOTO1 DATCON,DMCB,WORK,(2,NUNKEND)                                     
         MVI   NUNKTYPE,0                     0=END DATE                        
         CLI   ACTNUM,ACTADD       IS IT ADD                                    
         BE    VKEXIT                                                           
         CLI   QDPT,X'FF'          IS IT DDS UNIVS                              
         BE    VKEXIT                                                           
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                NO/SO CHECK IF END DATE MATCHES              
         CLC   KEY(13),KEYSAVE        A RECORD                                  
         BNE   ENDATERR                                                         
         B     VKEXIT                                                           
VKCODE   C     R1,=F'4'                                                         
         BH    VKINVAL                                                          
         MVC   WORK(4),NFLD                                                     
         XC    NFLD(5),NFLD                                                     
         LA    R0,4                                                             
         SR    R0,R1                                                            
         LA    R3,NFLD                                                          
         AR    R3,R0                                                            
         MVC   0(4,R3),WORK                                                     
         PACK  WORK(3),NFLD(5)                                                  
         MVC   NUNKCODE,WORK                                                    
         MVI   NUNKTYPE,1                     1=UNIV CODE                       
VKEXIT   DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         GOTO1 VSETSPT                                                          
         B     EXIT                                                             
*                                                                               
NOINPUT  MVI   ERROR,MISSING                                                    
         B     LFMERR                                                           
VKINVAL  MVI   ERROR,INVALID                                                    
         B     LFMERR                                                           
ENDATERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** END DATE ERROR - NEXT DATE'                   
         GOTO1 DATCON,DMCB,(2,NUNKEND),(5,CONHEAD+31)                           
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
NODATA   DS    0H                                                               
         LA    R2,NDUNCDH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'*** INVALID OR MISSING NAD CODE ***'              
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
INVADD   DS    0H                                                               
         LA    R2,NDUNCDH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** ADD ACTION NOT ALLOWED ***'                   
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
DK       NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         L     R3,AIO                                                           
         USING NUNRECD,R3                                                       
         LA    R2,NDUCODH                                                       
         XC    NDUCOD,NDUCOD                                                    
         CLC   NUNKAGY,=2X'00'     IS IT DDS                                    
         BNE   *+14                                                             
         MVC   8(4,R2),=C'DDS='                                                 
         LA    R2,4(R2)                                                         
         CLI   NUNKTYPE,0          IS IT ENDDATE                                
         BE    ENDAT                                                            
         UNPK  WORK(5),NUNKCODE(3)                                              
         MVC   8(4,R2),WORK                                                     
         B     DKXIT                                                            
ENDAT    GOTO1 DATCON,DMCB,(2,NUNKEND),(5,8(R2))                                
*                                                                               
DKXIT    GOTO1 VSETSPT                                                          
         FOUT  NDUCODH                                                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
LR       NTR1                                                                   
*                                                                               
LR02     MVI   NLISTS,X'0F'        SET NUM OF LIST LINES                        
         LA    R3,KEY                                                           
         USING NUNRECD,R3                                                       
         SPACE                                                                  
         OC    KEY(20),KEY                                                      
         BNZ   *+10                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
         SPACE                                                                  
LR22     DS    0H                                                               
         CLC   KEY(10),SVKEY               ID/AM                                
         BNE   LRX                                                              
LR22B    DS    0H                                                               
         GOTO1 GETREC                                                           
         DROP  R3                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         L     R6,AIO                                                           
         USING NUNRECD,R6                                                       
         CLI   NUNKTYPE,0          IS IT ENDDATE                                
         BE    LRDAT                                                            
         UNPK  WORK(5),NUNKCODE(3)                                              
         MVC   LRCODE,WORK                                                      
         B     LR30                                                             
LRDAT    GOTO1 DATCON,DMCB,(2,NUNKEND),(5,LRDATE)                               
*                                                                               
*                                                                               
LR30     XC    LRDESC,LRDESC                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR100                                                            
*                                                                               
         LA    RE,39                                                            
         CLI   1(R6),42                                                         
         BH    LR50                                                             
*                                                                               
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         BM    LR100                                                            
LR50     EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LRDESC(0),2(R6)                                                  
*                                                                               
LR100    GOTO1 LISTMON                                                          
         B     LR20                GOTO READ SEQ                                
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
*        DROP  R3,R5,R6                                                         
         EJECT                                                                  
PR       DS    0H                                                               
*                                                                               
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,P+10                                                          
         USING PLINED,R4                                                        
         L     R6,AIO                                                           
         USING NUNRECD,R6                                                       
*                                                                               
         CLI   NUNKTYPE,0          IS IT ENDDATE                                
         BE    PRDAT                                                            
         UNPK  WORK(5),NUNKCODE(3)                                              
         MVC   PRCODE,WORK                                                      
         B     PR30                                                             
PRDAT    GOTO1 DATCON,DMCB,(2,NUNKEND),(5,PRDATE)                               
*                                                                               
*                                                                               
PR30     XC    PRDESC,PRDESC                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR100                                                            
*                                                                               
         LA    RE,39                                                            
         CLI   1(R6),42                                                         
         BH    PR50                                                             
*                                                                               
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         BM    PR100                                                            
PR50     EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRDESC(0),2(R6)                                                  
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
*                                                                               
PREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
DR       NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   SVLIST,LISTDIR                                                   
*--CLEAR SCREEN                                                                 
         LA    R2,NDUFSTH                                                       
         LA    R6,42                                                            
DR20     MVI   5(R2),0                                                          
         FOUT  (R2),SPACES,12                                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         FOUT  (R2),SPACES,1                                                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         FOUT  (R2),SPACES,6                                                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R6,DR20                                                          
         XC    KEY,KEY                                                          
*--CHECK NAD CODE READ NAD RECORD                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         MVC   WORK+20(6),2(R6)                                                 
         B     DR30                                                             
         CLI   NDUNCDH+5,0                                                      
         BE    NODATA                                                           
         MVC   WORK+20(6),NDUNCD                                                
         OC    WORK+20(6),SPACES                                                
*-GET AGENCY/MEDIA CODE                                                         
DR30     LA    R2,WORK                                                          
         XC    0(8,R2),0(R2)                                                    
         MVI   8(R2),C'N'                                                       
         MVC   AIO,AIO3                                                         
         MVI   BYTE,C'A'           USER SET IO AREA                             
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO3                                                         
*                                                                               
         MVC   KEY(2),=XL2'0D16'                                                
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(6),WORK+20                                                 
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NODATA                                                           
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
*--MOVE NAD CODE TO SCREEN                                                      
         MVC   NDUNCD(6),KEYSAVE+3                                              
         OI    NDUNCDH+4,X'20'     SET PREV VAL BIT                             
         FOUT  NDUNCDH                                                          
*                                                                               
DR100    DS    0H                                                               
*                                                                               
         XC    DBLOCK(256),DBLOCK                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'NAD'                                                 
         MVI   DBSELMED,C'N'                                                    
         MVC   DBSELAGY,AGENCY                                                  
         LA    R2,NDUFSTH          FIRST DISPLAY FIELD                          
         LA    R5,42               NUMBER OF ENTRIES                            
         L     R6,AIO              UNIVERSE RECORD                              
         L     R4,AIO3             NAD DEMO DESCRIPTION RECORD                  
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR300                                                            
*        BAS   RE,FLTOVEL          FILTER THE OVERRIDE ELEMENTS                 
*        BNE   DR200                                                            
DR150    MVC   WORK(6),0(R6)                                                    
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,WORK+3),('DEMOCON_16',DUB),(R5),DUB+3           
*********                                                                       
         MVC   BYTE,DUB+3                                                       
         MVI   DUB+1,C'I'          DISPLAY AS AN IMPRESSION                     
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',DUB),(R5),DUB+3              
**********                                                                      
         GOTO1 VDEMCON1,DMCB,(1,DUB),(9,8(R2)),(0,DBLOCK)                       
         MVC   5(1,R2),DMCB        LENGTH                                       
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         MVC   DUB+3(1),BYTE                                                    
         MVI   DUB+1,C'V'          CHECK VPH                                    
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',WORK+3),(R5),DUB+3           
**********                                                                      
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(ELCODE,0(R4)),(4,WORK+2)          
         CLI   DMCB+12,0                                                        
         BE    DR180                                                            
*                                                                               
         L     R4,AIO3                                                          
         MVI   DUB+1,C'I'         CHECK IMPRESSION                              
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',WORK+3),(R5),DUB+3           
**********                                                                      
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(ELCODE,0(R4)),(4,WORK+2)          
         CLI   DMCB+12,0                                                        
         BE    DR180                                                            
*                                                                               
         L     R4,AIO3                                                          
         MVI   DUB+1,C'T'         CHECK IMPRESSION                              
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',WORK+3),(R5),DUB+3           
**********                                                                      
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(ELCODE,0(R4)),(4,WORK+2)          
         CLI   DMCB+12,0                                                        
         BE    DR180                                                            
         MVI   8(R2),C'-'   A C'-' INDICATES ELEMENT ON UNIVERSE                
*                           RECORD BUT NOT ON NAD DEMO RECORD, ELEMENT          
*                           WILL BE REMOVED ON NEXT CHANGE ACTION.              
         CLI   DMCB+12,X'06'                                                    
         BE    DR180                                                            
         DC    H'0'                                                             
DR180    FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         L     R0,8(R6)                                                         
         CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         EDIT  (P6,DUB),(6,8(R2)),0,ALIGN=LEFT                                  
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
DR200    BAS   RE,NEXTEL                                                        
         BNE   DR300                                                            
*        BAS   RE,FLTOVEL          FILTER THE OVERRIDE ELEMENTS                 
*        BNE   DR200                                                            
         L     R4,AIO3                                                          
         BCT   R5,DR150                                                         
*                                                                               
DR300    L     R6,AIO3             NAD DEMO DESCRIPTION RECORD                  
         L     R4,AIO              UNIVERSE RECORD                              
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR500                                                            
         BAS   RE,FLTOVEL          FILTER THE OVERRIDE ELEMENTS                 
         BNE   DR400                                                            
DR350    MVC   WORK(6),0(R6)                                                    
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,WORK+3),('DEMOCON_16',DUB),(R5),DUB+3           
*********                                                                       
         MVI   DUB+1,C'U'          SET UP FOR UNIVERSE LOOKUP                   
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',WORK+3),(R5),DUB+3           
**********                                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(ELCODE,0(R4)),(4,WORK+2)          
         CLI   DMCB+12,0                                                        
         BE    DR420                                                            
         GOTO1 VDEMCON1,DMCB,(1,WORK+3),(9,8(R2)),(0,DBLOCK)                    
         MVC   5(1,R2),DMCB        LENGTH                                       
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVI   8(R2),C'+'   A C'+' INDICATES ELEMENT ON NAD DEMO                
*                           RECORD BUT NOT ON UNIVERSE RECORD, ELEMENT          
*                           WILL BE ADDED ON NEXT CHANGE ACTION.                
         CLI   DMCB+12,X'06'                                                    
         BE    DR380                                                            
         DC    H'0'                                                             
DR380    FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
DR400    BAS   RE,NEXTEL                                                        
         BNE   DR500                                                            
         BAS   RE,FLTOVEL          FILTER THE OVERRIDE ELEMENTS                 
         BNE   DR400                                                            
         L     R4,AIO                                                           
         BCT   R5,DR350                                                         
*                                                                               
DR420    BAS   RE,NEXTEL                                                        
         BNE   DR500                                                            
         L     R4,AIO                                                           
         B     DR350                                                            
*                                                                               
DR500    B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*--FILTER THE "DD" ELEMENTS FOR NAD VPH DEMOS ONLY                              
*-- INPUT R6=(ADDRESS OF ELEMENT)                                               
*                                                                               
FLTOVEL  ST    RE,DUB+4                                                         
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,3(R6)),('DEMOCON_16',DUB),(R5),DUB+3            
*********                                                                       
* CHECK FOR PERSONAL LANGUAGE DEMO                                              
         CLI   DUB+3,X'40'                                                      
         BNH   FLTOV100                                                         
         CLC   BYTE,BYTE                                                        
         B     FLTOVEX                                                          
*                                                                               
FLTOV100 CLI   3(R6),1                                                          
         BNH   FLTOV200                                                         
*                                                                               
         CLI   3(R6),255                                                        
         BH    FLTOV200                                                         
*                                                                               
         CLI   DUB+1,C'I'                                                       
         BE    FLTOVEX                                                          
*                                                                               
         CLI   DUB+1,C'T'                                                       
         BE    FLTOVEX                                                          
*                                                                               
         CLI   DUB+1,C'V'                                                       
         BE    FLTOVEX                                                          
*                                                                               
FLTOV200 CLI   4(R6),0             SET NOT EQUAL CONDITION                      
FLTOVEX  L     RE,DUB+4                                                         
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
VR       NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         MVC   2(6,R6),NDUNCD                                                   
         B     VR50                                                             
*--CREATE 03 ELEMENT                                                            
         L     R6,AIO                                                           
         XC    ELEM(20),ELEM                                                    
         MVC   ELEM(2),=XL2'0314'                                               
         MVC   ELEM+2(6),NDUNCD                                                 
         GOTO1 ADDELEM                                                          
*                                                                               
VR50     L     R6,AIO                                                           
         MVI   ELCODE,X'DD'        DELETE DEMO ELEMNTS                          
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R6)),0                   
         TM    NDUNCDH+4,X'20'     WAS NAD CODE CHANGED                         
         BO    VR100               NO                                           
         B     DR                                                               
*                                                                               
VR100    DS    0H                                                               
         LA    R2,NDUFSTH                                                       
         LA    R4,42                                                            
         XC    ELEM(12),ELEM                                                    
         MVC   ELEM(2),=XL2'DD0C'                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'NAD'                                                 
         MVI   DBSELMED,C'N'                                                    
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDEMVAL1,CDEMOVAL                                                
         DROP  1                                                                
*                                                                               
VR200    CLI   5(R2),0             CHECK END OF LIST                            
         BE    VR500                                                            
*                                                                               
         LR    R3,R2                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         CLI   8(R3),C'-'          IF INDICATOR EQUALS C'-'                     
         BNE   VR250               DONT WRITE TO RECORD                         
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         LR    R2,R3               REPOSITION R2                                
         B     VR300                                                            
*--MOVE IN THE DEMO                                                             
VR250    MVC   WORK,0(R2)                                                       
         MVI   WORK+1,0            REMOVE PROTECT BIT                           
         GOTO1 VDEMVAL1,DMCB,(1,WORK),(1,WORK+30),(0,DBLOCK)                    
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,WORK+30),('DEMOCON_16',DUB),(R5),DUB+3          
*********                                                                       
         MVI   DUB+1,C'U'                                                       
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',ELEM+3),(R5),DUB+3           
**********                                                                      
*                                                                               
         ZIC   RF,0(3)                                                          
         AR    R3,RF                                                            
         LR    R2,R3                                                            
*--MOVE IN PRECISSION AND NAD DEMO FLAG                                         
         MVI   ELEM+6,X'80'        NAD DEMO FLAG                                
         MVI   ELEM+7,X'42'        PRECISSION FACTOR                            
*--MOVE IN THE AMOUNT                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BNH   EDTERR              CAN'T BE NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'1000'                                                     
         CP    DUB+5(3),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   EDTERR              NO DECIMAL OR ONES                           
         CVD   R0,DUB              I.E. NEAREST 10,000                          
         DP    DUB,=P'10'                                                       
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,ELEM+8                                                     
         GOTO1 ADDELEM                                                          
*                                                                               
VR300    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R4,VR200                                                         
*                                                                               
VR500    BAS   RE,ACTIVITY                                                      
         BAS   RE,DR                                                            
         B     EXIT                                                             
         EJECT                                                                  
*--PFKEY MODE                                                                   
PF       NTR1                                                                   
         CLI   PFAID,0                                                          
         BE    EXIT                                                             
         CLI   PFAID,12                                                         
         BNE   PF50                                                             
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   LISTDIR,SVLIST                                                   
         B     EXIT                                                             
PF50     OI    GENSTAT2,X'08'      SAVE THE SCREEN                              
         BAS   RE,PROCPF                                                        
         B     EXIT                                                             
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM            DELETE OLD DESC                               
         XC    ELEM(10),ELEM                                                    
         MVC   ELEM(2),=X'0108'                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,ELEM+2)                                     
         MVC   ELEM+5,CONACT       SAVE ACTION                                  
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,NDUCODH                                                       
         SPACE 1                                                                
PROCPFA  CLI   PFAID,PF3           PF3 FOR UNIVERSE ACTION CHANGE               
         BE    PROCPF4                                                          
         CLI   PFAID,PF2           PF2 FOR UNIVERSE ACTION DISPLAY              
         BE    PROCPF1                                                          
         CLI   PFAID,PF8           PF9 FOR NAD DEF ACTION DISPLAY               
         BE    PROCPF6                                                          
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF1  LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFAID,0                                                          
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   PROCPF3                                                          
         GOTO1 VCALL,WORK,=C'UNIVERSE',,(12,NDUCOD),0                           
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF3  GOTO1 VTRANSF,WORK,=C'UNIVERSE',,(12,NDUCOD),0                         
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF4  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
*                                                                               
PROCPF6  LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
         MVI   PFAID,0                                                          
         GOTO1 VCALL,WORK,=C'DEMDEF  ',,(6,NDUNCD),0                            
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPFX  B     EXIT                                                             
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,41,C'NETWORK UNIVERSE RECORDS'                                
         SSPEC H2,41,C'------------------------'                                
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
*        L     R6,ASPOOLD                                                       
*        USING SPOOLD,R6                                                        
*                                                                               
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRCODE(9),=C'CODE/DATE'                                          
         MVC   PRCODE+132(13),=13C'-'                                           
         MVC   PRDESC(7),=C'COMMENT'                                            
         MVC   PRDESC+132(40),=40C'-'                                           
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     LFMERR                                                           
*                                                                               
EDTERR   MVI   ERROR,INVALID                                                    
*                                                                               
LFMERR   GOTO1 ERREX                                                            
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
MAXUNIS  EQU   49                                                               
MAXVALS  EQU   36                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
LLINED   DSECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         DS    CL5                                                              
LRCODE   DS    CL4                                                              
         DS    CL1                                                              
LRDATE   DS    CL8                                                              
         DS    CL2                                                              
LRDESC   DS    CL40                                                             
*                                                                               
PLINED   DSECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         DS    CL5                                                              
PRCODE   DS    CL4                                                              
         DS    CL1                                                              
PRDATE   DS    CL8                                                              
         DS    CL2                                                              
PRDESC   DS    CL40                                                             
         EJECT                                                                  
       ++INCLUDE DEDEMEQUS2                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE     T31C19 WORK AREA                                    
SVDMWORK DS    CL96                DEMWORK SAVE AREA                            
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONHEAD-64                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*DSECT TO COVER SAVED STORAGE IN TWA0 FOR NESFM00                               
T31CFFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEADH+3520)-(T31CFFD+3072))                                 
         ORG   CONHEADH+3520-SAVAREAL                                           
SAVAREA  DS    0C                                                               
CALLSP   DS    X                   CALL ROUTINE STACK POINTER                   
CALLSTK  DS    XL9                 STACK (LIST OF OVERLAYS)                     
LASTOV   DS    X                   LAST OVERLAY                                 
SVLIST   DS    XL188               LISTDIR SAVE AREA                            
         ORG CONTAGH                                                            
       ++INCLUDE NESFME8D                                                       
         ORG CONTAGH                                                            
       ++INCLUDE NESFME9D                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENUNIV                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEGETNUND                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114NESFM19   09/29/10'                                      
         END                                                                    
