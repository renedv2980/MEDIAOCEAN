*          DATA SET NEWRI51    AT LEVEL 068 AS OF 07/02/08                      
*PHASE T32051A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32051 - WINDOW-MIRROR UPDATE'                                  
T32051   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MIRO**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          ANETWS1=WORKING STORAGE                      
         USING WORKD,R7                                                         
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
EDITM1   MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH               CLIENT                                  
         NETGO NVCLI,DMCB,                                                      
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVPRDALL,DMCB                                                    
*                                                                               
         LA    R2,SPLESTH               ESTIMATE                                
         NETGO NVESTALL,DMCB                                                    
*                                                                               
         LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,1               OPTIONAL                                
*                                                                               
         LA    R2,SPLDPTH               DAYPART                                 
         NETGO NVDPT,DMCB                                                       
*                                                                               
         LA    R2,SPLPAKH               PACKAGE                                 
         NETGO NVPAKLOK,DMCB                                                    
*                                                                               
         LA    R2,SPLRSTRH              START DATE                              
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLRENDH              END DATE                                
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
*                                                                               
         LA    R2,SPLPRGH               PROGRAM FILTER                          
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         MVI   TESTRUN,C'N'                                                     
         LA    R2,SPLTESTH              TEST RUN                                
         GOTO1 SCANNER,DMCB,(0,(R2)),SCNBLOCK,0                                 
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    EDINV                                                            
         LA    R3,SCNBLOCK                                                      
         LA    R5,REQRUN                                                        
EDT10    CLI   12(R3),C'Y'         TEST RUN                                     
         BNE   EDT12                                                            
         MVI   TESTRUN,C'Y'                                                     
         MVI   0(R5),C'Y'                                                       
         B     EDT20                                                            
EDT12    CLI   12(R3),C'A'         ALL                                          
         BNE   EDT14                                                            
         MVI   0(R5),C'A'                                                       
         B     EDT19                                                            
EDT14    CLI   12(R3),C'W'         WINDOWS                                      
         BNE   EDT16                                                            
         MVI   0(R5),C'W'                                                       
         B     EDT19                                                            
EDT16    CLI   12(R3),C'M'         MIRROR                                       
         BNE   EDT18                                                            
         CLI   0(R3),1             LET'S NOT CONFUSE THIS                       
         BNE   EDT18                                                            
         MVI   0(R5),C'M'                                                       
         B     EDT19                                                            
EDT18    CLI   12(R3),C'N'         NAD DEFINITION CODE/NAD PROGRAM EST          
         BNE   EDT18A                                                           
         MVI   0(R5),C'N'                                                       
         B     EDT19                                                            
EDT18A   CLI   12(R3),C'D'         SUB-DAYPART                                  
         BNE   EDT18F                                                           
         MVI   0(R5),C'D'                                                       
         B     EDT19                                                            
*DT18B   CLI   12(R3),C'U'         NAD UNIVERSE VALUES                          
*        BNE   EDT18F                                                           
*        MVI   0(R5),C'U'                                                       
*        B     EDT19                                                            
EDT18F   CLC   =C'MR',12(R3)       MULTI RUN?                                   
         BNE   EDT18G                                                           
         TM    3(R3),X'80'         MUST BE NUMERIC                              
         BNO   EDINV                                                            
         CLI   11(R3),1            IF IT'S 1                                    
         BNE   *+8                                                              
         MVI   11(R3),0            MAKE IT 0                                    
         MVC   MRNUMB,11(R3)       GET BINARY VALUE                             
         MVI   MULTIRUN,C'Y'       AND SET FLAG                                 
**                                                                              
         CLI   NBSELEST,0          NEEDS SPECIFIC ESTIMATE                      
         BE    EDINV                                                            
         CLC   =C'ALL',NBSELNET    NEEDS SPECIFIC NETWORK                       
         BE    EDINV                                                            
         CLI   NBSELNET,0                                                       
         BE    EDINV                                                            
**                                                                              
         B     EDT20               MULTI RUN REQUESTED ALONE                    
*                                                                               
EDT18G   B     EDINV                                                            
*                                                                               
EDT19    LA    R3,32(R3)                                                        
         LA    R5,1(R5)                                                         
         BCT   R4,EDT10                                                         
         B     EDTX                                                             
*                                                                               
EDT20    DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    EDT19                                                            
***      CLI   T320FFD+1,C'*'     IF NOT DDS                                    
***      BNE   EDINV                                                            
***      B     EDT19                                                            
*                                                                               
EDTX     DS    0H                                                               
* - NEED TO HAVE COPIES WRITTERN TO RECOVERY FILE                               
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMST                                                           
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
ENDMST   DS    0H                                                               
         LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
REPMOD   NTR1                                                                   
         XC    COUNTER,COUNTER                                                  
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBSEQ,C'Q'          PROGRAM ORDER X'84' KEY                      
         LA    R1,MAINLINE                                                      
         ST    R1,NBHOOK                                                        
RPM10    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   RPM10                                                            
         MVC   P+1(13),=C'UNITS UPDATED'                                        
         L     R2,COUNTER                                                       
         EDIT  (R2),(10,P+15),ALIGN=LEFT                                        
         LTR   R2,R2                                                            
         BNZ   *+10                                                             
         MVC   P+15(5),=C'=NONE'                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
MAINLINE NTR1                                                                   
         MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
         CLI   NBMODE,NBPROCUN    ONLY WANT UNIT RECORDS                        
         BNE   MNX                                                              
         CLI   MULTIRUN,C'Y'       IS IT MULTIRUN UPDAT?                        
         BNE   *+12                                                             
         BAS   RE,DOMULTI                                                       
         B     MNX                                                              
*                                                                               
         LA    R2,SPLPRGH          PROGRAM FILTER                               
         CLI   5(R2),0                                                          
         BE    MN10                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),NBACTPRG                                                 
         BNE   MNX                                                              
*                                                                               
MN10     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),NBMARKET                                                
         MVC   KEY+5(6),NBACTPRG                                                
         MVC   KEY+11(2),NBACTDAT                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                          
         CLC   KEY(11),KEYSAVE                                                  
         BNE   MNX                                                              
         L     R2,ANETWS2                                                       
         CLC   KEY(13),0(R2)       DO WE ALREADY HAVE RECORD                    
         BE    MN20                                                             
         LA    R6,KEY+14                                                        
         GOTO1 NBDM,DMCB,=C'GETREC',=C'SPTFIL',(R6),(R2),MYDMWRK                
         CLI   DMCB+8,0                                                         
         BNE   MNX                                                              
         MVC   NBDTADSP,=H'24'                                                  
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    MN17                                                             
         MVI   WINDOW,0                                                         
         MVI   MIRROR,0                                                         
         XC    SUBDPT,SUBDPT                                                    
         XC    NADCODE,NADCODE                                                  
         MVC   P+1(14),=C'PROGRAM RECORD'                                       
         B     MN15                                                             
         GOTO1 =V(HEXOUT),DMCB,KEY,P+16,16       TROUBLE SHOOTING               
         GOTO1 SPOOL,DMCB,(R8)                                                  
MN15     MVC   P+17(6),KEY+5                                                    
         GOTO1 DATCON,DMCB,(2,KEY+11),(8,P+24)                                  
         MVC   P+34(25),=C'INFORMATION NOT ON RECORD'                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     MN20                                                             
         USING NPGEL03,R2                                                       
MN17     MVC   WINDOW,NPGSTATB     WINDOW STATUS BIT                            
         MVC   MIRROR,NPGMIRCD     MIRROR CODE                                  
         MVC   SUBDPT,NPGSDPT      SUBDAYPART                                   
         MVC   NADCODE,NPGNADDM    NAD DEF CODE                                 
         MVC   P+1(14),=C'PROGRAM RECORD'                                       
         B     MN18                                                             
         GOTO1 =V(HEXOUT),DMCB,KEY,P+16,16                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
MN18     MVC   P+17(6),KEY+5                                                    
         GOTO1 DATCON,DMCB,(2,KEY+11),(8,P+24)                                  
         MVC   P+34(1),WINDOW                                                   
         MVC   P+36(1),MIRROR                                                   
         MVC   P+38(3),SUBDPT                                                   
         MVC   P+42(6),NADCODE                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
MN20     DS    0H                                                               
         LA    R5,REQRUN                                                        
*                                                                               
MN21     CLI   0(R5),C'A'        ALL                                            
         BNE   *+24                                                             
         BAS   R4,RWINDOW                                                       
         BAS   R4,RMIRROR                                                       
         BAS   R4,RDAYPT                                                        
         BAS   R4,RNAD                                                          
*        BAS   R4,RUNIV                                                         
         B     MN50                                                             
         CLI   0(R5),C'W'          WINDOW                                       
         BNE   *+12                                                             
         BAS   R4,RWINDOW                                                       
         B     MN22                                                             
         CLI   0(R5),C'M'          MIRROR                                       
         BNE   *+12                                                             
         BAS   R4,RMIRROR                                                       
         B     MN22                                                             
         CLI   0(R5),C'D'          DAYPART                                      
         BNE   *+12                                                             
         BAS   R4,RDAYPT                                                        
         B     MN22                                                             
*        CLI   0(R5),C'U'          NAD UNIVERSE VALUES                          
*        BNE   *+12                                                             
*        BAS   R4,RUNIV                                                         
*        B     MN22                                                             
         CLI   0(R5),C'N'          NAD DEF CODE                                 
         BNE   MN22                                                             
         BAS   R4,RNAD                                                          
         B     MN22                                                             
*                                                                               
MN22     LA    R5,1(R5)                                                         
         CLI   0(R5),X'40'                                                      
         BNH   MN50                                                             
         B     MN21                                                             
         EJECT                                                                  
*                                                                               
RMIRROR  BAS   RE,GET02                                                         
         USING NUSDRD,R2                                                        
         MVC   NUMIRTYP,MIRROR     SET MIRROR CODE                              
         MVC   P+34(1),NUMIRTYP                                                 
         BR    R4                                                               
*                                                                               
RWINDOW  TM    WINDOW,X'80'        IS WINDOW STATUS ON                          
         USING NUSDRD,R2                                                        
         BNO   RW10                                                             
         BAS   RE,GET02                                                         
         OI    NUSDST3,X'08'                                                    
         MVC   P+36(1),NUSDST3                                                  
         OI    P+36,X'F0'                                                       
RW10     DS    0H                                                               
         BR    R4                                                               
*                                                                               
RDAYPT   BAS   RE,GET02                                                         
         USING NUSDRD,R2                                                        
         MVC   NUSDPT,SUBDPT       SET SUB DAYPART                              
         MVC   P+38(3),NUSDPT                                                   
         BR    R4                                                               
*                                                                               
RNAD     BAS   RE,GET62                                                         
         USING  NUNADEL,R2                                                      
         MVC   NUNADCD,NADCODE      SET NADCODE                                 
         MVC   P+42(6),NUNADCD                                                  
*                                                                               
*  UPDATE THE UNITS ESTIMATED NAD DEMOS FROM THE PROGRAM RECORD                 
*                                                                               
*  READ ALL DEMO ELEMENTS DELETE ALL THOSE SET FOR NAD.                         
*                                                                               
         USING NUOVD,R3                                                         
         L     R2,NBAIO                                                         
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'DD',(R2)),0                         
         CLI   12(R1),0                                                         
         BNE   RN100                                                            
         L     R3,12(R1)                                                        
         B     RN20                                                             
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
RN10     ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'DD'                                                      
         BNE   RN100                                                            
*                                                                               
RN20     TM    NUOVFLG,X'80'        IS ELEMENT A NAD DEMO                       
         BZ    RN10                                                             
         CLI   NUOVMOD,C'U'         IS IT UNIVERSE ELEMENT                      
         BE    RN10                                                             
         MVI   NUOVZER,X'FF'        SET FOR DELETE                              
         B     RN10                                                             
*                                                                               
RN100    GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'DD',(R2)),(1,=X'FF')                
         DROP  R3                                                               
*  IF NEEDED READ THE ESTIMATE RECORD AND SAVE THE DEMO'S                       
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),=CL3'POL'                                               
         MVC   KEY+7(1),NUKEST                                                  
         CLC   KEY(13),ESTKEY                                                   
         BE    RN200                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 ESTIMAT RECORD MUST EXIST                   
         L     R2,ANETWS3                                                       
         USING ESTHDR,R2                                                        
         LA    R6,KEY+14                                                        
         GOTO1 NBDM,DMCB,=C'GETREC',=C'SPTFIL',(R6),(R2),MYDMWRK                
         MVC   ESTDEMS,EDEMLST                                                  
         DROP  RE,R2                                                            
*                                                                               
*  MOVE ALL THE NAD DEMO INFO THAT IS ON THE ESTIMATE                           
*  FROM THE PROGRAM RECORD TO THE UNIT RECORD.                                  
*                                                                               
RN200    L     R2,ANETWS2           PROGRAM RECORD                              
         L     R3,NBAIO             BUY RECORD                                  
******   GOTO1 =V(PRNTBL),DMCB,=C'BEFR',0(R3),C'DUMP',1500,=C'1D'               
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'DD',(R2)),0                         
         CLI   12(R1),0                                                         
         BNE   RNEX                                                             
         L     R6,12(R1)                                                        
         B     RN220                                                            
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
RN210    ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'DD'                                                      
         BNE   RNEX                                                             
*                                                                               
*  CHECK TO SEE IF DEMO CATEGORY IS ON THE ESTIMATE                             
*                                                                               
RN220    LA    RE,ESTDEMS                                                       
         LA    RF,20                                                            
*                                                                               
RN240    CLC   0(1,RE),3(R6)        CHECK NAD CATEGORY                          
         BNE   RN260                                                            
         CLC   2(1,RE),5(R6)        CHECK DEMO CODE                             
         BE    RN300                                                            
RN260    LA    RE,3(RE)                                                         
         BCT   RF,RN240                                                         
         B     RN210                                                            
*                                                                               
*  WRITE THE ELEMENT OUT                                                        
*                                                                               
RN300    GOTO1 HELLO,DMCB,(C'P',UNTFILE),(X'DD',(R3)),(R6),0                    
         B     RN210                                                            
*                                                                               
******   GOTO1 =V(PRNTBL),DMCB,=C'AFTR',0(R3),C'DUMP',1500,=C'1D'               
RNEX     BR    R4                                                               
         SPACE 3                                                                
*                                                                               
*  UPDATE THE UNITS ESTIMATED NAD DEMOS FROM THE UNIVERSE RECORD                
*                                                                               
*  READ ALL DEMO ELEMENTS DELETE ALL THOSE SET FOR NAD.                         
*                                                                               
RUNIV    L     R2,NBAIO                                                         
         B     UNEX                                                             
         USING NUOVD,R3                                                         
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'DD',(R2)),0                         
         CLI   12(R1),0                                                         
         BNE   UN100                                                            
         L     R3,12(R1)                                                        
         B     UN20                                                             
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
UN10     ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'DD'                                                      
         BNE   UN100                                                            
*                                                                               
UN20     TM    NUOVFLG,X'80'        IS ELEMENT A NAD DEMO                       
         BZ    UN10                                                             
         CLI   NUOVMOD,C'U'         IS IT UNIVERSE ELEMENT                      
         BNE   UN10                                                             
         MVI   NUOVZER,X'FF'        SET FOR DELETE                              
         B     UN10                                                             
*                                                                               
UN100    GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'DD',(R2)),(1,=X'FF')                
         DROP  R3                                                               
*  IF NEEDED READ THE ESTIMATE RECORD AND SAVE THE DEMO'S                       
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),=CL3'POL'                                               
         MVC   KEY+7(1),NUKEST                                                  
         CLC   KEY(13),ESTKEY                                                   
         BE    UN200                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 ESTIMAT RECORD MUST EXIST                   
         L     R2,ANETWS3                                                       
         USING ESTHDR,R2                                                        
         LA    R6,KEY+14                                                        
         GOTO1 NBDM,DMCB,=C'GETREC',=C'SPTFIL',(R6),(R2),MYDMWRK                
         MVC   ESTDEMS,EDEMLST                                                  
         DROP  RE,R2                                                            
*                                                                               
*  READ THE UNIVERSE RECORD FROM GETNUN                                         
*                                                                               
UN200    LA    RE,SCNBLOCK         NOW GET UNIV REC                             
         USING GUVD,RE             AND SAVE UNIV ELEM IN NEWUNIVS               
         LA    RF,NBAIO            NOW GET UNIV REC                             
         USING NURECD,RF                                                        
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,NUALPHA                                                   
         MVC   GUVCODE,NUUNCODE                                                 
         OC    GUVCODE,GUVCODE     TEST FOR UNIVERSE CODE                       
         BNZ   *+10                HAVE ONE                                     
         MVC   GUVDATE,NUKDATE     ELSE USE AIR DATE                            
         MVC   GUVAREC,ANETWS4     SET AND CLEAR AREA FOR UNIV. RECORD          
         L     R1,ANETWS4                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   GUVCMFCS,ACOMFACS                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A32'                                        
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,SCNBLOCK                                               
         CLI   GUVERROR,0                                                       
         BNE   UNEX                                                             
         DROP  RE,RF                                                            
*                                                                               
*  MOVE ALL THE NAD DEMO INFO THAT IS ON THE ESTIMATE                           
*  FROM THE PROGRAM RECORD TO THE UNIT RECORD.                                  
*                                                                               
UN300    L     R2,ANETWS4           UNIVERSE RECORD                             
         L     R3,NBAIO             BUY RECORD                                  
         GOTO1 =V(PRNTBL),DMCB,=C'BEFR',0(R3),C'DUMP',1500,=C'1D'               
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'DD',(R2)),0                         
         CLI   12(R1),0                                                         
         BNE   UNEX                                                             
         L     R6,12(R1)                                                        
         B     UN320                                                            
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
UN310    ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'DD'                                                      
         BNE   UNEX                                                             
*                                                                               
*  CHECK TO SEE IF DEMO CATEGORY IS ON THE ESTIMATE                             
*                                                                               
UN320    LA    RE,ESTDEMS                                                       
         LA    RF,20                                                            
*                                                                               
UN340    CLC   0(1,RE),3(R6)        CHECK NAD CATEGORY                          
         BNE   UN360                                                            
         CLC   2(1,RE),5(R6)        CHECK DEMO CODE                             
         BE    UN400                                                            
UN360    LA    RE,3(RE)                                                         
         BCT   RF,UN340                                                         
         B     UN310                                                            
*                                                                               
*  WRITE THE ELEMENT OUT                                                        
*                                                                               
UN400    GOTO1 HELLO,DMCB,(C'P',UNTFILE),(X'DD',(R3)),(R6),0                    
         B     UN310                                                            
*                                                                               
UNEX     GOTO1 =V(PRNTBL),DMCB,=C'AFTR',0(R3),C'DUMP',1500,=C'1D'               
         BR    R4                                                               
         SPACE 3                                                                
*                                                                               
MN50     CLI   TESTRUN,C'Y'        IS IT TEST                                   
         BE    MN70                                                             
         MVI   NBUPUNIT,C'Y'       NO/SET ON WRITE SWITCHES                     
         MVI   NBNOWRIT,C'Y'                                                    
*                                                                               
MN70     L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
*                                                                               
         CLI   TESTRUN,C'Y'        ONLY PRINT RECS ON TEST                      
         BNE   MNX                                                              
         GOTO1 =V(CLUNPK),DMCB,NBACTCLI,P+1                                     
         MVC   P+5(6),NBACTPRG                                                  
         MVC   P+13(4),NBACTNET                                                 
         ZIC   R3,NBACTEST                                                      
         EDIT  (R3),(3,P+19)                                                    
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,P+24)                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
MNX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*                                                                               
GET02    NTR1                                                                   
         MVC   NBDTADSP,=H'27'     SET PROGRAM DATA INTO UNIT                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1  REGS=(R2)                                                        
*                                                                               
GET62    NTR1                                                                   
         MVC   NBDTADSP,=H'27'     GET X'62' ELEMENT                            
GET622   L     R2,NBAIO                                                         
         MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
         BE    GET62X                                                           
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'620C'     ID/LENGTH                                   
         GOTO1 ADDELEM                                                          
         B     GET622                                                           
GET62X   XIT1  REGS=(R2)                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
UNTFILE  DC    CL8'UNTFIL'                                                      
SPTFILE  DC    CL8'SPTFIL'                                                      
         EJECT                                                                  
* IF WE HAVE MULTI ELEM, CHANGE/DELETE IT                                       
* IF WE DON'T , ADD IT                                                          
DOMULTI  NTR1                                                                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         USING NUOTH,R2                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DOMUL10  BAS   RE,NEXTEL                                                        
         BNE   DOMUL20                                                          
         CLI   NUOTTYP,C'L'        IS IT MULTI?                                 
         BNE   DOMUL10             NO                                           
*                                  YES                                          
         MVC   NUOTHER,MRNUMB      SET NEW MULTIRUNNUMBER                       
         CLI   NUOTHER,0           IS IT ZERO?                                  
         BNE   DOMUL30                                                          
         L     R2,NBAIO            THEN DELETE IT                               
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'60',(R2)),(1,=C'L')                 
         B     DOMUL30                                                          
*                                  NO MULTI/ADD ONE                             
DOMUL20  CLI   MRNUMB,0            IF CHANGING MULTI TO 0                       
         BE    DOMULXX             DON'T ADD                                    
         XC    WORK,WORK                                                        
         MVI   WORK,X'60'                                                       
         MVI   WORK+1,4                                                         
         MVI   WORK+2,C'L'                                                      
         MVC   WORK+3(1),MRNUMB    SET MULTIRUN #                               
         L     R2,NBAIO                                                         
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),(R2),WORK,0                            
DOMUL30  CLI   TESTRUN,C'N'                                                     
         BNE   DOMULXX                                                          
         MVI   NBUPUNIT,C'Y'                                                    
         MVI   NBNOWRIT,C'Y'                                                    
DOMULXX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
TESTRUN  DS    CL1                                                              
WINDOW   DS    CL1                                                              
MIRROR   DS    CL1                                                              
NADCODE  DS    CL6                                                              
SUBDPT   DS    CL3                                                              
REQRUN   DS    CL10                                                             
NADELEM  DS    CL20                                                             
SCNBLOCK DS    CL400                                                            
ESTKEY   DS    CL13                                                             
ESTDEMS  DS    CL60                                                             
MRNUMB   DS    CL1                                                              
MULTIRUN DS    CL1                                                              
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGETNUND                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID7D                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068NEWRI51   07/02/08'                                      
         END                                                                    
