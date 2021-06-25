*          DATA SET RESFM32    AT LEVEL 080 AS OF 06/11/08                      
*          DATA SET RESFM32    AT LEVEL 074 AS OF 05/18/99                      
*PHASE T81832A                                                                  
         TITLE 'T81832 - RESFM32 - INVENTORY BOOK LISTS'                        
***********************************************************************         
*                                                                     *         
*  RESFM32 (T81832) --- INPUT OF INVENTORY BOOK LISTS                 *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 15DEC98 (JRD) DATE OF BIRTH (CLONED FROM SETS RESFM2C)              *         
*                                                                     *         
* 11JUN08 (KUI) 2-CHAR BOOK TYPE SUPPORT                              *         
***********************************************************************         
T81832   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81832*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
                                                                                
         OI    GENSTAT4,NODELLST   DON'T ALLOW DELETE IN LIST                   
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       REPORT?                                      
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE?                                      
         BE    CANNOTD                                                          
                                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RIBLKEY,R6                                                       
         MVI   RIBLKTYP,RIBLKTYQ                                                
         MVI   RIBLKSTY,RIBLKSTQ                                                
         MVC   RIBLKREP,AGENCY                                                  
*                                                                               
         LA    R2,IBLIDENH         VALIDATE SET IDENTIFIER                      
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK0010              YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      NO - OPTIONAL FOR LIST AND REPORT            
         BE    VK0050                                                           
         CLI   ACTNUM,ACTREP                                                    
         BE    VK0050                                                           
*                                                                               
         B     MISSFLD             OTHERWISE REQUIRED                           
*                                                                               
VK0010   DS    0H                                                               
         CLI   5(R2),L'RIBLKTAG                                                 
         BH    INVLFLD                                                          
*                                                                               
         MVC   RIBLKTAG,8(R2)                                                   
         OC    RIBLKTAG,SPACES                                                  
*                                                                               
VK0050   DS    0H                                                               
         LA    R2,IBLTYPEH         VALIDATE BOOKTYPE                            
         CLI   5(R2),0                                                          
         BE    VK0100              NOT ENTERED -  OPTIONAL                      
*                                                                               
         CLI   5(R2),L'RIBLKBTY                                                 
         BH    INVLFLD                                                          
*                                                                               
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         MVI   WORK+5,8                                                         
         MVC   WORK+8(6),=C'MAY98('                                             
*                                                                               
         ZIC   R1,IBLTYPEH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8+6(0),IBLTYPE                                              
*                                                                               
         LA    RF,WORK+8+6                                                      
         LA    RF,1(R1,RF)                                                      
         MVI   0(RF),C')'                                                       
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 BOOKVAL,DMCB,(C'N',WORK),(1,WORK+16+8),                 +        
               (C'B',SCANNER),FULL,(C'C',ACOMFACS)                              
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    INVLFLD                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      NO - OPTIONAL FOR LIST AND REPORT            
         BE    VK0060                                                           
         CLI   ACTNUM,ACTREP                                                    
         BE    VK0060                                                           
*                                                                               
         MVC   WORK(L'KEY),KEY     MAKE SURE WE HAVE BASE REC                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   BASEREC                                                          
*                                                                               
VK0060   DS    0H                                                               
         MVC   RIBLKBTY,8(R2)                                                   
*                                                                               
VK0100   DS    0H                                                               
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                  SEE IF ANY CHANGES WERE MADE                 
         LA    R2,IBLDESCH                                                      
         CLI   IBLDESCH+5,0        MUST BE ENTERED                              
         BE    MISSFLD                                                          
*                                                                               
         TM    IBLDESCH+4,X'20'                                                 
         BZ    VR0020                                                           
*                                                                               
         LA    R2,IBLBOOKH                                                      
L        USING IBLBOOKH,R2                                                      
VR0010   DS    0H                                                               
         TM    L.IBLBOOKH+4,X'20'                                               
         BZ    VR0020                                                           
         TM    L.IBLOVNMH+4,X'20'                                               
         BZ    VR0020                                                           
         LA    R2,IBLNXTBH-IBLBOOKH(R2)                                         
         LA    RF,IBLENDLH                                                      
         CR    R2,RF                                                            
         BNH   VR0010                                                           
         B     VRX                 NOTHING WAS CHANGED                          
         DROP  L                                                                
*                                                                               
VR0020   DS    0H                                                               
         MVI   NOBLANK,C'N'                                                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RIBLBKEQ     REMOVE ALL BOOK ELEMENTS                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,IBLBOOKH                                                      
L        USING IBLBOOKH,R2                                                      
VR0030   DS    0H                                                               
         CLI   L.IBLBOOKH+5,0      ANY ENTRY?                                   
         BNE   VR0032              YES                                          
         CLI   L.IBLOVNMH+5,0      NO - ALIAS NOT ALLOWED                       
         BNE   INVLFLD                                                          
         B     VR0050                                                           
*                                                                               
VR0032   DS    0H                                                               
         MVI   NOBLANK,C'Y'                                                     
*                                                                               
         BAS   RE,CHECKDUP                                                      
         BNE   DUPLIC8                                                          
*                                                                               
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         MVC   WORK+5(1),L.IBLBOOKH+5                                           
         MVC   WORK+8(L'IBLBOOK),L.IBLBOOK                                      
*                                                                               
         CLC   IBLTYPE,SPACES      ADD ON BOOKTYPE?                             
         BNH   VR0034              NO                                           
*                                                                               
         ZIC   RE,WORK+5                                                        
         LA    RF,WORK+8(RE)                                                    
         MVI   0(RF),C'('                                                       
*                                                                               
         ZIC   R1,IBLTYPEH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),IBLTYPE                                                  
         LA    RF,2(R1,RF)                                                      
         MVI   0(RF),C')'                                                       
         LA    RE,3(R1,RE)                                                      
         STC   RE,WORK+5                                                        
*                                                                               
VR0034   DS    0H                                                               
         XC    FULL,FULL                                                        
         GOTO1 BOOKVAL,DMCB,(C'N',WORK),(1,WORK+16+8),                 +        
               (C'B',SCANNER),FULL,(C'C',ACOMFACS)                              
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    INVLFLD                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RIBLBOOK,R6                                                      
         MVI   RIBLBKEC,RIBLBKEQ                                                
         MVI   RIBLBKLN,RIBLBKLQ                                                
         MVC   RIBLBKBK,WORK+16+8                                               
         MVC   RIBLBKOV,L.IBLOVNM          <- VALIDATE FOR INVALID BK?          
         OC    RIBLBKOV,SPACES               IF YES REMOVE EXTRA CHK IN         
         DROP  R6                            CHECKDUP                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR0050   DS    0H                                                               
         LA    R2,IBLNXTBH-IBLBOOKH(R2)                                         
         LA    RF,IBLENDLH                                                      
         CR    R2,RF                                                            
         BNH   VR0030                                                           
         DROP  L                                                                
*                                                                               
         LA    R2,IBLBOOKH                                                      
         CLI   NOBLANK,C'N'                                                     
         BE    MISSFLD                                                          
*                                                                               
         L     R6,AIO              DELETE COMMENT ELEMENT                       
         MVI   ELCODE,RIBLCMEQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,IBLDESCH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM             BUILD NEW COMMENT                            
         USING RIBLCOMM,R6                                                      
         MVI   RIBLCMEC,RIBLCMEQ   ELEMENT TYPE                                 
         MVI   RIBLCMLN,RIBLCMOQ   ELEMENT OVERHEAD LENGTH                      
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RIBLCMTX(0),8(R2)                                                
*                                                                               
         ZIC   RF,RIBLCMLN         ADD VARIABLE LENGTH FROM                     
         LA    RF,1(R1,RF)         DESCRIPTION FIELD                            
         STC   RF,RIBLCMLN                                                      
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RIBLDEEQ     GET DECRIPTION ELEMENT IF IT EXISTS          
         BAS   RE,GETEL                                                         
         BNE   VR0070                                                           
*                                                                               
         ZIC   R1,1(R6)            SAVE ELEMENT                                 
         BCTR  R1,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         L     R6,AIO              NOW DELETE IT                                
         MVI   ELCODE,RIBLDEEQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
VR0070   DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RIBLDESC,R6                                                      
         MVI   RIBLDEEC,RIBLDEEQ                                                
         MVI   RIBLDELN,RIBLDELQ                                                
         GOTO1 DATCON,DMCB,(5,0),(19,RIBLDELM)                                  
*                                                                               
         L     RF,ACOMFACS         LUID OF CHANGE                               
         LA    RF,CGETFACT-COMFACSD(RF)                                         
         L     RF,0(RF)                                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   RIBLDELU,FASYM                                                   
         DROP  R6,RF                                                            
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVC   SAVEKEY,KEY         RESTART FROM HERE                            
*                                                                               
         LA    R6,KEY                                                           
         USING RIBLKEY,R6                                                       
         MVC   IBLIDEN(L'RIBLKTAG),RIBLKTAG                                     
         OI    IBLIDENH+6,X'80'    XMIT                                         
         MVC   IBLTYPE(L'RIBLKBTY),RIBLKBTY                                     
         OI    IBLTYPEH+6,X'80'    XMIT                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         XC    IBLLCHG,IBLLCHG                                                  
         XC    IBLDESC,IBLDESC                                                  
                                                                                
         TWAXC IBLBOOKH,PROT=Y     CLEAR SCREEN                                 
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,RIBLDEEQ                                                  
         USING RIBLDESC,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(8,RIBLDELM),(5,IBLLCHG)                             
         OI    IBLLCHGH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RIBLCMEQ                                                  
         USING RIBLCOMM,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   DR0010                                                           
*                                                                               
         ZIC   R1,RIBLCMLN         COPY COMMENT                                 
         AHI   R1,-(RIBLCMOQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IBLDESC(0),RIBLCMTX                                              
         OI    IBLDESCH+6,X'80'     XMIT                                        
         DROP  R6                                                               
*                                                                               
DR0010   DS    0H                                                               
         LA    R2,IBLBOOKH                                                      
L        USING IBLBOOKH,R2                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RIBLBKEQ                                                  
         USING RIBLBOOK,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                 HAS TO BE ONE                                
         DC    H'0'                                                             
*                                                                               
DR0020   DS    0H                                                               
         GOTO1 VUNBOOK,DMCB,(1,RIBLBKBK),L.IBLBOOKH,0,(C'+',=CL6' ')            
         OI    L.IBLBOOKH+6,X'80'  XMIT                                         
         MVC   L.IBLOVNM,RIBLBKOV                                               
         OI    L.IBLOVNMH+6,X'80'  XMIT                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR0030                                                           
*                                                                               
         LA    R2,IBLNXTBH-IBLBOOKH(R2)                                         
         LA    RF,IBLENDLH                                                      
         CR    R2,RF                                                            
         BNH   DR0020                                                           
         DROP  L,R6                                                             
*                                                                               
DR0030   DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
         OC    SAVEKEY,SAVEKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                                                             
         MVC   KEY,SAVEKEY                                                      
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
LR05     DS    0H                                                               
         OC    KEY(L'RIBLKEY),KEY                                               
         BNZ   LR10                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RIBLKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RIBLKTYP,RIBLKTYQ                                                
         MVI   RIBLKSTY,RIBLKSTQ                                                
         MVC   RIBLKREP,AGENCY                                                  
*                                                                               
         CLI   IBLIDENH+5,0        NAME FILTER?                                 
         BE    LR10                NO                                           
*                                                                               
         OC    IBLIDEN,SPACES                                                   
         MVC   RIBLKTAG,IBLIDEN                                                 
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(RIBLKTAG-RIBLKEY),KEYSAVE                                    
         BNE   LRX                                                              
*                                                                               
         LA    R6,KEY                                                           
         USING RIBLKEY,R6                                                       
         CLI   IBLIDENH+5,0        NAME FILTER?                                 
         BE    LR22                NO                                           
*                                                                               
         CLC   RIBLKTAG,IBLIDEN                                                 
         BE    LR22                                                             
         BL    LRSEQ                                                            
*                                                                               
         XC    RIBLKBTY,RIBLKBTY                                                
         B     LR23                DO A SKIP READ                               
*                                                                               
LR22     DS    0H                                                               
         CLI   IBLTYPEH+5,0        BOOKTYPE FILTER?                             
         BE    LR24                NO                                           
*                                                                               
         CLC   RIBLKBTY,IBLTYPE                                                 
         BE    LR24                                                             
         BL    LRSEQ                                                            
*                                                                               
         MVC   RIBLKBTY,IBLTYPE                                                 
LR23     DS    0H                                                               
         ZIC   RE,RIBLKTAG+(L'RIBLKTAG-1)       DO A SKIP READ                  
         LA    RE,1(RE)                                                         
         STC   RE,RIBLKTAG+(L'RIBLKTAG-1)                                       
*                                                                               
         B     LR10                                                             
*                                                                               
LR24     DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RIBLREC,R6                                                       
         MVC   LIBLIDEN,RIBLKTAG                                                
         MVC   LIBLTYPE,RIBLKBTY                                                
         DROP  R6                                                               
*                                                                               
         USING RIBLCOMM,R6                                                      
         MVI   ELCODE,RIBLCMEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR30                                                             
*                                                                               
         CLI   RIBLCMLN,RIBLCMOQ   SKIP IF NO DESCRIPTION                       
         BNH   LR30                                                             
*                                                                               
         ZIC   R1,RIBLCMLN                                                      
         AHI   R1,-(RIBLCMOQ+1)                                                 
         CHI   R1,L'LIBLDESC-1                                                  
         BNH   *+8                                                              
         LA    R1,L'LIBLDESC-1                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LIBLDESC(0),RIBLCMTX                                             
         DROP  R6                                                               
*                                                                               
LR30     DS    0H                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
                                                                                
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         MVC   H6(26),=C'INVENTORY BOOK LIST REPORT'                            
         GOTO1 CENTER,DMCB,H6,88                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK IF FIELD INPUT IS A DUPLICATE                                           
* R2 POINTS TO FIELD HEADER                                                     
***********************************************************************         
CHECKDUP NTR1                                                                   
         LA    R3,IBLBOOKH                                                      
O        USING IBLBOOKH,R2                                                      
N        USING IBLBOOKH,R3                                                      
CHKDUP10 DS    0H                                                               
         CR    R3,R2                                                            
         BNL   YES                                                              
         CLC   O.IBLBOOK,N.IBLBOOK                                              
         BE    NO                                                               
         CLC   O.IBLBOOK,N.IBLOVNM                                              
         BE    NO                                                               
*                                                                               
         CLC   O.IBLOVNM,SPACES                                                 
         BNH   CHKDUP20                                                         
*                                                                               
         CLC   O.IBLOVNM,N.IBLOVNM                                              
         BE    NO                                                               
         CLC   O.IBLOVNM,N.IBLBOOK                                              
         BE    NO                                                               
CHKDUP20 DS    0H                                                               
         LA    R3,IBLNXTBH-IBLBOOKH(R3)                                         
         B     CHKDUP10                                                         
         DROP  O,N                                                              
         EJECT                                                                  
**********************************************************************          
RELO     DS    A                                                                
         SPACE 3                                                                
                                                                                
                                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
BASEREC  MVC   RERROR,=AL2(837)                                                 
         B     ERREND                                                           
DUPLIC8  MVC   RERROR,=AL2(401)                                                 
         B     ERREND                                                           
CANNOTD  MVC   RERROR,=AL2(12)                                                  
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMA3D          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMA4D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* APPLICATION WORK AREA                                                         
*                                                                               
         ORG   SYSSPARE                                                         
SAVEKEY  DS    XL27                SAVED KEY FOR LIST REDISPLAY                 
NOBLANK  DS    X                   Y/N                                          
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LIBLIDEN DS    CL8                                                              
         DS    CL5                                                              
LIBLTYPE DS    CL2                                                              
         DS    CL3                                                              
LIBLDESC DS    CL60                                                             
*                                                                               
* OFFLINE LIST LINE                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
       ++INCLUDE REGENIBKL                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080RESFM32   06/11/08'                                      
         END                                                                    
