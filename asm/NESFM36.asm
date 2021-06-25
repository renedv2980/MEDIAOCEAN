*          DATA SET NESFM36    AT LEVEL 132 AS OF 10/31/05                      
*PHASE T31C36A,*                                                                
*                                                                               
***********************************************************************         
*                                                                               
* BPOO 12/14   FIX ACTION OF FILTER SCRREN TO BE SAME AS PREV SCREEN            
* BPOO 12/14   DEFAULT SCREEN HAS PFKEY LINE ON BOTTOM                          
* BPOO 12/13   CHANGE PFKEY MESSAGES FOR FILTER SCREEN                          
* BPOO 12/13   FIX FILTER SCREEN CURSORS                                        
* BPOO 11/24   REQUEST SCREEN SAVED IN TWA AND RETRIEVED FOR NEXT               
*              REQUEST AS A DEFAULT                                             
* BPOO 11/23   LIMIT BOOK LENGTH ON SCREEN                                      
* BPOO 11/23   SUPPORT REPORT ACTION                                            
* BPOO 11/23   ADD DESCRIPTION FIELD ON SCREEN                                  
* BPOO 11/23   INVALID REQUEST OR EMPTY REQUESTS NOW RETURN ERROR               
* BPOO 11/22   FIX DEMAND CALL BUG.                                             
* BPOO 11/22   ACTION ADD DOESNT GO TO DEFAULT REQUEST PAGE                     
*              USER CAN NOW ADD IN NTI CODES THEMSELVES                         
* BPOO 11/22   COPIED PAN BOOK FROM NESFM36A                          *         
*              FIX STATION TO 6 BYTES                                 *         
*                                                                     *         
*                                                                     *         
*  TITLE        T31C36 -  PROGRAM GROUP MAINT/LIST PROGRAM            *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T31C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T217C6 (MAINTENANCE)                           *         
*               SCREEN T217C0 (LIST)                                  *         
*               SCREEN T217CF (MAINT FILER SCREEN)                    *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                               
*               R8 -- WORK                                            *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - PSR RECORD                                      *         
*               IO2 - MISC.                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T31C36 - PROGRAM GROUP RECORDS '                                
T31C36   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C36,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   DEMAND,CDEMAND-COMFACSD(RF)    A(DEMAND)                         
         MVC   VGETDAY,CGETDAY-COMFACSD(RF)    A(DEMAND)                        
         MVC   VADDAY,CADDAY-COMFACSD(RF)    A(DEMAND)                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A26'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DEFINE,DMCB                                                      
*                                                                               
         MVC   DMCB+4(4),=X'D9000A17'       TIMVAL                              
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NETWEEKS,DMCB                                                    
*                                                                               
*                                                                               
         BAS   RE,SETUP                                                         
         OI    CONSERVH+6,X'81'                                                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   ACTNUM,PRINTREP                                                  
         BE    CHKMODE                                                          
         CLI   ACTNUM,ACTLIST                                                   
         BE    CHKMODE                                                          
         CLI   ACTNUM,ACTSEL                                                    
         BE    CHKMODE                                                          
*   NEWWWWWWWWWWWWWWWWWWWWWWWW                                                  
***      XC    DATPDES,DATPDES                                                  
**       OI    DATPDESH+6,X'80'                                                 
**       CLI   PFAID,4                                                          
**       BNE   *+14                                                             
**       MVC   DATPDES(3),=C'PF4'                                               
**       OI    DATPDESH+6,X'80'                                                 
*                                                                               
*                                                                               
*                                                                               
                                                                                
***************************************************                             
*  CHECK IF FILTER SCREEN PFKEYS ARE HITTED                                     
***************************************************                             
*                                                                               
PF05     CLI   SCRNNUM,1                                                        
         BE    PF505                                                            
**** ON SCREEN 2  (FILTER SCREEN)                                               
***      CLI   PFAID,8      NEWLY TAKEN OUT                                     
***      BE    PF0510                                                           
***      CLI   PFAID,8      NEWLY TAKEN OUT                                     
***      BNE   PF505                                                            
***      OC    SVFILNET,SVFILNET                                                
***      BZ    PF0510                                                           
*                                                                               
PF505    CLI   ACTNUM,ACTADD                                                    
         BNE   PF0210                                                           
         CLI   PFAID,2                                                          
         BE    PF0210                                                           
***      B     PF0215                                                           
         B     PF0210    DO THE SAME AS CHA NO DEFAULT FILT SCREEN              
*                                                                               
PF0510   OC    FILNET,FILNET                                                    
* NEW  === IF PF8 ON CHANGE THAN CLRFILT  PF8 IS TO GO BACK                     
*                                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    FILTERR                                                          
*        CLI   ACTNUM,ACTCHA     IF PF8 PRESS THAT MEANS                        
*        BNE   PF5011            GO BACK                                        
         GOTO1 CLRFILT     NEWWWWWW PF8 CLEARS 11/20                            
*                                                                               
PF5011   GOTO1 MAINSCRN                                                         
PF5012   MVI   SCRNNUM,1                                                        
         LA    R4,CONKEYH                                                       
PF50A    ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   PF50A                                                            
         MVC   1(2,R4),=X'0101'                                                 
*        B     CHKMODE                                                          
* NEWEST                                                                        
**        XC    DATPFK,DATPFK                                                   
**        MVC   DATPFK(L'ADDPFLIN),ADDPFLIN                                     
**        OI    DATPFKH+6,X'80'                                                 
         B     DR                                                               
*                                                                               
         B     PF0240                                                           
*                                                                               
********************************************                                    
PF0210   CLI   SCRNNUM,2          IF ON THE SECOND SCREEN                       
         BE    PF0220             IF ON 2ND SCREEN NO NEED TO NEW SCRN          
         CLI   PFAID,2            CHECK IF PFKEY 2 IS HITTED                    
*        BNE   CHKMODE                                                          
         BNE   PF0240                                                           
         GOTO1 CLRFILT            CLEAR FILTERS PF2 HITTED                      
PF0215   MVC   SVPGNAME,DATPNM                                                  
         MVC   SVPDES,DATPDES    NEW                                            
         CLI   SCRNNUM,2         ACTION ADD ALSO CHECK SCREEN                   
         BE    PF0220                                                           
         CLI   ACTNUM,ACTCHA                                                    
         BE    PF0218                                                           
******   OC    SVFILBK,SVFILBK                                                  
******   OC    SVFILNET,SVFILNET     NEWLY TAKEN OUT 11/27                      
         OC    SVFILE,SVFILE       NEWLY ADDED     11/27                        
*        BNZ   CHKMODE                                                          
         BNZ   PF0240                                                           
PF0218   DS    0H                                                               
*                                                                               
         GOTO1 FILSCRN                                                          
PF0220   DS    0H                                                               
PF0225   GOTO1 VALFILT                                                          
         MVI   FILPGNUM,0                                                       
         CLI   BLNKSCRN,C'Y'                                                    
         BE    DR                                                               
         CLI   ACTNUM,ACTDIS    ACTION DISPLAY DONT HAVE TO DISPLAY             
         BE    PF0240           TABLE FROM FILE, ONLY CHA AND ADD               
         GOTO1 DISTAB                                                           
         B     PF0240                                                           
PF0240   DS    0H                                                               
************************************************************                    
*                                                                               
CHKMODE  CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   CHKMOD05                                                         
**       OC    SVFILNET,SVFILNET    NEWLY TAKEN OUT                             
**       BZ    VR                   11/27                                       
         OC    SVFILE,SVFILE         NEWLY ADDED 11/27                          
         BZ    VR                                                               
         XC    DATPFK,DATPFK                                                    
         MVC   DATPFK(L'ADDPFLIN),ADDPFLIN                                      
         OI    DATPFKH+6,X'80'                                                  
         B     VRFILE                                                           
CHKMOD05 CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   CHKMOD10                                                         
         CLI   ACTNUM,ACTDIS                                                    
*        BNE   DR                                                               
         BE    DR                                                               
         GOTO1 CLRFILT                                                          
*        B     DR                                                               
         B     DR                                                               
*        OC    SVFILNET,SVFILNET                                                
*        BZ    DR                                                               
CHKMOD10 CLI   MODE,XRECADD                                                     
         BE    XR                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    RE,HEDSPECS                                                      
         ST    RE,SPECS                                                         
         B     PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************                      
* VALIDATE KEY ROUTINE                                                          
**********************************************************                      
VK       DS    0H                                                               
         BAS   RE,SAVEDEF                                                       
*                                                                               
VK01     CLC   SVPGNAME,DATPNM                                                  
         BE    VK03                                                             
         GOTO1 CLRFILT                                                          
*                                                                               
VK03     MVC   SVPGNAME,DATPNM                                                  
         MVC   SVPDES,DATPDES                                                   
VK05     LA    R6,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         USING NPRGRECD,R6                                                      
         MVC   NPRGKTYP(2),=X'0D3C'                                             
         MVC   NPRGKCOD,DATPNM                                                  
         OC    NPRGKCOD,=X'4040404040404040'                                    
*                                                                               
          XC    FAKEFLD,FAKEFLD          ALWAYS NETWORK                         
          MVI   FAKEFLD+8,C'N'                                                  
          MVI   FAKEFLD+5,1                                                     
          LA    RE,DATPNMH                                                      
          ST    RE,ACURFORC                                                     
          LA    R2,FAKEFLD                                                      
          MVC   AIO,AIO2                                                        
          GOTO1 VALIMED                  GET AGENCY                             
          MVC   NPRGKAGM,BAGYMD                                                 
*                                                                               
*                                                                               
VKX       XC    KEY,KEY                                                         
          MVC   KEY,SVKEY           GENCON NEEDS KEY                            
          MVC   AIO,AIO1                                                        
*                                                                               
*                                                                               
          CLI   ACTNUM,ACTADD                                                   
          BNE   VKXX                                                            
          MVI   VKFLAG,C'Y'                                                     
***       MVI   SCRNNUM,1                                                       
***       BNE   VKXX                                                            
***       LA    RE,DATPGM1H                                                     
*                                                                               
          MVI   DATENDFH+5,1                                                    
*         MVI   8(RE),C'A'                                                      
*         OI    6(RE),X'80'                                                     
*         XC    CONHEAD,CONHEAD                                                 
*         MVC   CONHEAD(L'ADDMSG),ADDMSG                                        
*         XC    CONHEAD+6,X'80'                                                 
*                                                                               
VKXX      B     XIT                                                             
          DROP  R6                                                              
          EJECT                                                                 
*                                                                               
****************************************************                            
*        VALREC                                                                 
****************************************************                            
VR        DS    0H                                                              
*                                                                               
*                                                                               
          XC    DATPFK,DATPFK                                                   
          MVC   DATPFK(L'DISPFLIN),DISPFLIN                                     
          OI    DATPFKH+6,X'80'                                                 
*                                                                               
*  CHECK DESCRIPTION FIELD                                                      
          MVI   ELCODE,NPDESELQ                                                 
          GOTO1 REMELEM                                                         
          LA    R2,DATPDESH                                                     
          CLI   5(R2),0                                                         
          BE    VR10                                                            
          LA    R5,ELEM                                                         
          XC    ELEM,ELEM                                                       
          USING NPDESEL,R5                                                      
          MVI   NPDESEL,NPDESELQ                                                
          MVC   NPDES,8(R2)                                                     
          OC    NPDES,=20X'40'                                                  
          MVI   NPDESLEN,NPDESQ                                                 
          GOTO1 ADDELEM                                                         
*                                                                               
*                                                                               
VR10      L     R6,AIO                                                          
          LA    R4,ADDTAB           TAB OF ELEMS TO ADD                         
*****     LA    R4,FILTAB           TAB OF ELEMS TO ADD                         
          USING ADDTABD,R4                                                      
          LA    R2,DATPGM1H         1ST DATA FIELD IN SCREEN                    
          USING SCRNDATD,R2         DSECT FOR EACH SCREEN DATA                  
          LR    RE,R2                                                           
          ST    RE,ACURFORC                                                     
*                                                                               
**        CLI   ACTNUM,ACTADD                                                   
*         CLI   5(R2),0                                                         
*         BE    MISSERR                                                         
*                                                                               
VR20      TM    1(R2),X'20'         PROTECTED IS END OF PAGE                    
          BO    VRX                                                             
          OC    SNET,SNET           EMTPY FIELD IGNORE                          
          BZ    VR40                                                            
*                                                                               
          MVC   SVSNET,SNET                                                     
          OC    SVSNET,=X'4040404040'                                           
          MVC   SVSNTINM,SNTINUM                                                
          OC    SVSNTINM,=C'00000'                                              
          LR    RF,R2                   CURRENT SCREEN POSITION                 
          LA    RE,DATPGM1H             1ST DATA FIELD IN SCREEN                
          SR    RF,RE                                                           
          SR    RE,RE                                                           
          LA    R5,SCRNDATQ             EACH SCREEN ELEMENT LENGTH              
          DR    RE,R5                   INDEX OF WHICH SCREEN ELEMENT           
          ST    RF,DMCB                 DMCB FOR DELELEM                        
*                                                                               
          LA    R5,L'SVFIELD            LEN OF RECS IN TABLE                    
          SR    RE,RE                                                           
          MR    RE,R5                                                           
          LA    R5,SCRNTAB                                                      
          AR    RF,R5                   R5 POINTS TABLE REC                     
          CLC   SVFIELD,0(RF)           DID SCREEN FIELD CHANGE                 
          BE    VR25                    IF SCREEN FIELD DIDNT CHANGE            
******************************                                                  
*                                                                               
          LR    RE,R2                                                           
          ST    RE,ACURFORC                                                     
*                                                                               
* VALIDATE NET WITH DEGET ROUTINE VALSTA                                        
*                                                                               
****8     CLI   ACTNUM,ACTCHA           ACTION ADD CHECK NET                    
****8     BNE   VR24                                                            
****      CLI   ACTNUM,ACTADD           ACTION ADD CHECK NET                    
****      BE    VR24              COMMENT OUT, ADD GOES TO VRFILE               
****                              NOW                                           
* IF COMMAND TO DELETE ELEMENT                                                  
*                                                                               
          CLC   =X'C400000000',SNET          NET FIELD DELETION                 
          BE    VR23                                                            
          CLC   =C'DEL',SNET                                                    
          BE    VR23                                                            
          B     VR24                    NOT DELETION CHECK NET                  
VR23      LR    RF,R2                   CURRENT SCREEN POSITION                 
          LA    RE,DATPGM1H         1ST DATA FIELD IN SCREEN                    
          SR    RF,RE                                                           
          SR    RE,RE                                                           
          LA    R5,SCRNDATQ             EACH SCREEN ELEMENT LENGTH              
          DR    RE,R5                   INDEX OF WHICH SCREEN ELEMENT           
          XC    DMCB,DMCB               WE R DELETING                           
          ST    RF,DMCB                                                         
          GOTO1 DELELEM,DMCB                                                    
          MVI   ELCODE,X'FF'                                                    
          GOTO1 REMELEM                                                         
          MVI   ELCODE,NPRGELQ         REMOVE ELEMENT AND CHECK NEXT            
          B     VR40                                                            
********************************************                                    
VR24      DS    0H                                                              
*-------------------------------                                                
*&&DO                                                                           
          LA    R5,DBLOCK1                                                      
          USING DBLOCKD,R5                                                      
**8       XC    DBLOCK,DBLOCK                                                   
          LA    R1,DBLOCK1                                                      
          XCEF  (R1),276                                                        
          MVC   DBAREC,AIO3                                                     
*                                                                               
          MVI   DBFUNCT,DBVLNBK          FUNCTION                               
          MVC   DBCOMFCS,ACOMFACS         A(COMFACS),                           
          MVI   DBSELSRC,C'N'             SOURCE,                               
          MVC   DBSELBK,SVFILBK           BOOK,                                 
*         MVI   DBBTYPE,C'A'                                                    
          MVI   DBSELMED,C'N'            MEDIA                                  
          MVC   DBSELAGY,AGENCY           AGENCY CODE,                          
          MVC   DBFILE,=C'NTI'                                                  
          MVC   DBSELSTA,8(R2)                                                  
          OC    SVDAY,SVDAY                                                     
          BZ    *+10                                                            
          MVC   DBSELDAY,SVDAY                                                  
*                                                                               
          OC    SVTIME,SVTIME                                                   
          BZ    *+10                                                            
          MVC   DBSELTIM,SVTIME                                                 
*                                                                               
***       BAS   RE,SETDEF                                                       
          GOTO1 DEMAND,DMCB,DBLOCK,DUMBHK,0                                     
****      GOTO1 DEMAND,DMCB,DBLOCK,0                                            
          CLI   DBERROR,0                                                       
          BE    VR25                                                            
          CLI   DBERROR,X'80'                                                   
          BE    *+6                                                             
          DC    H'0'                                                            
          B     INVNET                                                          
*&&                                                                             
*-----------------------------------                                            
* CHECK IF SCREEN FIELDS CHANGED                                                
*                                                                               
VR25      DS    0H                                                              
****      BAS   RE,RSTRDEF                                                      
*                                                                               
          LR    RF,R2                   CURRENT SCREEN POSITION                 
          LA    RE,DATPGM1H             1ST DATA FIELD IN SCREEN                
          SR    RF,RE                                                           
          SR    RE,RE                                                           
          LA    R5,SCRNDATQ             EACH SCREEN ELEMENT LENGTH              
          DR    RE,R5                   INDEX OF WHICH SCREEN ELEMENT           
          ST    RF,DMCB                 DMCB FOR DELELEM                        
*                                                                               
          LA    R5,L'SVFIELD            LEN OF RECS IN TABLE                    
          SR    RE,RE                                                           
          MR    RE,R5                                                           
          LA    R5,SCRNTAB                                                      
          AR    RF,R5                   R5 POINTS TABLE REC                     
          CLC   SVFIELD,0(RF)           DID SCREEN FIELD CHANGE                 
          BE    VR40                    IF SCREEN FIELD DIDNT CHANGE            
*----------------------------                                                   
*&&DO                                                                           
*************************************                                           
*   VALIDATE THE NTI NUMBER                                                     
*************************************                                           
          LA    R5,DBLOCK1                                                      
          USING DBLOCKD,R5                                                      
***       XC    DBLOCK,DBLOCK                                                   
          LA    R1,DBLOCK1                                                      
          XCEF  (R1),276                                                        
          MVC   DBAREC,AIO3                                                     
*                                                                               
          MVI   DBFUNCT,DBGETNTI         FUNCTION                               
          MVC   DBCOMFCS,ACOMFACS         A(COMFACS),                           
          MVI   DBSELSRC,C'N'             SOURCE,                               
*         MVC   DBSELBK,=X'6306'          BOOK,                                 
          MVC   DBSELBK,SVFILBK           BOOK,                                 
*         MVI   DBBTYPE,C'A'                                                    
          MVI   DBSELMED,C'N'            MEDIA                                  
*         MVC   DBSELAGY,=C'SJ'           AGENCY CODE,                          
          MVC   DBSELAGY,AGENCY           AGENCY CODE,                          
*         MVC   DBFILE,=C'NTI'                                                  
*                                                                               
          MVC   DBFILE,=C'NTI'                                                  
          MVC   DBSELSTA,8(R2)                                                  
          OC    SVDAY,SVDAY                                                     
          BZ    *+10                                                            
          MVC   DBSELDAY,SVDAY                                                  
*                                                                               
          OC    SVTIME,SVTIME                                                   
          BZ    *+10                                                            
          MVC   DBSELTIM,SVTIME                                                 
*                                                                               
*         MVC   DBSELSTA,8(R2)                                                  
*&&                                                                             
* -=--------------------------                                                  
          LA    RE,SNTINUMH                                                     
          ST    RE,ACURFORC                                                     
          TM    SNTINUMH+4,X'08'                                                
*         BZ    INVERR                                                          
          BZ    INVNTIN                                                         
          CLI   SNTINUMH+5,0                                                    
          BE    MISSERR                                                         
          XC    DUB,DUB                                                         
*                                                                               
          ZIC   RE,SNTINUMH+5            NTI CODE                               
          BCTR  RE,0                                                            
          XC    DUB,DUB                                                         
          EX    RE,*+8                                                          
          B     *+10                                                            
          PACK  DUB,SNTINUM(0)                                                  
          CVB   R0,DUB                                                          
****      STH   R0,HALF                                                         
          XC    SVBNTI,SVBNTI                                                   
***       STCM  R0,3,SVBNTI+1    SAVE BINARY NTI TO EDIT LATER                  
          STCM  R0,7,SVBNTI     SAVE BINARY NTI TO EDIT LATER                   
*                                                                               
          LA    RE,SNTINAMH                                                     
          ST    RE,ACURFORC                                                     
          CLI   SNTINAMH+5,0                                                    
          BE    MISSERR                                                         
*                                                                               
*&&DO                            3 BYTES W HIGH ORDER BYTE AS NULLS             
          MVC   DBSELPRG,HALF                                                   
***       BAS   RE,SETDEF                                                       
          GOTO1 DEMAND,DMCB,DBLOCK,DUMBHK,0                                     
****      GOTO1 DEMAND,DMCB,DBLOCK,0                                            
          CLI   DBERROR,0                                                       
          BE    VR28                                                            
          CLI   DBERROR,X'80'                                                   
          BE    *+6                                                             
          DC    H'0'                                                            
          B     INVNTIN                                                         
*&&                                                                             
*---------------------------------                                              
VR28      DS    0H                                                              
*&&DO                                                                           
****      BAS   RE,RSTRDEF                                                      
          GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,WORK                             
          MVC   SNTINAME,WORK                                                   
          OI    SNTINAMH+6,X'80'                                                
*&&                                                                             
*----------------------------------                                             
VR30      LA    R5,ELEM             CHUNK                                       
          XC    ELEM,ELEM                                                       
          USING NPRGEL,R5                                                       
          MVI   NPRGEL,NPRGELQ                                                  
*                                                                               
          MVC   NPRGENET,SNET                                                   
          OC    NPRGENET,=X'4040404040'                                         
*                                                                               
          CLC   =X'4040',NPRGENET+3                                             
          BNE   *+8                                                             
          MVI   NPRGENET+4,C'T'                                                 
*                                                                               
*   NEW CODE TO CONVERT STATION                                                 
          XC    DMCB,DMCB                                                       
          MVC   DMCB(L'NPRGENET),NPRGENET                                       
          GOTO1 CNVSTA,DMCB                                                     
          OC    DMCB,DMCB                                                       
          BNZ   VR33                                                            
          LA    RE,SNETH                                                        
          ST    RE,ACURFORC                                                     
          B     INVNET                                                          
VR33      MVC   NPRGENET,DMCB+8                                                 
*                                                                               
          EDIT  (B3,SVBNTI),(5,NPRGENTI),0,ZERO=BLANK,ALIGN=RIGHT               
          OC    NPRGENTI,=X'F0F0F0F0F0'                                         
*                                                                               
*                                                                               
          MVC   NPRNTINM,SNTINAME                                               
*                                                                               
          MVI   NPRGELEN,NPRGELNQ                                               
          CLI   ACTNUM,ACTADD                                                   
          BNE   VR35                                                            
          GOTO1 ADDELEM                                                         
          B     VR40                                                            
          DROP  R5                                                              
VR35      DS    0H                                                              
***************** ACTION ADD SKIPS ALL THE FOLLOWING ******                     
*                                                                               
          MVC   SVSNET,SNET                                                     
          OC    SVSNET,=X'4040404040'                                           
          CLC   =X'4040',SVSNET+3                                               
          BNE   *+8                                                             
          MVI   SVSNET+4,C'T'                                                   
*         MVC   SVSNTINM,SNTINUM                                                
          EDIT  (B3,SVBNTI),(5,SVSNTINM),0,ZERO=BLANK,ALIGN=RIGHT               
          OC    SVSNTINM,=C'00000'                                              
****      GOTO1 DUPELEM                                                         
          GOTO1 =A(DUPELEM),DMCB,RR=Y                                           
*         BE    DUPERR                                                          
          BE    VR40                                                            
*                                                                               
          LR    RF,R2                   CURRENT SCREEN POSITION                 
          LA    RE,DATPGM1H             1ST DATA FIELD IN SCREEN                
          SR    RF,RE                                                           
          SR    RE,RE                                                           
          LA    R5,SCRNDATQ             EACH SCREEN ELEMENT LENGTH              
          DR    RE,R5                   INDEX OF WHICH SCREEN ELEMENT           
          ST    RF,DMCB                 DMCB FOR DELELEM                        
*                                                                               
          LA    R5,L'SVFIELD            LEN OF RECS IN TABLE                    
          SR    RE,RE                                                           
          MR    RE,R5                                                           
          LA    R5,SCRNTAB                                                      
          AR    RF,R5                   R5 POINTS TABLE REC                     
          CLC   SVFIELD,0(RF)           DID SCREEN FIELD CHANGE                 
          BE    VR38                    IF SCREEN FIELD DIDNT CHANGE            
*                                                                               
          CLC   =X'FFFF',0(RF)         IF THE SCREEN HAD NOTHING BEFORE         
          BE    VR38                                                            
          OC    0(L'SCRNNET+L'SCRNNTI,RF),0(RF)                                 
          BZ    VR38                                                            
*                                                                               
          GOTO1 DELELEM,DMCB                                                    
          MVI   ELCODE,X'FF'                                                    
          GOTO1 REMELEM                                                         
          MVI   ELCODE,NPRGELQ                                                  
*                                                                               
VR38      DS    0H                                                              
*   MOVE FIELDS TO ADD TABLE TO ADD LATER                                       
          MVC   ADDNET,SNET                                                     
          OC    ADDNET,=X'4040404040'                                           
*                                                                               
          CLC   =X'4040',ADDNET+3                                               
          BNE   *+8                                                             
          MVI   ADDNET+4,C'T'                                                   
*                                                                               
*   NEW CODE TO CONVERT STATION                                                 
          XC    DMCB,DMCB                                                       
          MVC   DMCB(L'ADDNET),ADDNET                                           
          GOTO1 CNVSTA,DMCB                                                     
          OC    DMCB,DMCB                                                       
          BNZ   VR39                                                            
          LA    RE,SNETH                                                        
          ST    RE,ACURFORC                                                     
          B     INVNET                                                          
VR39      MVC   ADDNET,DMCB+8                                                   
*                                                                               
          EDIT  (B3,SVBNTI),(5,ADDNTI),0,ZERO=BLANK,ALIGN=RIGHT                 
          OC    ADDNTI,=C'00000'                                                
          MVC   ADDNTINM,SNTINAME                                               
          LA    R4,ADDTABQ(R4)                                                  
*                                                                               
VR40      LA    R2,SCRNDATQ(R2)                                                 
          B     VR20                                                            
VRX       MVI   ELCODE,X'FF'                                                    
          MVC   0(2,R4),=X'FFFF'  END OF ADDREC TABLE                           
          GOTO1 REMELEM                                                         
* NOW ADD ALL NEW ELEMENTS                                                      
*                                                                               
          LA    R4,ADDTAB                                                       
****      LA    R4,FILTAB                                                       
          USING ADDTABD,R4                                                      
VRX10     CLC   =X'FFFF',0(R4)                                                  
          BE    VRXX                                                            
          LA    R5,ELEM                                                         
          XC    ELEM,ELEM                                                       
          USING NPRGEL,R5                                                       
          MVI   NPRGEL,NPRGELQ                                                  
          MVI   NPRGELEN,NPRGELNQ                                               
          MVC   NPRGENET,ADDNET                                                 
          MVC   NPRGENTI,ADDNTI                                                 
          MVC   NPRNTINM,ADDNTINM                                               
          GOTO1 ADDELEM                                                         
          LA    R4,ADDTABQ(R4)                                                  
VRX20     B     VRX10                                                           
          DROP  R5                                                              
*  NEW FEB16                                                                    
*                                                                               
VRXX      DS    0H                                                              
          B     DR                                                              
****************************************************                            
VRFILE    DS    0H                                                              
*                                                                               
*                                                                               
*         LA    RE,ADDTAB                                                       
*         XCEF  (RE),900                                                        
*                                                                               
********   ONLY FOR ADD                                                         
          CLI   ACTNUM,ACTCHA                                                   
          BE    VRFIL80                                                         
          B     XIT                                                             
*   ADD EVERYTHING IN FILTABLE                                                  
VRFIL80   DS   0H                                                               
*&&DO                                                                           
*                                                                               
VRFIL80   CLI   PFAID,4                                                         
          BNE   VRFIL90                                                         
*                                                                               
          ZIC   RE,FILPGNUM     PF4 PRESSED GET NEW PAGE                        
          LA    RE,1(RE)                                                        
          STC   RE,FILPGNUM                                                     
          GOTO1 DISTAB                                                          
*&&                                                                             
VRFIL90   XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(27),=C'SELECT CODES TO ADD TO FILE'                     
          OI    CONHEADH+6,X'80'                                                
VRFIL100  L     R6,AIO                                                          
          LA    R2,DATPGM1H         1ST DATA FIELD IN SCREEN                    
          USING SCRNDATD,R2         DSECT FOR EACH SCREEN DATA                  
          LR    RE,R2                                                           
          ST    RE,ACURFORC                                                     
VRFIL200  TM    1(R2),X'20'         PROTECTED IS END OF PAGE                    
          BO    VRFILX                                                          
          OC    SNET,SNET           EMTPY FIELD IGNORE                          
          BZ    VRFIL400                                                        
          CLC   =X'C400000000',SNET   DONT ADD IF DELETE COMMANDS               
          BE    VRFIL400              REQUESTED                                 
          CLC   =C'DEL',SNET                                                    
          BE    VRFIL400                                                        
******************************                                                  
*                                                                               
*****8    LR    RE,R2                                                           
*****8    ST    RE,ACURFORC                                                     
*                                                                               
*                                                                               
VRFIL300  LA    R5,ELEM             CHUNK                                       
          XC    ELEM,ELEM                                                       
          USING NPRGEL,R5                                                       
          MVI   NPRGEL,NPRGELQ                                                  
*                                                                               
*         MVC   NPRGENET,SNET                                                   
          MVC   NPRGENET,SVFILNET                                               
          OC    NPRGENET,=X'4040404040'                                         
          MVC   NPRGENTI,SNTINUM                                                
*         EDIT  (B3,SVBNTI),(5,NPRGENTI),0,ZERO=BLANK,ALIGN=RIGHT               
          OC    NPRGENTI,=X'F0F0F0F0F0'                                         
*                                                                               
*                                                                               
          MVC   NPRNTINM,SNTINAME                                               
*                                                                               
          MVI   NPRGELEN,NPRGELNQ                                               
*                                                                               
* CHECK FOR DUPLICTE BEFORE ADDING                                              
          MVC   SVSNET,SNET                                                     
          OC    SVSNET,=X'4040404040'                                           
          CLC   =X'4040',SVSNET+3                                               
          BNE   *+8                                                             
          MVI   SVSNET+4,C'T'                                                   
          MVC   SVSNTINM,SNTINUM                                                
          OC    SVSNTINM,=C'00000'                                              
****      GOTO1 DUPELEM                                                         
          GOTO1 =A(DUPELEM),DMCB,RR=Y                                           
*         BE    DUPERR                                                          
          BE    VRFIL400                                                        
          GOTO1 ADDELEM                                                         
*      MOVE INTO ADDTABLE TO ADD LATER                                          
*         LA    R4,ADDTAB                                                       
*         USING ADDTABD,R4                                                      
*RFIL32   OC    0(ADDTABQ,R4),0(R4)                                             
*RFIL32   CLC   =X'FFFFFFFFFF',0(R4)                                            
*         BE    VRFIL35                                                         
*         LA    R4,ADDTABQ(R4)                                                  
*         B     VRFIL32                                                         
*                                                                               
*                                                                               
*RFIL35   MVC   ADDNET,NPRGENET                                                 
**        MVC   ADDNTI,NPRGENTI                                                 
**        MVC   ADDNTINM,NPRNTINM                                               
**        LA    R4,ADDTABQ(R4)                                                  
**        MVC   0(5,R4),=X'FFFFFFFFFF'                                          
*                                                                               
*                                                                               
VRFIL400  LA    R2,SCRNDATQ(R2)                                                 
          B     VRFIL200                                                        
VRFILX    DS    0H                                                              
* NOW ADD ALL NEW ELEMENTS                                                      
          DROP  R5                                                              
*                                                                               
VRFILXX   DS    0H                                                              
*                                                                               
          CLI   PFAID,4                                                         
          BNE   VRFILX01                                                        
          GOTO1 PUTREC                                                          
*                                                                               
          ZIC   RE,FILPGNUM     PF4 PRESSED GET NEW PAGE                        
          LA    RE,1(RE)                                                        
          STC   RE,FILPGNUM                                                     
          GOTO1 DISTAB                                                          
          MVI   PFAID,0                                                         
          B     VRFIL80                                                         
*                                                                               
*                                                                               
VRFILX01  CLI   PFAID,10                                                        
          BNE   PF10ERR                                                         
          CLI   ACTNUM,ACTCHA                                                   
          BNE   VRFILX05                                                        
          GOTO1 CLRFILT                                                         
VRFILX05  DS    0H                                                              
*                                                                               
* NOW ADD ALL NEW ELEMENTS                                                      
*&&DO                                                                           
          LA    R4,ADDTAB                                                       
          USING ADDTABD,R4                                                      
VRFILX10  CLC   =X'FFFFFFFFFF',0(R4)                                            
          BE    VRFILXXX                                                        
          LA    R5,ELEM                                                         
          XC    ELEM,ELEM                                                       
          USING NPRGEL,R5                                                       
          MVI   NPRGEL,NPRGELQ                                                  
          MVI   NPRGELEN,NPRGELNQ                                               
          MVC   NPRGENET,ADDNET                                                 
          MVC   NPRGENTI,ADDNTI                                                 
          MVC   NPRNTINM,ADDNTINM                                               
          GOTO1 ADDELEM                                                         
          LA    R4,ADDTABQ(R4)                                                  
VRFILX20  B     VRFILX10                                                        
          DROP  R5                                                              
*&&                                                                             
*RFILXXX  B     DR                                                              
VRFILXXX  DS    0H                                                              
*                                                                               
          L     R6,AIO                                                          
          MVI   ELCODE,NPDESELQ                                                 
          GOTO1 REMELEM                                                         
          LA    R2,DATPDESH                                                     
          CLI   5(R2),0                                                         
          BE    DR                                                              
          LA    R5,ELEM                                                         
          XC    ELEM,ELEM                                                       
          USING NPDESEL,R5                                                      
          MVI   NPDESEL,NPDESELQ                                                
          MVC   NPDES,8(R2)                                                     
          OC    NPDES,=20X'40'                                                  
          MVI   NPDESLEN,NPDESQ                                                 
          GOTO1 ADDELEM                                                         
*                                                                               
          B     DR                                                              
          DROP  R5                                                              
*                                                                               
*                                                                               
*         XIT1                                                                  
****************************************************                            
*       DISPLAY REC                                                             
****************************************************                            
DR        DS    0H                                                              
**        CLI   PFAID,4                                                         
**        BNE   XIT                                                             
**        MVC   DATPDES(9),=C'DR CALLED'                                        
**        OI    DATPDESH+6,X'80'                                                
**        B     XIT                                                             
*                                                                               
*                                                                               
          GOTO1 CLRPROT                                                         
          LA    R2,DATPNMH                                                      
          LA    RE,DATPNMH                                                      
          ST    RE,ACURFORC                                                     
*                                                                               
*                                                                               
          CLI   PFAID,9                                                         
          BNE   DR05                                                            
          GOTO1 CLRFILT                                                         
*                                                                               
DR05      L     R6,AIO                                                          
*                                                                               
*                                                                               
DR10      LA    RE,SCRNTAB                                                      
          XCEF  (RE),340                                                        
*                                                                               
****      CLI   PFAID,0                                                         
****      BNE   DR11                                                            
          GOTO1 CLRSCRN                                                         
          XC    DATPFK,DATPFK                                                   
          MVC   DATPFK(L'DISPFLIN),DISPFLIN                                     
          OI    DATPFKH+6,X'80'                                                 
*                                                                               
          USING NPDESEL,R6                                                      
          MVI   ELCODE,NPDESELQ                                                 
          XC    DATPDES,DATPDES                                                 
          OI    DATPDESH+6,X'80'                                                
          BAS   RE,GETEL                                                        
          BNE   *+14                                                            
          MVC   DATPDES,NPDES                                                   
          OI    DATPDESH+6,X'80'                                                
          DROP  R6                                                              
          L     R6,AIO                                                          
*                                                                               
DR11      LA    R2,DATPGM1H         1ST DATA FIELD IN SCREEN                    
          USING SCRNDATD,R2         DSECT FOR EACH SCREEN DATA                  
          LA    R5,SCRNTAB          TABLE TO STORE SCREEN                       
DR20      MVI   ELCODE,NPRGELQ                                                  
*************************************************************8                  
DR30      DS    0H                                                              
*                                                                               
          CLI   PFAID,4     TESTING                                             
          BNE   DR33  2                                                         
*         EDIT  (B1,SCRNNUM),(1,DATPDES),ZERO=NOBLANK                           
*         MVC   DATPDES(11),=C'PF4 PRESSED'                                     
*         OI    DATPDESH+6,X'80'                                                
******    MVI   PFAID,0                                                         
*3/14     L     R6,NEWADDR                                                      
*3/14     CLI   0(R6),NPRGELQ                                                   
*3/14     BE    DR32                                                            
*         B     ENDERR                                                          
          L     R6,AIO                                                          
          LH    RE,13(R6)                                                       
          AR    RE,R6                                                           
          AH    R6,=H'985'                                                      
*                                                                               
*         LA    R6,250(R6)                                                      
*         LA    R6,250(R6)                                                      
*         LA    R6,250(R6)                                                      
*         LA    R6,235(R6)                                                      
*         AR    RE,R6                                                           
          CR    R6,RE                                                           
          BH    DR34                                                            
*                                                                               
          CLI   0(R6),NPRGELQ                                                   
          BNE   DRXXX                                                           
* 3/13                                                                          
**        MVC   DATPDES(9),=C'AIO RESET'                                        
**        OI    DATPDESH+6,X'80'                                                
*                                                                               
****      B     DR35                                                            
          B     DR40                                                            
*         B     DRX                                                             
DR32      GOTO1 CLRSCRN                                                         
          B     DR40                                                            
*         BE    DR34                                                            
*                                                                               
* ====================  PFKEY5   ================                               
DR33      DS    0H                                                              
*&&DO                                                                           
DR33      CLI   PFAID,5                                                         
          BNE   DR35                                                            
*                                                                               
*        GOTO1 CLRSCRN                                                          
*                                                                               
         MVC   NEWADDR,SVADDR   GET PREV START ELEME                            
**       GOTO1 PREVPAGE         READJUST PREV START ELEM                        
*******  GOTO1 =A(PREVPAGE),DMCB,RR=Y  READJUST PREV START ELEM                 
*3/14    L     R6,NEWADDR                                                       
**       L     RE,AIO                                                           
**       LA    R2,24(RE)                                                        
**       CR    R6,RE                                                            
**       BNH   DR35                                                             
*3/14    CLI   0(R6),NPRGELQ                                                    
*3/14    BE    DR40                                                             
*&&                                                                             
DR34     L     R6,AIO                                                           
         B     DR35                                                             
*                                                                               
*        B     DR40                                                             
*************************************************                               
DR35      DS    0H                                                              
*         MVC   DATPDES(10),=C'READ FIRST'                                      
*         OI    DATPDESH+6,X'80'                                                
          BAS   RE,GETEL                                                        
          BE    DR50                                                            
          B     DRXXX                                                           
***       DC    H'0'                                                            
DR40      TM    1(R2),X'20'         PROTECTED IS END OF PAGE                    
          BO    DRX                                                             
DR45      BAS   RE,NEXTEL                                                       
          BE    DR50                                                            
          B     DRXX                                                            
                                                                                
DR50      DS    0H                                                              
*         ST    R6,NEWADDR                                                      
          USING NPRGEL,R6                                                       
          OC    SVFILNET,SVFILNET      FILTER STATION                           
          BZ    DR55                                                            
          CLC   NPRGENET,SVFILNET                                               
          BNE   DR40                                                            
*         OC    SVPNAME,SVPNAME                                                 
*         BZ    DR55                                                            
*         CLC   NPRNTINM,SVPNAME                                                
*         BL    DR40                                                            
DR55      MVC   SNET,NPRGENET                                                   
          CLC   =C' T',SNET+3                                                   
          BNE   *+8                                                             
          MVI   SNET+4,X'40'                                                    
          OI    SNETH+6,X'80'                                                   
*                                                                               
          MVC   SNTINUM(L'NPRGENTI),NPRGENTI                                    
          OC    SNTINUM,=C'00000'                                               
          NI    SNTINUMH+1,X'FF'-X'20'                                          
          OI    SNTINUMH+6,X'80'                                                
*                                                                               
          MVC   SNTINAME,NPRNTINM                                               
          NI    SNTINAMH+1,X'FF'-X'20'                                          
          OI    SNTINAMH+6,X'80'                                                
*                                                                               
***       MVC   0(L'NPRGENET,R5),NPRGENET        SAVING INTO SCREEN             
          MVC   0(L'NPRGENET,R5),SNET     NO T   SAVING INTO SCREEN             
          LA    R5,L'NPRGENET(R5)                TABLE                          
          MVC   0(L'NPRGENTI,R5),NPRGENTI                                       
          LA    R5,L'NPRGENTI(R5)                                               
*                                                                               
DR60      LA    R2,SCRNDATQ(R2)                                                 
          B     DR40                                                            
*                                                                               
DRX       DS    0H                                                              
*                                                                               
DRXX      DS    0H                                                              
*  DRXX                                                                         
          MVC   0(2,R5),=X'FFFF'  EOT                                           
*                                                                               
          MVC   SVADDR,NEWADDR    SAVE OLD ADDRESS BEFORE OVERWRITING           
*         GOTO1 PREVPAGE          SAVE THE PREVIOUS PAGE BEFORE UPDATE          
          GOTO1 =A(PREVPAGE),DMCB,RR=Y  READJUST PREV START ELEM                
          ST    R6,NEWADDR                                                      
*  CLEAR ADD FILETERS                                                           
**        XC    SVFILBK,SVFILBK                                                 
**        XC    SVFILE,SVFILE                                                   
*                                                                               
DRXXX     DS    0H                                                              
          MVI   DISTABQ,C'N'                                                    
***       LA    R2,DATPNMH                                                      
***       NI    4(R2),X'FF'-X'0F'                                               
***       OI    4(R2),X'80'                                                     
***       MVI   PFAID,0                                                         
          B     XIT                                                             
*                                                                               
******************************************************************              
*   THIS ROUTINE UNPROTECTS NTI CODE AND NTI NAME  THATS PROTECTED              
*                                                                               
CLRPROT   NTR1                                                                  
          LA    R2,DATPGM1H                                                     
          USING SCRNDATD,R2                                                     
CLRPROT5  TM    1(R2),X'20'     NET FIELD NOT PROTECTED                         
          BO    CLRPROTX        TEST END OF SCREEN                              
*                                                                               
          NI    SNTINUMH+1,X'FF'-X'20'                                          
          NI    SNTINAMH+1,X'FF'-X'20'                                          
          LA    R2,SCRNDATQ(R2)                                                 
          B     CLRPROT5                                                        
*                                                                               
CLRPROTX  XIT1                                                                  
******************************************************************              
*  EXIT ; ADDTABLE HAS ALL THE PROGRAM NUMBERS  FILTERED FROM FILE              
******************************************************************              
*                                                                               
DRFILE    NTR1                                                                  
*                                                                               
          LA    R5,DBLOCK1                                                      
          USING DBLOCKD,R5                                                      
**        XC    DBLOCK,DBLOCK                                                   
          LA    R1,DBLOCK1                                                      
          XCEF  (R1),276                                                        
*         MVC   DBAREC,AIO3                                                     
          MVC   DBAREC,AIO3                                                     
*                                                                               
          MVI   DBFUNCT,DBVLNBK          FUNCTION                               
          MVC   DBCOMFCS,ACOMFACS         A(COMFACS),                           
          MVI   DBSELSRC,C'N'             SOURCE,                               
*         MVC   DBSELBK,=X'6306'          BOOK,                                 
          MVC   DBSELBK,SVFILBK           BOOK,                                 
*         MVI   DBBTYPE,C'A'                                                    
          MVI   DBSELMED,C'N'            MEDIA                                  
*         MVC   DBSELAGY,=C'SJ'           AGENCY CODE,                          
          MVC   DBSELAGY,AGENCY           AGENCY CODE,                          
*         MVC   DBFILE,=C'NTI'                                                  
          MVC   DBFILE,=C'NTI'                                                  
          MVC   DBSELSTA,SVFILNET                                               
          OC    SVDAY,SVDAY                                                     
          BZ    *+10                                                            
          MVC   DBSELDAY,SVDAY                                                  
*                                                                               
          OC    SVTIME,SVTIME                                                   
          BZ    *+10                                                            
          MVC   DBSELTIM,SVTIME                                                 
*                                                                               
***       BAS   RE,SETDEF                                                       
          GOTO1 DEMAND,DMCB,DBLOCK,DUMBHK,0                                     
****      GOTO1 DEMAND,DMCB,DBLOCK,0                                            
          CLI   DBERROR,0                                                       
          BE    DRFIL25                                                         
          CLI   DBERROR,X'80'                                                   
          BE    DRFIL25                                                         
***       BE    *+6                                                             
****      DC    H'0'                                                            
          B     INVNET                                                          
*                                                                               
* NOW VALIDATE THE NTI CODE                                                     
*                                                                               
DRFIL25   DS    0H                                                              
*****     BAS   RE,RSTRDEF                                                      
*         LA    R4,FILTAB                                                       
          LA    R4,ADDTAB                                                       
          LA    RE,ADDTAB                                                       
          XCEF  (RE),900                                                        
*         XCEF  (RE),2000                                                       
          MVC   0(5,R4),=X'FFFFFFFFFF'                                          
*************************************                                           
          LA    R5,DBLOCK1                                                      
          USING DBLOCKD,R5                                                      
**        XC    DBLOCK,DBLOCK                                                   
          LA    R1,DBLOCK1                                                      
          XCEF  (R1),276                                                        
          MVC   DBAREC,AIO3                                                     
*                                                                               
          MVI   DBFUNCT,DBGETNTI         FUNCTION                               
          MVC   DBCOMFCS,ACOMFACS         A(COMFACS),                           
          MVI   DBSELSRC,C'N'             SOURCE,                               
          MVC   DBSELBK,SVFILBK          BOOK,                                  
*         MVI   DBBTYPE,C'A'                                                    
          MVI   DBSELMED,C'N'            MEDIA                                  
*         MVC   DBSELAGY,=C'SJ'           AGENCY CODE,                          
          MVC   DBSELAGY,AGENCY           AGENCY CODE,                          
          MVC   DBFILE,=C'NTI'                                                  
*                                                                               
*         MVC   DBFILE,SVFILE                                                   
          MVC   DBSELSTA,SVFILNET                                               
          OC    SVDAY,SVDAY                                                     
          BZ    *+10                                                            
          MVC   DBSELDAY,SVDAY                                                  
*                                                                               
          OC    SVTIME,SVTIME                                                   
          BZ    *+10                                                            
          MVC   DBSELTIM,SVTIME                                                 
*                                                                               
*                                3 BYTES W HIGH ORDER BYTE AS NULLS             
**        MVC   DBSELPRG,HALF                                                   
****      BAS   RE,SETDEF                                                       
          MVI   HAVEDATA,C'N'                                                   
          GOTO1 DEMAND,DMCB,DBLOCK,DBFILHK,0                                    
          CLI   DBERROR,0                                                       
          BE    DRFIL28                                                         
***       CLI   DBERROR,X'80'    REACHED EOF IS FINE                            
***       BE    *+6                                                             
***       DC    H'0'                                                            
***       B     INVNTIN                                                         
DRFIL28   DS    0H                                                              
*****     BAS   RE,RSTRDEF                                                      
*                                                                               
DRFIL40   DS    0H                                                              
*                                                                               
*                                                                               
DRFILX    DS    0H                                                              
          XIT1                                                                  
*                                                                               
DBFILHK   DS    0H                                                              
          MVC   HAVEDATA,C'Y'                                                   
          ST    RE,SAVERE                                                       
* NEW                                                                           
*****     CLI   FILTABST,0                                                      
*****     BNE   DBHK50                                                          
*                                                                               
***       LA    R2,FILTAB                                                       
          LA    R2,ADDTAB                                                       
          USING FILTABD,R2                                                      
*BHK10    CLC   =X'FFFFFFFFFF',0(R2)                                            
DBHK10    OC    0(5,R2),0(R2)         NEW CODE FEB16                            
          BE    DBHK20                                                          
          LA    R2,FILTABQ(R2)                                                  
*         LA    RE,FILTAB                                                       
*         AH    RE,=H'3000'                                                     
*         CR    R2,RE                                                           
*         BNL   DBHKX                                                           
          B     DBHK10                                                          
*BHK20    EDIT  (B2,DBSELPRG),(5,(R2)),ALIGN=RIGHT                              
DBHK20    DS    0H                                                              
          GOTO1 DEFINE,DMCB,=C'NTI',DBLOCK,WORK                                 
*         MVC   0(5,R2),WORK                                                    
          MVC   FILNTI,WORK                                                     
*                                                                               
          DS    0H                                                              
          GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,WORK                             
*         MVC   SNTINAME,WORK                                                   
          MVC   FILNTIN,WORK                                                    
*         CLC   =C'20/20-SUN',WORK                                              
*         BNE   *+6                                                             
*         DC    H'0'                                                            
*                                                                               
          OC    SVPNAME,SVPNAME                                                 
          BZ    DBHK45                                                          
          CLC   SVPNAME,WORK                                                    
          BNE   DBHK60                                                          
DBHK45    DS    0H                                                              
*                                                                               
          GOTO1 DEFINE,DMCB,=C'PTYPE',DBLOCK,WORK                               
*         MVC   SNTINAME,WORK                                                   
*         MVC   FILNTIN,WORK                                                    
*                                                                               
          OC    SVPTYPE,SVPTYPE                                                 
          BZ    DBHK55                                                          
          CLC   SVPTYPE,WORK                                                    
          BNE   DBHK60                                                          
DBHK55    DS    0H                                                              
*                                                                               
*         LA    R2,L'NPRGENET(R2)                                               
          LA    R2,FILTABQ(R2)                                                  
**HK60    MVC   0(5,R2),=X'FFFFFFFFFF'                                          
DBHK60    XC    0(5,R2),0(R2)          NEW CODE   FEB16                         
*  NEW                                                                          
****50    CLI   FILTABST,0                                                      
****      BE    DBHKX                                                           
****      ZIC   RE,FILTABST                                                     
*****     BCTR  RE,0                                                            
*****     STC   RE,FILTABST                                                     
*                                                                               
DBHKX     L     RE,SAVERE                                                       
          BR    RE                                                              
                                                                                
***       XIT1                                                                  
          DROP   R2                                                             
****************************************************                            
DUMBHK    DS    0H                                                              
          ST    RE,SAVERE                                                       
          L     RE,SAVERE                                                       
          BR    RE                                                              
*                                                                               
****************************************************                            
*       DISPLAY REC TABLE                                                       
****************************************************                            
DISTAB    NTR1                                                                  
*         XC    DATPFK,DATPFK                                                   
*         MVC   DATPFK(L'DISPFLIN),DISPFLIN                                     
*         OI    DATPFKH+6,X'80'                                                 
*                                                                               
          LA    R2,DATPNMH                                                      
          LA    RE,DATPNMH                                                      
          ST    RE,ACURFORC                                                     
*   NEW                                                                         
          BAS   RE,DRFILE       FILL UP FILTABLE                                
          LA    R3,32           NUMBER OF SCREEN FIELDS                         
          SR    R2,R2                                                           
          ZIC   R5,FILPGNUM     NOW MULTIPLY BY PAGE NUM                        
          MR    R2,R5           R3 NOW POINT TO FILTABLE TO START DIS           
          LA    R5,FILTABQ                                                      
          SR    R2,R2                                                           
          MR    R2,R5                                                           
*         STC   R3,FILTABST                                                     
*                                                                               
*         BAS   RE,DRFILE       FILL UP FILTABLE                                
*                                                                               
****      LA    R4,FILTAB                                                       
          LA    R4,ADDTAB                                                       
          AR    R4,R3                                                           
*                                                                               
***       CLC   =X'FFFFFFFFFF',0(R4)    EOT                                     
          OC    0(5,R4),0(R4)    EOT    NEW FEB16                               
          BNE   DISTAB01                                                        
          MVI   FILPGNUM,0                                                      
          B     DISTX                                                           
DISTAB01  OC    0(FILTABQ,R4),0(R4)     IF NO MORE IN TABLE                     
          BNZ   DISTAB02                                                        
          MVI   FILPGNUM,0                                                      
          B     DISTX                                                           
*                                                                               
*                                                                               
DISTAB02  DS    0H                                                              
          USING FILTABD,R4                                                      
*         MVI   TWANUM,3        RES FILTABLE                                    
*         MVC   COMAND2,=C'DMREAD  '                                            
*         GOTO1 SVRESTAB                                                        
*                                                                               
*****     CLI   PFAID,9                                                         
*****     BNE   DISTAB05                                                        
*****     GOTO1 CLRFILT                                                         
*                                                                               
DISTAB05  L     R6,AIO                                                          
*                                                                               
*                                                                               
DISTAB10  LA    RE,SCRNTAB                                                      
          XCEF  (RE),340                                                        
*                                                                               
****      CLI   PFAID,0                                                         
****      BNE   DR11                                                            
          GOTO1 CLRSCRN                                                         
*                                                                               
*                                                                               
          MVC   DATPDES,SVPDES                                                  
          OI    DATPDESH+6,X'80'                                                
DISTAB11  LA    R2,DATPGM1H         1ST DATA FIELD IN SCREEN                    
          USING SCRNDATD,R2         DSECT FOR EACH SCREEN DATA                  
          LA    R5,SCRNTAB          TABLE TO STORE SCREEN                       
*************************************************************8                  
DISTAB30  DS    0H                                                              
DISTAB33  CLI   PFAID,5                                                         
          BNE   DISTAB35                                                        
DISTAB35  DS    0H                                                              
DISTAB40  TM    1(R2),X'20'         PROTECTED IS END OF PAGE                    
          BO    DISTX                                                           
**STAB45  CLC   0(5,R4),=X'FFFFFFFFFF'                                          
DISTAB45  OC    0(5,R4),0(R4)       NEW FEB16                                   
          BE    DISTXX                                                          
          BE    DISTAB50                                                        
                                                                                
DISTAB50  DS    0H                                                              
DISTAB55  MVC   SNET,SVFILNET                                                   
          CLC   =C' T',SNET+3                                                   
          BNE   *+8                                                             
          MVI   SNET+4,X'40'                                                    
          OI    SNETH+6,X'80'                                                   
*                                                                               
          MVC   SNTINUM(L'NPRGENTI),FILNTI       LOCK NTI NUM FIELD             
          OC    SNTINUM,=C'00000'                                               
          OI    SNTINUMH+1,X'20'                                                
          OI    SNTINUMH+6,X'80'                                                
*                                                LOCK NTI NAME FIELD            
          MVC   SNTINAME,FILNTIN                                                
          OI    SNTINAMH+1,X'20'                                                
          OI    SNTINAMH+6,X'80'                                                
*                                                                               
DISTAB60  LA    R2,SCRNDATQ(R2)                  NEXT FIELD ELEMENT             
          LA    R4,FILTABQ(R4)                                                  
          B     DISTAB40                                                        
*                                                                               
DISTX     DS    0H                                                              
          ST    R4,NEWADDR                                                      
*                                                                               
DISTXX    DS    0H                                                              
*  DRXX                                                                         
***       MVC   0(2,R5),=X'FFFF'  EOT                                           
*                                                                               
*         MVC   SVADDR,NEWADDR    SAVE OLD ADDRESS BEFORE OVERWRITING           
*         GOTO1 PREVPAGE          SAVE THE PREVIOUS PAGE BEFORE UPDATE          
*         GOTO1 =A(PREVPAGE),DMCB,RR=Y  READJUST PREV START ELEM                
*         ST    R6,NEWADDR                                                      
*  CLEAR ADD FILETERS                                                           
**        XC    SVFILBK,SVFILBK                                                 
**        XC    SVFILE,SVFILE                                                   
*                                                                               
DISTXXX   DS    0H                                                              
          MVI   DISTABQ,C'Y'                                                    
          XIT1                                                                  
*                                                                               
******************************************************************              
*      DISPLAY KEY                                                              
****************************************************                            
DK       DS     0H                                                              
         L      R6,AIO                                                          
         USING  NPRGRECD,R6                                                     
         XC     KEY,KEY                                                         
         MVC    KEY(L'NPRGKEY),NPRGKEY                                          
         XC     SVKEY,SVKEY                                                     
         MVC    SVKEY(L'NPRGKEY),NPRGKEY                                        
         MVC    DATPNM,NPRGKCOD                                                 
         OI     DATPNMH+6,X'80'                                                 
                                                                                
         B      XIT                                                             
         DROP   R6                                                              
****************************************************                            
XR       DS     0H                                                              
         OC    SVFILNET,SVFILNET      11/18/99   NEW CODE                       
         BZ    XR20                   11/1899                                   
         XC    CONACT,CONACT                                                    
         MVI   CONACTH+5,6                                                      
         MVC   CONACT(6),=C'CHANGE'                                             
         MVC   CONHEAD(27),=C'SELECT CODES TO ADD TO FILE'                      
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
*                                                                               
         B     VRFILE                                                           
*                                                                               
XR20     XC    SVFILBK,SVFILBK                                                  
         XC    SVFILE,SVFILE                                                    
         XC    SVFILNET,SVFILNET                                                
         XC    SVPNAME,SVPNAME                                                  
         XC    SVPDES,SVPDES                                                    
         XC    SVDAY,SVDAY                                                      
         XC    SVTIME,SVTIME                                                    
XRX      B     DR                                                               
*        B      XIT                                                             
****************************************************                            
****************************************************                            
*   LISTREC                                                                     
***************************************************                             
LR       DS     0H                                                              
*        XC     SVFILNET,SVFILNET   CLEAR FILTERS ON LIST                       
*                                                                               
         OC     KEY,KEY                                                         
         BNZ    LR10                                                            
         MVC    KEY(L'SVKEY),SVKEY                                              
*                                                                               
LR10     GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     LR30                                                            
         DC     H'0'                                                            
*                                                                               
LR20     GOTO1  SEQ                                                             
         CLI    DMCB+8,0                                                        
         BE     LR30                                                            
         DC     H'0'                                                            
*                                                                               
**30     CLC    KEY(3),KEYSAVE    RECORD TYPE AND AGY/MED                       
LR30     CLC    KEY(3),SVKEY      RECORD TYPE AND AGY/MED                       
         BNE    LRX                                                             
         GOTO1  GETREC                                                          
         L      R6,AIO                                                          
         CLC    0(3,R6),KEYSAVE    RECORD TYPE AND AGY/MED                      
         BNE    LRX                                                             
*        CLC    KEY(3),KEYSAVE    RECORD TYPE AND AGY/MED                       
***                                                                             
*                                                                               
LR60     LA     R6,KEY                                                          
         USING  NPRGRECD,R6                                                     
         MVC    LSTNAME(L'NPRGKCOD),NPRGKCOD                                    
*                                                                               
         DROP   R6                                                              
         L      R6,AIO                                                          
         XC     LSTDES,LSTDES                                                   
         MVI    ELCODE,NPDESELQ                                                 
         USING  NPDESEL,R6                                                      
         BAS    RE,GETEL                                                        
         BNE    LR220                                                           
         MVC    LSTDES,NPDES                                                    
         DROP   R6                                                              
*                                                                               
LR220    GOTO1  LISTMON                                                         
         B      LR20                                                            
         DROP   R2                                                              
*                                                                               
LRX      B      XIT                                                             
****************************************************                            
* REPORT                                                                        
PR       DS     0H                                                              
         LA     RE,HDHK                                                         
         ST     RE,HEADHOOK                                                     
         OC     KEY,KEY                                                         
         BNZ    PR10                                                            
         MVC    KEY(L'SVKEY),SVKEY                                              
*                                                                               
PR10     GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     PR30                                                            
         DC     H'0'                                                            
*                                                                               
PR20     GOTO1  SEQ                                                             
         CLI    DMCB+8,0                                                        
         BE     PR30                                                            
         DC     H'0'                                                            
*                                                                               
**30     CLC    KEY(3),KEYSAVE    RECORD TYPE AND AGY/MED                       
PR30     CLC    KEY(3),SVKEY      RECORD TYPE AND AGY/MED                       
         BNE    PRX                                                             
         GOTO1  GETREC                                                          
         L      R6,AIO                                                          
         CLC    0(3,R6),KEYSAVE    RECORD TYPE AND AGY/MED                      
         BNE    PRX                                                             
*        CLC    KEY(3),KEYSAVE    RECORD TYPE AND AGY/MED                       
*                                                                               
         LA     R6,KEY                                                          
         USING  NPRGRECD,R6                                                     
         MVC    P(L'HEADING1),HEADING1                                          
         MVC    P+11(L'NPRGKCOD),NPRGKCOD                                       
*                                                                               
         MVI    ELCODE,NPDESELQ                                                 
         L      R6,AIO                                                          
         USING  NPDESEL,R6                                                      
         BAS    RE,GETEL                                                        
         BNE    *+10                                                            
         MVC    P+L'HEADING1+3(L'NPDES),NPDES                                   
         DROP   R6                                                              
         GOTO1  SPOOL,DMCB,SPOOLD                                               
*                                                                               
         MVC    P(L'HEADING2),HEADING2                                          
         GOTO1  SPOOL,DMCB,SPOOLD                                               
*                                                                               
         MVC    P(L'HEADING3),HEADING3                                          
PR220    GOTO1  SPOOL,DMCB,SPOOLD                                               
         MVI    ELCODE,NPRGELQ                                                  
         L      R6,AIO                                                          
*  NOW PRINT OUT ELEMENTS                                                       
         BAS    RE,GETEL                                                        
         BNE    PR290                                                           
         B      PR250                                                           
PR230    BAS    RE,NEXTEL                                                       
         BNE    PR280                                                           
PR250    DS     0H                                                              
         USING  NPRGEL,R6                                                       
         CLC    =X'4040404040',PNET1                                            
         BNE    PR270                                                           
         MVC    PNET1,NPRGENET                                                  
         MVC    PNTIN1,NPRGENTI                                                 
         MVC    PNTINAM1,NPRNTINM                                               
         B      PR230                                                           
*                                                                               
PR270    DS     0H                                                              
         MVC    PNET2,NPRGENET                                                  
         MVC    PNTIN2,NPRGENTI                                                 
         MVC    PNTINAM2,NPRNTINM                                               
PR275    GOTO1  SPOOL,DMCB,SPOOLD                                               
         B      PR230                                                           
*                                                                               
*                                                                               
PR280    GOTO1  SPOOL,DMCB,SPOOLD                                               
PR290    MVC    P(80),=80C'='                                                   
*                                                                               
         GOTO1  SPOOL,DMCB,SPOOLD                                               
         B      PR20                                                            
*                                                                               
PRX      B      XIT                                                             
****************************************************                            
HDHK     DS     0H                                                              
         B      XIT                                                             
****************************************************                            
*  ROUTINE TO VALIDATE FIELDS IN FILTER SCREEN                                  
****************************************************                            
* VALIDATE FILTER FIELDS                                                        
VALFILT   NTR1                                                                  
          XC    SVFILNET,SVFILNET                                               
          MVI   BLNKSCRN,C'N'                                                   
*                                                                               
*                                                                               
VALFIL10  LA    R2,FILFILH                                                      
          MVC   FILPGNM,SVPGNAME                                                
          OI    FILPGNMH+6,X'80'                                                
          MVC   FILPDES,SVPDES                                                  
          OI    FILPDESH+6,X'80'                                                
          LA    RE,FILFILH                                                      
          ST    RE,ACURFORC                                                     
          CLI   ACTNUM,ACTDIS                                                   
          BE    VALFIL11                                                        
*                                                                               
******    CLI   5(R2),0     NEW                                                 
******    BE    MISSERR                                                         
          CLI   5(R2),0     NEW                                                 
          BNE   *+18                                                            
          MVI   BLNKSCRN,C'Y'                                                   
          XC    SVFILE,SVFILE                                                   
          B     VALFIL11                                                        
*     SO THEY CAN LEAVE AND ADD NOTHING TO NEW RECORD                           
*                                                                               
          CLC   =C'NTI',8(R2)                                                   
          BNE   *+8                                                             
          B     *+14                                                            
          CLC   =C'NHT',8(R2)                                                   
          BNE   FILEERR                                                         
          MVC   SVFILE,8(R2)                                                    
*                                                                               
*                                                                               
VALFIL11  DS    0H                                                              
*         LA    RE,FILNETH                                                      
*         ST    RE,ACURFORC                                                     
          LA    R2,FILNETH                                                      
***       CLI   5(R2),0           NEW                                           
***       BE    MISSERR                                                         
          CLI   5(R2),0           NEW                                           
          BNE   VALFIL11A                                                       
          XC    SVFILNET,SVFILNET                                               
          CLI   ACTNUM,ACTDIS                                                   
          BE    VALFIL11A                                                       
          CLI   BLNKSCRN,C'N'                                                   
          BE    MISSERR                                                         
          B     VALFIL12A                                                       
VALFIL11A MVC   SVFILNET,FILNET                                                 
*                                                                               
          CLI   5(R2),0         THIS IS BASICALLY ONLY A ACTION DISPLAY         
          BE    VALFIL12A       COMPARE                                         
*                                                                               
*         OC    SVFILNET,=X'4040404040'                                         
          OC    SVFILNET,=X'404040404040'                                       
          CLC   =X'4040',SVFILNET+3                                             
          BNE   *+12                                                            
          MVI   SVFILNET+4,C'T'                                                 
          B     VALFIL12                                                        
          CLC   =X'4040',SVFILNET+4                                             
          BNE   *+8                                                             
          MVI   SVFILNET+5,C'T'                                                 
*                                                                               
*   NEW CODE TO CONVERT STATION                                                 
VALFIL12  XC    DMCB,DMCB                                                       
          MVC   DMCB(L'SVFILNET),SVFILNET                                       
          GOTO1 CNVSTA,DMCB                                                     
          OC    DMCB,DMCB                                                       
          BZ    INVNET                                                          
          XC    SVFILNET,SVFILNET                                               
          MVC   SVFILNET,DMCB+8                                                 
*    ACTION DISPLAY NEED ONLY VALIDATE NETWORK                                  
VALFIL12A CLI   ACTNUM,ACTDIS                                                   
          BE    VALFIL40                                                        
*                                                                               
*                                                                               
*                                                                               
*ALFIL12A DS    0H                                                              
          LA    R2,FILBKSH                                                      
*         LA    RE,FILBKSH                                                      
*         ST    RE,ACURFORC                                                     
*****8    CLI   5(R2),0      NEW                                                
*****     BE    MISSERR                                                         
          CLI   5(R2),0           NEW                                           
          BNE   *+22                                                            
          XC    SVFILBK,SVFILBK                                                 
          CLI   BLNKSCRN,C'N'                                                   
          BE    MISSERR                                                         
          B     VALFIL14                                                        
*                                                                               
**        LA    RE,FILBKSH                                                      
**        ST    RE,ACURFORC                                                     
          CLC   =C'NTI',SVFILE                                                  
          BE    VALFIL13                                                        
          GOTO1 DATVAL,DMCB,(2,8(R2)),DATETEMP                                  
*                                                                               
          CLC   DATETEMP,=C'000000'                                             
          BE    BOOKERR                                                         
*                                                                               
          GOTO1 DATCON,DMCB,(0,DATETEMP),(3,ELEM)                               
          MVC   SVFILBK,ELEM                                                    
          B     VALFIL14                                                        
*  FILE NHT IS WEEKLY SO CALL NETWEEK                                           
VALFIL13  GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                  
*                                                                               
          CLC   DATETEMP,=C'000000'                                             
          BE    BOOKERR                                                         
          GOTO1 NETWEEKS,DMCB,DATETEMP,VGETDAY,VADDAY                           
          MVC   SVFILBK+1(1),DMCB          WEEK                                 
          MVC   SVFILBK(1),DMCB+4      YEAR                                     
*                                                                               
*                                                                               
* OPTIONAL FIELDS                                                               
*                                                                               
VALFIL14  DS    0H                                                              
          LA    R2,FILPNMH      PROGRAM NAME                                    
*         LA    RE,FILPNMH                                                      
*         ST    RE,ACURFORC                                                     
          CLI   5(R2),0                                                         
          BNE   *+14                                                            
          XC    SVPNAME,SVPNAME                                                 
          B     VALFIL15                                                        
          MVC   SVPNAME,8(R2)                                                   
          OC    SVPNAME,=16X'40'                                                
*                                                                               
VALFIL15  LA    R2,FILTYPH                                                      
*         LA    RE,FILTYPH                                                      
*         ST    RE,ACURFORC                                                     
          CLI   5(R2),0                                                         
          BNE   *+14                                                            
          XC    SVPTYPE,SVPTYPE                                                 
          B     VALFIL20                                                        
          MVC   SVPTYPE,8(R2)                                                   
          OC    SVPTYPE,=X'4040'                                                
*                                                                               
VALFIL20  DS    0H                                                              
          LA    R2,FILDAYH                                                      
*         LA    RE,FILDAYH                                                      
*         ST    RE,ACURFORC                                                     
          CLI   5(R2),0                                                         
          BE    VALFIL30                                                        
*&&DO                                                                           
          LA    RE,DAYTAB                                                       
VALFIL21  CLC   =X'FFFF',0(RE)                                                  
          BE    DAYERR                                                          
VALFIL22  CLC   0(3,RE),FILDAY                                                  
          BE    VALFIL25                                                        
          LA    RE,L'DAYTAB(RE)                                                 
          B     VALFIL21                                                        
VALFIL25  MVC   SVDAY,3(RE)                                                     
*&&                                                                             
          XC    DMCB,DMCB                                                       
          LA    RE,FILDAY                                                       
          ST    RE,DMCB       A(INPUT)                                          
          MVC   DMCB(1),5(R2)    LENGTH OF INPUT                                
          LA    RE,SVDAY                                                        
          ST    RE,DMCB+4     A(OUTPUT)                                         
          LA    RE,BYTE                                                         
          ST    RE,DMCB+8     A(OUTPUT)                                         
          GOTO1 DAYVAL,DMCB                                                     
*         GOTO1 DAYPAK,DMCB                                                     
          CLI   SVDAY,0                                                         
          BE    DAYERR                                                          
*                                                                               
                                                                                
*                                                                               
VALFIL30  DS    0H                                                              
*                                                                               
          LA    R2,FILTIMH                                                      
*         LA    RE,FILTIMH                                                      
*         ST    RE,ACURFORC                                                     
          CLI   5(R2),0                                                         
          BNE   *+14                                                            
          XC    SVTIME,SVTIME                                                   
          B     VALFIL35                                                        
          LA    RE,8(R2)                                                        
          ST    RE,DMCB                                                         
          MVC   DMCB(1),5(R2)                                                   
          GOTO1 TIMVAL,DMCB,,SVTIME                                             
          CLI   DMCB,X'FF'                                                      
          BE    TIMEERR                                                         
*  VALIDATE THAT NET ENTERED IS A VALID NET                                     
VALFIL35  DS    0H                                                              
          OC    SVTIME,SVTIME         IF TIME SPECIFIED AND DAY ISNT            
          BZ    VALFIL36              IT IS ALL DAY                             
          OC    SVDAY,SVDAY                                                     
          BNZ   VALFIL36                                                        
          MVI   SVDAY,X'7F'                                                     
*                                                                               
VALFIL36  LA    R5,DBLOCK1                                                      
          USING DBLOCKD,R5                                                      
*                                                                               
          MVI   DBFUNCT,DBVLNBK          FUNCTION                               
***8      XC    DBLOCK,DBLOCK                                                   
          LA    R1,DBLOCK                                                       
          XCEF  (R1),276                                                        
          MVC   DBAREC,AIO3                                                     
*                                                                               
          MVI   DBFUNCT,DBVLNBK          FUNCTION                               
          MVC   DBCOMFCS,ACOMFACS         A(COMFACS),                           
          MVI   DBSELSRC,C'N'             SOURCE,                               
          MVC   DBSELBK,SVFILBK           BOOK,                                 
*         MVI   DBBTYPE,C'A'                                                    
          MVI   DBSELMED,C'N'            MEDIA                                  
*         MVC   DBSELAGY,=C'SJ'           AGENCY CODE,                          
          MVC   DBSELAGY,AGENCY           AGENCY CODE,                          
          MVC   DBFILE,=C'NTI'                                                  
*         MVC   DBFILE,SVFILE                                                   
          MVC   DBSELSTA,SVFILNET                                               
          OC    SVDAY,SVDAY                                                     
          BZ    *+10                                                            
          MVC   DBSELDAY,SVDAY                                                  
*                                                                               
          OC    SVTIME,SVTIME                                                   
          BZ    *+10                                                            
          MVC   DBSELTIM,SVTIME                                                 
*                                                                               
***       BAS   RE,SETDEF                                                       
          GOTO1 DEMAND,DMCB,DBLOCK,DUMBHK,0                                     
***       GOTO1 DEMAND,DMCB,DBLOCK,0                                            
          CLI   DBERROR,0                                                       
          BE    VALFIL40                                                        
          CLI   DBERROR,X'80'                                                   
          BE    VALFIL40                                                        
*         BE    *+6                                                             
*         DC    H'0'                                                            
          LA    R2,FILNETH                                                      
          LA    RE,FILNETH                                                      
          ST    RE,ACURFORC                                                     
          B     INVNET                                                          
*         DC    H'0'                                                            
*                                                                               
VALFIL40  DS    0H                                                              
***       BAS   RE,RSTRDEF                                                      
*                                                                               
***********************************************                                 
          CLI   PFAID,8                                                         
          BE    *+12                                                            
          MVI   SCRNNUM,2                                                       
          B     PF8ERR                                                          
**********************************88                                            
*                                                                               
**LFIL60 CLI    ACTNUM,ACTADD                                                   
**       BNE    VALFIL70                                                        
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    VALFIL55                                                         
*   ACTION CHANGE, COMPARE TO SEE IF BLANK SCREEN IF SO GO BACK                 
*   TO MAINSCREEN WITHOUT LOOKING AT FILE                                       
         CLI   BLNKSCRN,C'Y'                                                    
         BE    VALFIL55                                                         
*                                                                               
         BAS   RE,DRFILE       GET ADD TABLE FROM FILE                          
         CLI   HAVEDATA,C'N'                                                    
         BNE   *+16                                                             
         LA    RE,FILFILH                                                       
         ST    RE,ACURFORC                                                      
         B     INVREQ                                                           
*                                                                               
*  RELOAD MAINT SCREEN                                                          
VALFIL55 MVI   TWANUM,3            SAVE CURRENT FIL SCREEN                      
         MVC   COMAND2,=C'DMWRT   '                                             
         GOTO1 TWAIO                                                            
* NEW                                                                           
*                                                                               
*ALFIL55 DS     0H                                                              
***      GOTO1 MAINSCRN                                                         
         GOTO1 CALLOV,DMCB,(X'CF',CONTAGH),0,0                                  
         MVI   SCRNNUM,1      NEW                                               
         MVC   DATPNM,SVPGNAME                                                  
         MVC   DATPDES,FILPDES                                                  
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    VALFIL80                                                         
*                                                                               
***      BAS   RE,DRFILE       GET ADD TABLE FROM FILE                          
*        MVI   TWANUM,3            SAVE CURRENT TABLEIN TWA3                    
*        MVC   COMAND2,=CL8'DMWRT   '                                           
*        GOTO1 SVRESTAB                                                         
         B     VALFIL80                                                         
*ALFIL70 GOTO1 MAINSCRN                                                         
*        GOTO1 CLRSCRN                                                          
VALFIL80 MVI   SCRNNUM,1                                                        
         LA    R4,CONKEYH                                                       
VALFIL90 ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   VALFIL90                                                         
         MVC   1(2,R4),=X'0101'                                                 
*        LA    RE,ADDTAB          CLEAR ADDTABLE FOR VRFILE                     
*        XCEF  (RE),900                                                         
*        LA    R4,ADDTAB                                                        
*        MVC   0(5,R4),=X'FFFFFFFFFF'                                           
*                                                                               
*                                                                               
VALFILX  XIT1                                                                   
          DROP  R5                                                              
                                                                                
****************************************************                            
****************************************************                            
NEXTSCRN NTR1                                                                   
         MVI   SCRNNUM,2                                                        
         MVI   TWANUM,2            SAVE CURRENT SCRN IN TWA2                    
         MVC   COMAND2,=CL8'DMWRT   '                                           
         GOTO1 TWAIO                                                            
*        GOTO1 CALLOV,DMCB,(X'A5',CONTAGH),0,0   LOAD NEW SCREEN                
         GOTO1 CALLOV,DMCB,(X'CF',CONTAGH),0,0                                  
         EJECT                                                                  
NEXTX    XIT1                                                                   
****************************************************                            
*  LOAD NEW FILTER SCREEN                                                       
FILSCRN  NTR1                                                                   
         MVI   SCRNNUM,2                                                        
         MVI   TWANUM,2            SAVE CURRENT SCRN IN TWA2                    
         MVC   COMAND2,=CL8'DMWRT   '                                           
         GOTO1 TWAIO                                                            
         MVC   SVACTION,CONACT                                                  
*                                                                               
* NEW                                                                           
         CLI   ACTNUM,ACTDIS                                                    
         BNE   FILS05                                                           
         GOTO1 CALLOV,DMCB,(X'C0',CONTAGH),0,0                                  
         MVI   FILSFLAG,C'N'                                                    
         GOTO1 CLRFILT                                                          
         B     FILX                                                             
FILS05   CLI   FILSFLAG,C'Y'                                                    
         BNE   FILS30                                                           
         MVI   TWANUM,3            RESTORE OLD MAINT SCREEN                     
         MVC   COMAND2,=C'DMREAD  '                                             
         GOTO1 TWAIO                                                            
         LA    R4,CONKEYH                                                       
         MVC   CONACT,SVACTION                                                  
FILS10   ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   FILS10                                                           
         MVC   1(2,R4),=X'0101'                                                 
         B     FILX                                                             
*                                                                               
*        GOTO1 CALLOV,DMCB,(X'A5',CONTAGH),0,0   LOAD NEW SCREEN                
FILS30   GOTO1 CALLOV,DMCB,(X'C0',CONTAGH),0,0                                  
         MVI   FILSFLAG,C'Y'                                                    
         XC    SVFILNET,SVFILNET                                                
         EJECT                                                                  
FILX     XIT1                                                                   
****************************************************                            
*  ROUTINE TO DELETE A CERTAIN ELEMENT                                          
* ENTRY : DMCB+0 FULL WORD INDEX TO WHICH ELEMENT TO DEL                        
* EXIT  : ELEMENT CODE MARKED WITH X'FF' FOR DEL                                
****************************************************                            
DELELEM  NTR1                                                                   
         L     R6,AIO                                                           
         L     R3,DMCB       INDEX                                              
         SR    R2,R2                                                            
         LA    R5,SCRNLQ     LENGTH OF SCREEN TABLE ENTRY                       
         MR    R2,R5                                                            
         LA    R5,SCRNTAB                                                       
         AR    R5,R3         R5 POINTS THE SCRN TABLE REC                       
         USING SCRNTABD,R5                                                      
         MVI   ELCODE,NPRGELQ                                                   
         BAS   RE,GETEL                                                         
         BE    DEL20                                                            
****     DC    H'0'          MUST FIND IT                                       
         B     DELX                                                             
DEL10    BAS   RE,NEXTEL                                                        
         BE    DEL20                                                            
****     DC    H'0'          MUST FIND IT TO DEL                                
         B     DELX                                                             
         USING NPRGEL,R6                                                        
*                                                                               
*                                                                               
DEL20    DS    0H                                                               
         CLC   NPRGENET+3(2),=C' T'                                             
         BNE   DEL25                                                            
         CLC   SCRNNET(3),NPRGENET    IF IT IS A STATION W C' T'                
         BNE   DEL10                  IT MUST BE STORED AS SPACE THE            
         CLC   SCRNNET+3(2),=X'4040'  LAST 2 BYTES IN TABLE ENTRY               
         BNE   DEL10                                                            
         B     DEL30                                                            
DEL25    CLC   SCRNNET,NPRGENET                                                 
         BNE   DEL10                                                            
DEL30    CLC   SCRNNTI,NPRGENTI                                                 
         BNE   DEL10                                                            
         MVI   NPRGEL,X'FF'                                                     
DELX     XIT1                                                                   
****************************************************                            
****************************************************                            
*=================== TWAIO- SAVE/RESTORE TWA FIELD ===================*         
*                                                                               
TWAIO    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         ZIC   R3,TWANUM                                                        
         SLL   R3,32-8                                                          
         ICM   R3,3,TRMNUM                                                      
         LH    R4,=H'2400'                                                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'0960'        READ 2400 BYTES                       
         GOTO1 DATAMGR,DMCB,COMAND2,=C'TEMPSTR',(R3),ATWA                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
TWAIOX   XIT1                                                                   
         EJECT                                                                  
****************************************************                            
*  LOAD BACK MAINTAINECE SCRREN                                                 
MAINSCRN NTR1                                                                   
*                                                                               
         MVI   TWANUM,3            RESTORE OLD MAINT SCREEN                     
         MVC   COMAND2,=C'DMWRT   '                                             
         GOTO1 TWAIO                                                            
*                                                                               
         MVI   TWANUM,2            RESTORE OLD MAINT SCREEN                     
         MVC   COMAND2,=C'DMREAD  '                                             
         GOTO1 TWAIO                                                            
*        GOTO1 VSUBR01,DMCB,('TWAIE',(R9)),(RC)                                 
*        GOTO1  CALLOV,DMCB,(X'CF',CONTAGH),0,0                                 
         XIT1                                                                   
****************************************************                            
****************************************************                            
*=================== TWAIO- SAVE/RESTORE TWA FIELD ===================*         
*                                                                               
SVRESTAB NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         ZIC   R3,TWANUM                                                        
         SLL   R3,32-8                                                          
         ICM   R3,3,TERM                                                        
         LH    R4,=H'3000'                                                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'0BB8'        READ 2400 BYTES                       
         LA    RE,ADDTAB                                                        
         ST    RE,DMCB+12                                                       
         GOTO1 DATAMGR,DMCB,COMAND2,=C'TEMPSTR',(R3),,0                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
SVRESX   XIT1                                                                   
         EJECT                                                                  
****************************************************                            
CLRSCRN   NTR1                                                                  
          LA    R2,DATPGM1H         1ST DATA FIELD IN SCREEN                    
          USING SCRNDATD,R2         DSECT FOR EACH SCREEN DATA                  
*         XC    DATPDES,DATPDES                                                 
*         OI    DATPDESH+6,X'80'                                                
CLR10     TM    1(R2),X'20'                                                     
          BO    CLRX                                                            
          XC    SNET,SNET                                                       
          OI    SNETH+6,X'80'                                                   
          XC    SNTINUM,SNTINUM                                                 
          OI    SNTINUMH+6,X'80'                                                
          XC    SNTINAME,SNTINAME                                               
          OI    SNTINAMH+6,X'80'                                                
          LA    R2,SCRNDATQ(R2)                                                 
          B     CLR10                                                           
CLRX      XIT1                                                                  
*                                                                               
                                                                                
**************************************************************                  
CLRFILT   NTR1                                                                  
*                                                                               
          XC    SVFILE,SVFILE                                                   
          XC    SVFILNET,SVFILNET                                               
          XC    SVFILBK,SVFILBK                                                 
          XC    SVPNAME,SVPNAME                                                 
          XC    SVDAY,SVDAY                                                     
          XC    SVTIME,SVTIME                                                   
          XIT1                                                                  
**************************************************************                  
* ROUTINE TURNS STATION ENTERED INTO DBSELSTA INTERNAL FORMAT                   
*  ON ENTRY DMCB+0  HAS FIVE BYTES STATION                                      
*  ON EXIT  DMCB+8  HAS FIVE BYTE INTERNAL FORM                                 
*           IF FORMAT IS INVALID, DMCB WILL HAVE NULLS                          
**************************************************************                  
CNVSTA    NTR1                                                                  
*                                                                               
          LA   R1,DMCB                                                          
          LA   R3,DMCB+8                                                        
          XC   DMCB+8(5),DMCB+8                                                 
CNVSTA10  DS   0H                                                               
          LA   R2,DMCB+4                                                        
          CR   R1,R2                                                            
          BNE  CNVSTA15                                                         
          MVI  DMCB+12,X'40'                                                    
          B    CNVEXIT                                                          
CNVSTA15  CLI  0(R1),X'40'         SPACE ?                                      
          BE   CNVSTA30                                                         
          CLI  0(R1),C'-'          DASH  ?                                      
          BE   CNVSTA50                                                         
          MVC  0(1,R3),0(R1)                                                    
          LA   R1,1(R1)                                                         
          LA   R3,1(R3)                                                         
          B    CNVSTA10                                                         
* IF WE FIND THE SEPERATOR, NEXT BYTE MUST BE A CHAR OR SPACES ONLY             
CNVSTA30  DS   0H        SPACE SEPERATOR                                        
          LA   R1,1(R1)                                                         
          CLI  0(R1),X'40'      SPACE AFTER SPACE SEPERATOR                     
          BE   CNVEXIT          JUST EXIT- DMCB SHOULD BE DONE                  
          MVI  DMCB+11,X'40'                                                    
          MVC  DMCB+12,0(R1)                                                    
          OC   DMCB+8(L'DBSELSTA),=X'4040404040'                                
          B    CNVEXIT                                                          
*                                                                               
CNVSTA50  DS   0H        DASH  SEPERATOR                                        
          LA   R1,1(R1)                                                         
          CLI  0(R1),X'40'    CAN'T BE SPACE AFTER DASH                         
          BE   CNVINV         INVALID                                           
          MVI  DMCB+11,X'40'                                                    
          MVC  DMCB+12,0(R1)                                                    
          OC   DMCB+8(L'DBSELSTA),=X'4040404040'                                
          B    CNVEXIT                                                          
*                                                                               
CNVINV    DS   0H                                                               
          XC   DMCB,DMCB                                                        
          B    CNVEXIT                                                          
*                                                                               
CNVEXIT   XIT1                                                                  
*                                                                               
**************************************************************                  
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
*        MVC   MYUSEIO,USEIO                                                    
*        MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SETDEF   MVC   SYSDIR,=C'NET     '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'NET     '                                              
*        MVI   USEIO,C'Y'                                                       
*        MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'15'              SET LENGTH OF STATION KEY               
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
*        MVC   USEIO,MYUSEIO                                                    
*        MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
*                                                                               
**************************************************************                  
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DEL ALLOWED                               
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
**************************************************************                  
*                                                                               
         EJECT                                                                  
         DROP  R4                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
ADDMSG   DC    CL46'ENTER NTI CODES OR HIT PF2 FOR REQUEST SCREEN'              
ADDPFLIN DC    CL57'PF2 REQUEST NTI #S PF4 NEXT PAGE  PF10 ADD NTI #S T+        
               O FILE'                                                          
DISPFLIN DC    CL70'PF2 REQUEST NTI #S PF4 NEXT PAGE  PF5 PREV PAGE  PF+        
               9 CLEAR ALL FILTERS'                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX2                                                           
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
*RAPERR  GOTO1 ERREX                                                            
*                                                                               
*                                                                               
DAYERR    XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(14),=C'INVALID DAY(S)'                                  
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
TIMEERR   XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(15),=C'INVALID TIME(S)'                                 
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
FILEERR   XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(22),=C'INVALID FILE (NTI,NHT)'                          
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
FILTERR   XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(30),=C'MUST ENTER FILE, STATION, BOOK'                  
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
*NDERR    XC    CONHEAD,CONHEAD                                                 
*         MVC   CONHEAD(23),=C'NO MORE DATA TO DISPLAY'                         
*         MVI   ERROR,0                                                         
*         GOTO1 ERREX2                                                          
*                                                                               
BOOKERR   XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(12),=C'INVALID BOOK'                                    
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
*F10ERR   XC    CONHEAD,CONHEAD                                                 
*         MVC   CONHEAD(20),=C'PRESS PF10 TO RETURN'                            
PF10ERR   MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
*F8ERR    XC    CONHEAD,CONHEAD                                                 
*         MVC   CONHEAD(19),=C'PRESS PF8 TO RETURN'                             
*         MVI   ERROR,0                                                         
*         GOTO1 ERREX2                                                          
*                                                                               
PF8ERR    XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(52),=C'ENTER REQUEST AND THEN PRESS PF8 TO REPO+        
               RT NTI CODES'                                                    
*              CODES                                                            
          CLI   ACTNUM,ACTDIS                                                   
          BNE   *+10                                                            
          MVC   CONHEAD(52),=C'ENTER FILTERS AND THEN PRESS PF8 TO REPO+        
               RT NTI CODES'                                                    
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
*UPERR    XC    CONHEAD,CONHEAD                                                 
*         MVC   CONHEAD(19),=C'ENTRY ALREADY EXIST'                             
*         MVI   ERROR,0                                                         
*         GOTO1 ERREX2                                                          
*                                                                               
INVNET    XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(15),=C'INVALID STATION'                                 
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
INVNTIN   XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(18),=C'INVALID NTI NUMBER'                              
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
INVREQ    XC    CONHEAD,CONHEAD                                                 
          MVC   CONHEAD(35),=C'NO DATA FOR REQUEST/INVALID REQUEST'             
          MVI   ERROR,0                                                         
          GOTO1 ERREX2                                                          
*                                                                               
*          DATA SET SPSFM28    AT LEVEL 078 AS OF 10/25/99                      
***********************************************************************         
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
*                                                                               
         SSPEC H4,40,C'PROGRAM GROUP LISITNG'                                   
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
HEADING1 DC    CL36'CODE                     DESCRIPTION'                       
HEADING2 DC    CL71'NET      NTI #     NTINAME                   NET   +        
                  NTI #     NTINAME'                                            
HEADING3 DC    CL80'-----    -----     ----------------          ----- +        
                  -----     ----------------'                                   
***********************************************************************         
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
DBLOCK1  DS    CL256                                                            
         DS    XL15                RESERVE                                      
ADDTAB   DS    XL3000                                                           
*        DS    XL10                RESERVE                                      
         DROP  RB                                                               
         PRINT OFF                                                              
*****************************************************                           
PREVPAGE  NTR1  BASE=*,LABEL=*                                                  
          LA    RE,DATPGM1H                                                     
          LA    R1,DATPGM2H                                                     
          SR    R1,RE               R1 HAS LENGTH OF EACH SCREEN                
*                                   ELEMENT (2ND SCRNFIELD - 1ST)               
          LA    RF,DATENDF                                                      
          LA    RE,DATPGM1                                                      
          SR    RF,RE                                                           
          SR    RE,RE                                                           
          DR    RE,R1                                                           
          LA    RF,1(RF)            NOW RF HAS THE NUMBER OF SCREEN             
*                                   ELEMENTS ON THE SCREEN                      
*  NOW MULTIPLY RF BY LENGTH OF ELEMENT AND SUBTRACT FROM NEWADDR               
*                                                                               
         MVC   NEWADDR,SVADDR                                                   
*                                                                               
         LA    R1,NPRGELNQ           LENGTH OF ELEMENTS                         
         SR    RE,RE                                                            
         MR    RE,R1                                                            
         L     R6,NEWADDR           PREV PAGE                                   
*        L     R6,SVADDR                                                        
         SR    R6,RF                                                            
         ST    R6,SVADDR                                                        
*        L     R6,SVADDR                                                        
*                                                                               
*        ST    R6,SVADDR                                                        
*        L     R6,NEWADDR                                                       
         L     RE,AIO                                                           
         LA    RE,24(RE)                                                        
         L     RF,SVADDR                                                        
         CR    RF,RE                                                            
         BH    PREVX                                                            
         L     R6,AIO                                                           
         ST    R6,SVADDR                                                        
*                                                                               
*                                                                               
PREVX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
****************************************************                            
* CHECK DUPLICATE ELEMENT.                                                      
* ENTRY : SVFIELD CONTAINS SNET+SNTINUM                                         
* EXIT :  CC SET EQUAL IF DUPLICATE FOUND                                       
*         CC SET NMOT EQUAL IF NOT FOUND                                        
****************************************************                            
DUPELEM  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
                                                                                
         BAS   RE,GETEL                                                         
         BE    DUP20                                                            
****     DC    H'0'                                                             
DUP10    BAS   RE,NEXTEL                                                        
         BE    *+8                                                              
         B     DUPNO                                                            
         USING NPRGEL,R6                                                        
DUP20    CLC   SVSNET,NPRGENET                                                  
         BNE   DUP10                                                            
         CLC   SVSNTINM,NPRGENTI                                                
         BNE   DUP10                                                            
DUPYES   CR    RB,RB                                                            
         B     DUPX                                                             
DUPNO    CR    RB,R5                                                            
DUPX     XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
****************************************************                            
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE NESFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMCFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC6D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC0D                                                       
         PRINT OFF                                                              
       ++INCLUDE NESFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
PERCENT  DS    F                   SUM OF PERCENTAGES                           
OPTNUM   DS    X                   SCANNER ENTRY NUMBER                         
REPCODE  DS    CL3                 EBCDIC REP CODE                              
BREP     DS    H                   BINARY REP CODE                              
SAVEKEY  DS    XL48                                                             
FAKEFLD  DS    XL9                                                              
TWANUM   DS    X                   FOR SAVING/RESTORING TWAS                    
TRMNUM   DS    XL2                                                              
COMAND2  DS    CL8                                                              
SVBNTI   DS    XL3                                                              
DEMAND   DS    A                                                                
VGETDAY  DS    A                                                                
VADDAY   DS    A                                                                
VTIMVAL  DS    A                                                                
DEFINE   DS    A                                                                
NETWEEKS DS    A                                                                
DAYPAK   DS    A                                                                
SAVERE   DS    A                                                                
SVFIELD  DS    0CL(L'SNET+L'SNTINUM)                                            
SVSNET   DS    CL(L'SNET)                                                       
SVSNTINM DS    CL(L'SNTINUM)                                                    
SVFILBK  DS    XL2                                                              
SVFILE   DS    CL3                                                              
SCRNNUM  DS    X                                                                
NEWADDR  DS    A                                                                
SVADDR   DS    A                                                                
SVPGNAME DS    CL(L'DATPNM)                                                     
SVPDES   DS    CL(L'FILPDES)                                                    
SVPNAME  DS    CL(L'FILPNM)                                                     
SVPTYPE  DS    CL(L'FILTYP)                                                     
SVFILNET DS    CL(L'FILNET)                                                     
SVTIME   DS    CL(L'FILTIM)                                                     
SVACTION DS    CL(L'CONACT)                                                     
SVDAY    DS    X                                                                
HAVEDATA DS    C                                                                
FILSFLAG DS    C                                                                
FILPGNUM DS    X                                                                
FILTABST DS    X                                                                
DISTABQ  DS    C                                                                
VKFLAG   DS    C                                                                
BLNKSCRN DS    C                                                                
DATETEMP DS    CL6                                                              
SCRNTAB  DS    32CL10             ****DO NOT INSERT FIELD BETWEEN TABLE         
         DS    CL20               *SPARE                                        
SCRNTABQ EQU   *-SCRNTAB          **** AND EQUATE !!!!!!!!!!!!!!!!!!!!!         
*DDTAB   DS    XL900                                                            
*DDTAB   DS    XL3000                                                           
*                                                                               
* SCREENS EQUATES                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENPRG                                                       
         EJECT                                                                  
PSRRECD  DSECT                                                                  
       ++INCLUDE SPGENPSR                                                       
         SPACE 5                                                                
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTNAME  DS    CL8                                                              
         DS    CL12                                                             
LSTDES   DS    CL20                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PNET1    DS    CL5                                                              
         DS    CL4                                                              
PNTIN1   DS    CL5                                                              
         DS    CL5                                                              
PNTINAM1 DS    CL16                                                             
         DS    CL10                                                             
PNET2    DS    CL5                                                              
         DS    CL4                                                              
PNTIN2   DS    CL5                                                              
         DS    CL5                                                              
PNTINAM2 DS    CL16                                                             
*                                                                               
*                                                                               
         SPACE 3                                                                
SCRNDATD DSECT           DSECT FOR DATA IN MAINT SCREEN                         
SNETH    DS    CL8     HEADER                                                   
SNET     DS    CL5                                                              
SNTINUMH DS    CL8     HEADER                                                   
SNTINUM  DS    CL5                                                              
SNTINAMH DS    CL8     HEADER                                                   
SNTINAME DS    CL16                                                             
SCRNDATQ EQU   *-SNETH                                                          
         SPACE 3                                                                
ADDTABD  DSECT                                                                  
ADDNET   DS    CL5                                                              
ADDNTI   DS    CL5                                                              
ADDNTINM DS    CL16                                                             
ADDTABQ  EQU   *-ADDNET                                                         
*                                                                               
FILTABD   DSECT                                                                 
FILNTI   DS    CL5                                                              
FILNTIN  DS    CL16                                                             
FILTABQ  EQU   *-FILNTI                                                         
*                                                                               
SCRNTABD DSECT                                                                  
SCRNNET  DS    CL5                                                              
SCRNNTI  DS    CL5                                                              
SCRNLQ   EQU   *-SCRNNET                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'132NESFM36   10/31/05'                                      
         END                                                                    
