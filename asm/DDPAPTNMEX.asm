*          DATA SET DDPAPTNMEX AT LEVEL 001 AS OF 07/08/08                      
*PHASE PAPTNMXA                                                                 
APAS0200 TITLE 'PANAPT: NO INVENTORY MEMBER EXISTENCE EXIT'                     
*                                                                     *         
* *** DEIS: THIS WAS COPIED FROM 'PANAPT.DDS.SRCELIB(APAS0200)' .     *         
* *** THIS PANAPT MEMBER EXISTENCE EXIT IS INTENDED TO BE USED WITH   *         
* ***  LIBCODES THAT HAVE NO INVENTORY ASSOCIATED WITH THEM           *         
* ***  (E.G., WARN/BAK)                                               *         
*                                                                     *         
* NAME       : APAS0200                                               *         
* PRODUCT    : PANAPT                                                 *         
* TYPE       : ASSEMBLER SOURCE PROGRAM                               *         
*                                                                     *         
* ENTRY COND.                                                         *         
*                                                                     *         
*    LINKAGE : STANDARD OS LINKAGE                                    *         
*    PARMS   : (USAGE IS IN=INPUT, OUT=OUTPUT, MOD=MODIFIED)          *         
*              (TYPE IS STRUCTURE = A COLLECTION OF RELATED DATA      *         
*               FIELDS USED TOGETHER AS A DATA STRUCTURE)             *         
*                                                                     *         
*      PARAMETER  USAGE  TYPE      DESCRIPTION                        *         
*      ---------  -----  --------  ---------------------------------- *         
*      APAM02XX    MOD   STRUCTURE MEMBER EXISTENCE PARAMETER BLOCK   *         
*      APAMDIB2    IN    STRUCTURE MEMBER INVENTORY PARAMETER BLOCK   *         
*      APAMLIB2    IN    STRUCTURE LIBRARY CODE RECORD         013A*GRS         
*      APAMMMBR    IN    STRUCTURE PENDING FILE RECORD         013A*GRS         
*      APAMMDES    IN    STRUCTURE MEMBER DESCRIPTION RECORD   013A*GRS         
*                                                                     *         
* EXIT COND.                                                          *         
*                                                                     *         
*    RETURN  :  0 <===> MEMBER FOUND, THE MOVE REQUEST MAY BE CLOSED. *         
*    CODES   :          THE VERIFY FLAG WILL BE CLEARED.              *         
*                       - OR FOR PURGE, PURGE SUCCESSFUL              *         
*                                                                     *         
*               4 <===> MEMBER NOT FOUND, THE MOVE REQUEST MAY NOT BE *         
*                       CLOSED. THE VERIFY FLAG WILL REMAIN SET TO    *         
*                       "V" INDICATING VERIFICATION IS STILL REQUIRED *         
*                       - OR FOR PURGE, PURGE UNSUCCESSFUL            *         
*                                                                     *         
*               8 <===> INDICATES NO PROCESSING WAS DONE BY THE EXIT. *         
*                       THE VERIFY FLAG WILL NOT BE MODIFIED.         *         
*                                                                     *         
*              12 <===> USER PROVIDED DATA ERRORS OR                  *         
*                       ALLOCATION/DEALLOCATION ERROR OR              *         
*                       BLDL PROCESSING ERRORS                        *         
*                                                                     *         
* EXTERNAL                                                            *         
* CALLS      : NONE.                                                  *         
*                                                                     *         
* COPYBOOKS  : NONE.                                                  *         
*                                                                     *         
* MACROS     : IEFZB4D0, IEFZB4D2, DYNALLOC.                          *         
*                                                                     *         
* COMMENTS   : NOTE:   UPON ENTRY TO THIS MODULE, PANAPT WILL SET THE *         
*                      VERIFY FLAG TO "V" FOR ADDS AND MAKECOPY.      *         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        P A R A M E T E R S   F R O M   C A L L E R                            
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
APAM02XX DSECT                                                                  
         COPY  APAM02XX                                                         
APAMDIB2 DSECT                                                                  
         COPY  APAMDIB2                                                         
         COPY  APAMLIB2                                                         
APAMMMBR DSECT                                                                  
         APAMMMBR                                                               
         APAMMDES                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        P R O C E S S I N G   V E R I F I C A T I O N   T A B L E              
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
         DSECT                     PROCESSING CRITERIA                          
PROCESS  DS    0CL3                                                             
VERFLG   DS    CL1                   VERIFY FLAG                                
SOURCE   DS    CL1                   SOURCE OF EXIT CALLING                     
AUTOCHKO DS    CL1                   AUTO CHECKOUT FLAG                         
PROCESSL EQU   *-PROCESS           TABLE ENTRY LENGTH                           
         SPACE 3                                                                
         REQUS                                                                  
         PRINT NOGEN                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        M A I N   R O U T I N E                                                
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
APAS0200 CSECT                                                                  
         USING APAS0200,RC         ESTABLISH BASE ADDRESSABILITY                
         SAVE  (14,12),,APAS0200-&SYSDATE-&SYSTIME                              
         LR    RC,RF               LOAD BASE ADDRESS                            
         LA    R0,SAVEAREA         FETCH POINTER TO SAVEAREA                    
         ST    RD,SAVEAREA+4       SAVE LINKAGE TO CALLING PROGRAM              
         ST    R0,8(RD)            CHAIN SAVEAREAS                              
         LR    RD,R0               POINT TO CURRENT SAVEAREA                    
**       *----------------------------------------------------*                 
**       *  ESTABLISH ADDRESSABILITY TO PASSING PARAMETERS.   *                 
**       *----------------------------------------------------*                 
*                                                                               
         L     R2,0(,R1)           FETCH POINTER TO                             
** (PARM #1) -------------------->  USERS REQUEST BLOCK.                        
         USING APAM02XX,R2         ESTABLISH ADDRESSABILITY.                    
*                                                                               
         L     R7,4(,R1)           FETCH POINTER TO                             
** (PARM #2) -------------------->  INVENTORY RECORD.                           
         USING APAMDIB2,R7         ESTABLISH ADDRESSABILITY.                    
*                                                                               
         L     R6,8(,R1)           FETCH POINTER TO                             
** (PARM #3) ---------------------> LIBRARY CODE RECORD.                        
         USING APAMLIB2,R6         ESTABLISH ADDRESSABILITY.                    
*                                                                               
         L     R5,12(,R1)          FETCH POINTER TO                             
** (PARM #4) ---------------------> PENDING FILE RECORD.                        
         USING APAMMMBR,R5         ESTABLISH ADDRESSABILITY.                    
*                                                                               
         L     R3,16(,R1)          FETCH POINTER TO                             
** (PARM #5) -------------------->  MEMBER DESCRIPTION RECORD.                  
         TM    16(R1),X'80'        CHECK "VL" PARMLIST BIT.                     
         BO    @ASSERT             BIT OFF, ABEND.                              
         ABEND 2020,DUMP           *** ASSERT FAILURE ***                       
@ASSERT  DS    0H ---------------- PASSED ASSERT CHECK.                         
         USING APAMMDES,R3         ESTABLISH ADDRESSABILITY.                    
         SLL   R3,1                                                             
         SRL   R3,1                SHIFT OUT VL BIT.                            
*                                                                               
         MVI   M02XXMSG,C' '       CLEAR ERROR MESSAGE FIELD                    
         MVC   M02XXMSG+1(L'M02XXMSG-1),M02XXMSG                                
         SR    RF,RF               SET NORMAL RETURN CODE                       
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        DETERMINE IF PROCESSING IS NECESSARY                                   
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
         CLI   M02XXSRC,CLOSE      CLOSE PROCESSING?                            
         BE    MEMBRCHK            YES.. PROCESS MEMBER                         
*                                                                               
         LA    R4,TABLE            FETCH POINTER TO VERIFY TABLE                
         USING PROCESS,R4          ESTABLISH BASE ADDRESSABILITY                
         LA    R9,TABLECTP         FETCH NUMBER OF TABLE ENTRIES                
*                                                                               
CKPROCES DS    0H                                                               
         CLC   PROCESS,M02XXVER    PROCESS THIS MEMBER?                         
         BE    MEMBRCHK            YES.. CONTINUE                               
         LA    R4,PROCESSL(,R4)    POINT TO NEXT TABLE ENTRY                    
         BCT   R9,CKPROCES         GO... CHECK TABLE ENTRY                      
         DROP  R4                  DROP BASE ADDRESSABLITY                      
*                                                                               
         LHI   RF,8                SET NO PROCESSING RETURN CODE                
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBRCHK DS    0H                                                               
         CLC   M02XXACT,=CL8'MEMBER' VALID ACTION REQUESTED                     
         BE    MEMBRPRC            YES.. CONTINUE PROCESSING                    
         CLC   M02XXACT,=CL8'MEMPURGE' VALID ACTION REQUESTED                   
         BE    MEMBRPRC            YES.. CONTINUE PROCESSING                    
         MVC   PDACT01,M02XXACT    SET INVALID ACTION IN MESSAGE                
         MVC   M02XXMSG(PDERR01L),PDERR01 RETURN INVALID ACTION MSG             
         LHI   RF,12               SET USER ERROR RETURN CODE                   
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBRPRC DS    0H                                                               
         BAS   RE,USERRTN                                                       
*                                                                               
PROCRET  DS    0H                                                               
         LTR   RF,RF               MEMBER NOT FOUND IN LIBRARY                  
         BNZ   RETURN              YES.. RETURN TO CALLER                       
*                                                                               
RETURN   DS    0H                  EXIT MODULE                                  
         L     RD,4(,RD)           RESTORE LINKAGE TO CALLING PROGRAM           
         RETURN (14,12),RC=(15)    RETURN TO CALLING PROGRAM                    
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        U S E R   S U B R O U T I N E                                          
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
USERRC   DC    F'0'                                                             
USERRTN  DS    0H                                                               
*                                                                               
         ST    R8,USERRTNL         SAVE ROUTINE RETURN LINKAGE                  
         XC    USERRC,USERRC       POSIT GOOD RETURN                            
*                                                                               
         CLC   MRLIBC,=C'WARNBAK'  LIBCODE IS WARN/BAK?                         
         BNE   USERRTNX            NO                                           
         CLI   M02XXSRC,BACKOUT    BACKOUT PROCESSING?                          
         BNE   USERRTNX            NO                                           
*                                                                               
         MVC   M02XXMSG(MXERR01L),MXERR01   RETURN ERROR MESSAGE                
         MVC   USERRC,=F'4'        SET VERIFY FAIL RETURN CODE                  
         B     USERRTNX                                                         
*                                                                               
USERRTNX DS    0H                                                               
         L     RF,USERRC           RETURN CODE                                  
         L     R8,USERRTNL         RESTORE RETURN LINKAGE                       
         BR    R8                  EXIT                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        C O N S T A N T S   A N D   W O R K A R E A S                          
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
SAVEAREA DC    9D'0'               LOCAL SAVEAREA                               
*                                                                               
USERRTNL DC    F'0'                USER ROUTINE LINKAGE SAVEAREA                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        E R R O R   M E S S A G E S                                            
*                                                                               
*----------------------------------------------------------------------         
* NOTE: THESE MESSAGES WILL BE SENT TO THE ISPF LOG.                            
*       ISPF WILL TRUNCATE THEM TO 78 CHARACTERS.                               
*                                                                               
PDERR01  DS    0C                  INVALID ACTION==>  MEMBER ONLY!              
         DC    CL12'APAS0200-01'                                                
PDACT01  DC    CL08' '                                                          
         DC    CL22' IS AN INVALID ACTION.'                                     
PDERR01L EQU   *-PDERR01                                                        
*                                                                               
MXERR01  DS    0C                  WARN/BAK LIBCODE WARNING                     
         DC    C'THIS MR REQUIRES APPROVAL FOR BACKOUT. BE VERY SURE A +        
               BACKOUT IS APPROPRIATE!!!'                                       
MXERR01L EQU   *-MXERR01                                                        
*                                                                               
         EJECT                                                                  
         COPY  APAMXTBL                                                         
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDPAPTNMEX07/08/08'                                      
         END                                                                    
